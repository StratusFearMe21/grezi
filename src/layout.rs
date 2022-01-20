use std::collections::HashMap;

use cassowary::strength::{REQUIRED, WEAK};
use cassowary::WeightedRelation::*;
use cassowary::{Constraint as CassowaryConstraint, Expression, Solver, Variable};

#[derive(Debug, Hash, Clone, Copy, PartialEq, Eq)]
pub enum Corner {
    TopLeft,
    TopRight,
    BottomRight,
    BottomLeft,
}

#[derive(Debug, Hash, Clone, PartialEq, Eq)]
pub enum Direction {
    Horizontal,
    Vertical,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Constraint {
    // TODO: enforce range 0 - 100
    Percentage(f64),
    Ratio(f64, f64),
    Length(f64),
    Max(f64),
    Min(f64),
}

impl Constraint {
    pub fn apply(&self, length: f64) -> f64 {
        match *self {
            Constraint::Percentage(p) => length * p / 100.0,
            Constraint::Ratio(num, den) => num * length / den,
            Constraint::Length(l) => length.min(l),
            Constraint::Max(m) => length.min(m),
            Constraint::Min(m) => length.max(m),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Margin {
    pub vertical: f64,
    pub horizontal: f64,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Alignment {
    Left,
    Center,
    Right,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Layout {
    direction: Direction,
    margin: Margin,
    constraints: Vec<Constraint>,
    /// Whether the last chunk of the computed layout should be expanded to fill the available
    /// space.
    expand_to_fill: bool,
}

impl Default for Layout {
    fn default() -> Layout {
        Layout {
            direction: Direction::Vertical,
            margin: Margin {
                horizontal: 0.0,
                vertical: 0.0,
            },
            constraints: Vec::new(),
            expand_to_fill: true,
        }
    }
}

impl Layout {
    pub fn constraints<C>(mut self, constraints: C) -> Layout
    where
        C: Into<Vec<Constraint>>,
    {
        self.constraints = constraints.into();
        self
    }

    pub fn margin(mut self, margin: f64) -> Layout {
        self.margin = Margin {
            horizontal: margin,
            vertical: margin,
        };
        self
    }

    pub fn horizontal_margin(mut self, horizontal: f64) -> Layout {
        self.margin.horizontal = horizontal;
        self
    }

    pub fn vertical_margin(mut self, vertical: f64) -> Layout {
        self.margin.vertical = vertical;
        self
    }

    pub fn direction(mut self, direction: Direction) -> Layout {
        self.direction = direction;
        self
    }

    /// Wrapper function around the cassowary-rs solver to be able to split a given
    /// area into smaller ones based on the preferred widths or heights and the direction.
    ///
    /// # Examples
    /// ```
    /// # use grezi::layout::{Rect, Constraint, Direction, Layout};
    /// let chunks = Layout::default()
    ///     .direction(Direction::Vertical)
    ///     .constraints([Constraint::Length(5), Constraint::Min(0)].as_ref())
    ///     .split(Rect {
    ///         x: 2,
    ///         y: 2,
    ///         width: 10,
    ///         height: 10,
    ///     });
    /// assert_eq!(
    ///     chunks,
    ///     vec![
    ///         Rect {
    ///             x: 2,
    ///             y: 2,
    ///             width: 10,
    ///             height: 5
    ///         },
    ///         Rect {
    ///             x: 2,
    ///             y: 7,
    ///             width: 10,
    ///             height: 5
    ///         }
    ///     ]
    /// );
    ///
    /// let chunks = Layout::default()
    ///     .direction(Direction::Horizontal)
    ///     .constraints([Constraint::Ratio(1, 3), Constraint::Ratio(2, 3)].as_ref())
    ///     .split(Rect {
    ///         x: 0,
    ///         y: 0,
    ///         width: 9,
    ///         height: 2,
    ///     });
    /// assert_eq!(
    ///     chunks,
    ///     vec![
    ///         Rect {
    ///             x: 0,
    ///             y: 0,
    ///             width: 3,
    ///             height: 2
    ///         },
    ///         Rect {
    ///             x: 3,
    ///             y: 0,
    ///             width: 6,
    ///             height: 2
    ///         }
    ///     ]
    /// );
    /// ```
    pub fn split(&self, area: Rect) -> Vec<Rect> {
        // TODO: Maybe use a fixed size cache ?
        split(area, self)
    }
}

fn split(area: Rect, layout: &Layout) -> Vec<Rect> {
    let mut solver = Solver::new();
    let mut vars: HashMap<Variable, (usize, usize)> = HashMap::new();
    let elements = layout
        .constraints
        .iter()
        .map(|_| Element::new())
        .collect::<Vec<Element>>();
    let mut results = layout
        .constraints
        .iter()
        .map(|_| Rect::default())
        .collect::<Vec<Rect>>();

    let dest_area = area.inner(&layout.margin);
    for (i, e) in elements.iter().enumerate() {
        vars.insert(e.x, (i, 0));
        vars.insert(e.y, (i, 1));
        vars.insert(e.width, (i, 2));
        vars.insert(e.height, (i, 3));
    }
    let mut ccs: Vec<CassowaryConstraint> =
        Vec::with_capacity(elements.len() * 4 + layout.constraints.len() * 6);
    for elt in &elements {
        ccs.push(elt.width | GE(REQUIRED) | 0f64);
        ccs.push(elt.height | GE(REQUIRED) | 0f64);
        ccs.push(elt.left() | GE(REQUIRED) | dest_area.left);
        ccs.push(elt.top() | GE(REQUIRED) | dest_area.top);
        ccs.push(elt.right() | LE(REQUIRED) | dest_area.right);
        ccs.push(elt.bottom() | LE(REQUIRED) | dest_area.bottom);
    }
    if let Some(first) = elements.first() {
        ccs.push(match layout.direction {
            Direction::Horizontal => first.left() | EQ(REQUIRED) | dest_area.left,
            Direction::Vertical => first.top() | EQ(REQUIRED) | dest_area.top,
        });
    }
    if layout.expand_to_fill {
        if let Some(last) = elements.last() {
            ccs.push(match layout.direction {
                Direction::Horizontal => last.right() | EQ(REQUIRED) | dest_area.right,
                Direction::Vertical => last.bottom() | EQ(REQUIRED) | dest_area.bottom,
            });
        }
    }
    match layout.direction {
        Direction::Horizontal => {
            for pair in elements.windows(2) {
                ccs.push((pair[0].x + pair[0].width) | EQ(REQUIRED) | pair[1].x);
            }
            for (i, size) in layout.constraints.iter().enumerate() {
                ccs.push(elements[i].y | EQ(REQUIRED) | dest_area.top);
                ccs.push(elements[i].height | EQ(REQUIRED) | dest_area.height());
                ccs.push(match *size {
                    Constraint::Length(v) => elements[i].width | EQ(WEAK) | v,
                    Constraint::Percentage(v) => {
                        elements[i].width | EQ(WEAK) | (v * dest_area.width() / 100.0)
                    }
                    Constraint::Ratio(n, d) => {
                        elements[i].width | EQ(WEAK) | (dest_area.width() * n / d)
                    }
                    Constraint::Min(v) => elements[i].width | GE(WEAK) | v,
                    Constraint::Max(v) => elements[i].width | LE(WEAK) | v,
                });
            }
        }
        Direction::Vertical => {
            for pair in elements.windows(2) {
                ccs.push((pair[0].y + pair[0].height) | EQ(REQUIRED) | pair[1].y);
            }
            for (i, size) in layout.constraints.iter().enumerate() {
                ccs.push(elements[i].x | EQ(REQUIRED) | dest_area.left);
                ccs.push(elements[i].width | EQ(REQUIRED) | dest_area.width());
                ccs.push(match *size {
                    Constraint::Length(v) => elements[i].height | EQ(WEAK) | v,
                    Constraint::Percentage(v) => {
                        elements[i].height | EQ(WEAK) | (v * dest_area.height() / 100.0)
                    }
                    Constraint::Ratio(n, d) => {
                        elements[i].height | EQ(WEAK) | (dest_area.height() * n / d)
                    }
                    Constraint::Min(v) => elements[i].height | GE(WEAK) | v,
                    Constraint::Max(v) => elements[i].height | LE(WEAK) | v,
                });
            }
        }
    }
    solver.add_constraints(&ccs).unwrap();
    for &(var, value) in solver.fetch_changes() {
        let (index, attr) = vars[&var];
        let value = if value.is_sign_negative() { 0.0 } else { value };
        match attr {
            0 => {
                let width = results[index].width();
                results[index].left = value;
                results[index].right = value + width;
            }
            1 => {
                let height = results[index].height();
                results[index].top = value;
                results[index].bottom = value + height;
            }
            2 => {
                results[index].right = results[index].left + value;
            }
            3 => {
                results[index].bottom = results[index].top + value;
            }
            _ => {}
        }
    }

    if layout.expand_to_fill {
        // Fix imprecision by extending the last item a bit if necessary
        if let Some(last) = results.last_mut() {
            match layout.direction {
                Direction::Vertical => {
                    last.bottom = dest_area.bottom;
                }
                Direction::Horizontal => {
                    last.right = dest_area.right;
                }
            }
        }
    }
    results
}

/// A container used by the solver inside split
struct Element {
    x: Variable,
    y: Variable,
    width: Variable,
    height: Variable,
}

impl Element {
    fn new() -> Element {
        Element {
            x: Variable::new(),
            y: Variable::new(),
            width: Variable::new(),
            height: Variable::new(),
        }
    }

    fn left(&self) -> Variable {
        self.x
    }

    fn top(&self) -> Variable {
        self.y
    }

    fn right(&self) -> Expression {
        self.x + self.width
    }

    fn bottom(&self) -> Expression {
        self.y + self.height
    }
}

/// A simple rectangle used in the computation of the layout and to give widgets an hint about the
/// area they are supposed to render to.
#[derive(Debug, Clone, Copy, PartialEq, Default)]
pub struct Rect {
    pub left: f64,
    pub top: f64,
    pub right: f64,
    pub bottom: f64,
}

impl Rect {
    /// Creates a new rect, with width and height limited to keep the area under max u16.
    /// If clipped, aspect ratio will be preserved.
    pub fn new(left: f64, top: f64, width: f64, height: f64) -> Rect {
        let max_area = f64::MAX;
        let (clipped_width, clipped_height) = if width * height > max_area {
            let aspect_ratio = width / height;
            let max_area_f = max_area;
            let height_f = (max_area_f / aspect_ratio).sqrt();
            let width_f = height_f * aspect_ratio;
            (width_f, height_f)
        } else {
            (width, height)
        };
        Rect {
            left,
            top,
            right: left + clipped_width,
            bottom: top + clipped_height,
        }
    }

    pub fn area(self) -> f64 {
        self.width() * self.height()
    }

    pub fn width(self) -> f64 {
        self.right - self.left
    }

    pub fn height(self) -> f64 {
        self.bottom - self.top
    }

    pub fn inner(self, margin: &Margin) -> Rect {
        if self.width() < 2.0 * margin.horizontal || self.height() < 2.0 * margin.vertical {
            Rect::default()
        } else {
            Rect {
                left: self.left + margin.horizontal,
                top: self.top + margin.vertical,
                right: self.right - margin.horizontal,
                bottom: self.bottom - margin.vertical,
            }
        }
    }

    pub fn union(self, other: Rect) -> Rect {
        let x1 = f64::min(self.left, other.left);
        let y1 = f64::min(self.top, other.top);
        let x2 = f64::max(self.right, other.right);
        let y2 = f64::max(self.bottom, other.bottom);
        Rect {
            left: x1,
            top: y1,
            right: x2,
            bottom: y2,
        }
    }

    pub fn intersection(self, other: Rect) -> Rect {
        let x1 = f64::max(self.left, other.left);
        let y1 = f64::max(self.top, other.top);
        let x2 = f64::min(self.right, other.right);
        let y2 = f64::min(self.bottom, other.bottom);
        Rect {
            left: x1,
            top: y1,
            right: x2,
            bottom: y2,
        }
    }

    pub fn intersects(self, other: Rect) -> bool {
        self.left < other.right
            && self.right > other.left
            && self.top < other.bottom
            && self.bottom > other.top
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_vertical_split_by_height() {
        let target = Rect {
            left: 2.0,
            top: 2.0,
            right: 12.0,
            bottom: 12.0,
        };

        let chunks = Layout::default()
            .direction(Direction::Vertical)
            .constraints(
                [
                    Constraint::Percentage(10.0),
                    Constraint::Max(5.0),
                    Constraint::Min(1.0),
                ]
                .as_ref(),
            )
            .split(target);

        println!("{:?}", chunks);

        assert_eq!(
            target.height(),
            chunks.iter().map(|r| r.height()).sum::<f64>()
        );
        chunks
            .windows(2)
            .for_each(|w| assert!(w[0].top <= w[1].top));
    }

    #[test]
    fn test_rect_size_truncation() {
        for width in 256..300 {
            for height in 256..30 {
                let rect = Rect::new(0.0, 0.0, f64::from(width), f64::from(height));
                rect.area(); // Should not panic.
                assert!(rect.width() < f64::from(width) || rect.height() < f64::from(height));
                // The target dimensions are rounded down so the math will not be too precise
                // but let's make sure the ratios don't diverge crazily.
                assert!(
                    (rect.width() / rect.height() - f64::from(width) / f64::from(height)).abs()
                        < 1.0
                )
            }
        }

        // One dimension below 255, one above. Area above max u16.
        let width = f64::MAX;
        let height = f64::MAX;
        let rect = Rect::new(0.0, 0.0, f64::from(width), f64::from(height));
        assert_ne!(rect.width(), 900.0);
        assert_ne!(rect.height(), 100.0);
        assert!(rect.width() < f64::from(width) || rect.height() < f64::from(height));
    }

    #[test]
    fn test_rect_size_preservation() {
        for width in 0..256 {
            for height in 0..256 {
                let rect = Rect::new(0.0, 0.0, f64::from(width), f64::from(height));
                rect.area(); // Should not panic.
                assert_eq!(rect.width(), f64::from(width));
                assert_eq!(rect.height(), f64::from(height));
            }
        }

        // One dimension below 255, one above. Area below max u16.
        let rect = Rect::new(0.0, 0.0, 300.0, 100.0);
        assert_eq!(rect.width(), 300.0);
        assert_eq!(rect.height(), 100.0);
    }
}
