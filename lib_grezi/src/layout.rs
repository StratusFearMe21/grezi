use ahash::RandomState;
type AHashMap<K, V> = std::collections::HashMap<K, V, RandomState>;
use cassowary::strength::{REQUIRED, WEAK};
use cassowary::WeightedRelation::*;
use cassowary::{Constraint as CassowaryConstraint, Expression, Solver, Variable};

/// The direction in which the viewbox's boxes go
#[derive(Debug, Hash, Clone, PartialEq, Eq)]
pub enum Direction {
    /// Left to right
    Horizontal,
    /// Top to bottom
    Vertical,
}

/// A Constraint decides how viewboxes are split. In the `.grz` format, the available constraints
/// are
/// - `1:2`: [`Constraint::Ratio`]
/// - `50%`: [`Constraint::Percentage`]
/// - `50+`: [`Constraint::Max`]
/// - `50-`: [`Constraint::Min`]
/// - `50`: [`Constraint::Length`]
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Constraint {
    /// Allocate this percentage of the box
    Percentage(f64),
    /// Allocate this portion of the box
    Ratio(f64, f64),
    /// Allocate exactly this amount of the box
    Length(f64),
    /// Allocate at most this much of the box
    Max(f64),
    /// Allocate at least this much of the box
    Min(f64),
}

impl Constraint {
    /// Apply a constraint directly on a given length
    #[inline]
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

/// This is the space between boxes inside of a viewbox
#[derive(Debug, Clone, PartialEq)]
pub struct Margin {
    /// Pads the boxes on the top and bottom
    pub vertical: f64,
    /// Pads the boxes on the left and right
    pub horizontal: f64,
}

/// A raw, unsolved viewbox. You must use the [`Layout.split()`] method to solve the viewbox.
#[derive(Debug, Clone, PartialEq)]
pub struct Layout {
    /// The direction in which the boxes inside of the viewbox should go.
    direction: Direction,
    /// The margin between the boxes inside of a viewbox
    margin: Margin,
    /// Tells the solver how the boxes should be allocated inside of the viewbox
    constraints: Vec<Constraint>,
    /// Whether the last chunk of the computed layout should be expanded to fill the available
    /// space.
    expand_to_fill: bool,
}

impl Default for Layout {
    #[inline]
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
    /// Sets the constraints for the unsolved viewbox
    #[inline]
    pub fn constraints<C>(mut self, constraints: C) -> Layout
    where
        C: Into<Vec<Constraint>>,
    {
        self.constraints = constraints.into();
        self
    }

    /// Sets the vertical and horizontal margins for the unsolved viewbox
    #[inline]
    pub fn margin(mut self, margin: f64) -> Layout {
        self.margin = Margin {
            horizontal: margin,
            vertical: margin,
        };
        self
    }

    /// Sets the horizontal margin of the unsolved viewbox
    #[inline]
    pub fn horizontal_margin(mut self, horizontal: f64) -> Layout {
        self.margin.horizontal = horizontal;
        self
    }

    /// Sets the vertical margin of the unsolved viewbox
    #[inline]
    pub fn vertical_margin(mut self, vertical: f64) -> Layout {
        self.margin.vertical = vertical;
        self
    }

    /// Sets the direction of the unsolved viewbox
    #[inline]
    pub fn direction(mut self, direction: Direction) -> Layout {
        self.direction = direction;
        self
    }

    /// Wrapper function around the cassowary-rs solver to be able to split a given
    /// viewbox into smaller ones based on the constraints and the direction.
    ///
    /// # Examples
    /// ```
    /// # use grezi::layout::{Rect, Constraint, Direction, Layout};
    /// let chunks = Layout::default()
    ///     .direction(Direction::Vertical)
    ///     .constraints([Constraint::Length(5.0), Constraint::Min(0.0)].as_ref())
    ///     .split(Rect {
    ///         left: 2.0,
    ///         top: 2.0,
    ///         right: 12.0,
    ///         bottom: 12.0,
    ///     });
    /// assert_eq!(
    ///     chunks,
    ///     vec![
    ///         Rect {
    ///             left: 2.0,
    ///             top: 2.0,
    ///             right: 12.0,
    ///             bottom: 7.0
    ///         },
    ///         Rect {
    ///             left: 2.0,
    ///             top: 7.0,
    ///             right: 12.0,
    ///             bottom: 12.0
    ///         }
    ///     ]
    /// );
    ///
    /// let chunks = Layout::default()
    ///     .direction(Direction::Horizontal)
    ///     .constraints([Constraint::Ratio(1.0, 3.0), Constraint::Ratio(2.0, 3.0)].as_ref())
    ///     .split(Rect {
    ///         left: 0.0,
    ///         top: 0.0,
    ///         right: 9.0,
    ///         bottom: 2.0,
    ///     });
    /// assert_eq!(
    ///     chunks,
    ///     vec![
    ///         Rect {
    ///             left: 0.0,
    ///             top: 0.0,
    ///             right: 3.0,
    ///             bottom: 2.0
    ///         },
    ///         Rect {
    ///             left: 3.0,
    ///             top: 0.0,
    ///             right: 9.0,
    ///             bottom: 2.0
    ///         }
    ///     ]
    /// );
    /// ```
    pub fn split(&self, area: Rect) -> Vec<Rect> {
        let mut solver = Solver::new();
        let mut vars: AHashMap<Variable, (usize, usize)> = AHashMap::default();
        let elements = self
            .constraints
            .iter()
            .map(|_| Element::new())
            .collect::<Vec<Element>>();
        let mut results = self
            .constraints
            .iter()
            .map(|_| Rect::default())
            .collect::<Vec<Rect>>();

        let dest_area = area.inner(&self.margin);
        for (i, e) in elements.iter().enumerate() {
            vars.insert(e.x, (i, 0));
            vars.insert(e.y, (i, 1));
            vars.insert(e.width, (i, 2));
            vars.insert(e.height, (i, 3));
        }
        let mut ccs: Vec<CassowaryConstraint> =
            Vec::with_capacity(elements.len() * 4 + self.constraints.len() * 6);
        for elt in &elements {
            ccs.push(elt.width | GE(REQUIRED) | 0f64);
            ccs.push(elt.height | GE(REQUIRED) | 0f64);
            ccs.push(elt.left() | GE(REQUIRED) | dest_area.left);
            ccs.push(elt.top() | GE(REQUIRED) | dest_area.top);
            ccs.push(elt.right() | LE(REQUIRED) | dest_area.right);
            ccs.push(elt.bottom() | LE(REQUIRED) | dest_area.bottom);
        }
        if let Some(first) = elements.first() {
            ccs.push(match self.direction {
                Direction::Horizontal => first.left() | EQ(REQUIRED) | dest_area.left,
                Direction::Vertical => first.top() | EQ(REQUIRED) | dest_area.top,
            });
        }
        if self.expand_to_fill {
            if let Some(last) = elements.last() {
                ccs.push(match self.direction {
                    Direction::Horizontal => last.right() | EQ(REQUIRED) | dest_area.right,
                    Direction::Vertical => last.bottom() | EQ(REQUIRED) | dest_area.bottom,
                });
            }
        }
        match self.direction {
            Direction::Horizontal => {
                for pair in elements.windows(2) {
                    ccs.push((pair[0].x + pair[0].width) | EQ(REQUIRED) | pair[1].x);
                }
                for (i, size) in self.constraints.iter().enumerate() {
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
                for (i, size) in self.constraints.iter().enumerate() {
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

        if self.expand_to_fill {
            // Fix imprecision by extending the last item a bit if necessary
            if let Some(last) = results.last_mut() {
                match self.direction {
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
}

/// A container used by the solver inside split
struct Element {
    x: Variable,
    y: Variable,
    width: Variable,
    height: Variable,
}

impl Element {
    #[inline]
    fn new() -> Element {
        Element {
            x: Variable::new(),
            y: Variable::new(),
            width: Variable::new(),
            height: Variable::new(),
        }
    }

    #[inline]
    fn left(&self) -> Variable {
        self.x
    }

    #[inline]
    fn top(&self) -> Variable {
        self.y
    }

    #[inline]
    fn right(&self) -> Expression {
        self.x + self.width
    }

    #[inline]
    fn bottom(&self) -> Expression {
        self.y + self.height
    }
}

/// A simple rectangle used in the computation of the viewbox and to give objects a hint about the
/// area they are supposed to render to.
#[derive(Debug, Clone, Copy, PartialEq, Default)]
pub struct Rect {
    /// The x coordinate to the left of the [`Rect`]
    pub left: f64,
    /// The y coordinate on top of the [`Rect`]
    pub top: f64,
    /// The x coordinate to the right of the [`Rect`]
    pub right: f64,
    /// The y coordinate on the bottom of the [`Rect`]
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

    /// Calculates the total area of the [`Rect`] (width * height)
    #[inline]
    pub fn area(self) -> f64 {
        self.width() * self.height()
    }

    /// Calculates the width of the [`Rect`]
    #[inline]
    pub fn width(self) -> f64 {
        self.right - self.left
    }

    /// Calculates the height of the [`Rect`]
    #[inline]
    pub fn height(self) -> f64 {
        self.bottom - self.top
    }

    /// Applies a margin to the [`Rect`]
    #[inline]
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

    /// Computes the [`Rect`] which encapsulates itself and another [`Rect`]
    #[inline]
    pub fn union(self, other: Rect) -> Rect {
        Rect {
            left: f64::min(self.left, other.left),
            top: f64::min(self.top, other.top),
            right: f64::max(self.right, other.right),
            bottom: f64::max(self.bottom, other.bottom),
        }
    }

    /// Computes the [`Rect`] which intersects itself and another [`Rect`]
    #[inline]
    pub fn intersection(self, other: Rect) -> Rect {
        Rect {
            left: f64::max(self.left, other.left),
            top: f64::max(self.top, other.top),
            right: f64::min(self.right, other.right),
            bottom: f64::min(self.bottom, other.bottom),
        }
    }

    /// Computes whether a [`Rect`] intersects another [`Rect`]
    #[inline]
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
