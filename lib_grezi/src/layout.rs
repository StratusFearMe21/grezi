use std::fmt::Display;

use ahash::RandomState;
type AHashMap<K, V> = std::collections::HashMap<K, V, RandomState>;
use cassowary::strength::{REQUIRED, WEAK};
use cassowary::WeightedRelation::*;
use cassowary::{Constraint as CassowaryConstraint, Expression, Solver, Variable};

/// The direction in which the viewbox's boxes go
#[derive(Debug, Hash, Clone, Copy, PartialEq, Eq)]
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
    Percentage(f32),
    /// Allocate this portion of the box
    Ratio(f32, f32),
    /// Allocate exactly this amount of the box
    Length(f32),
    /// Allocate at most this much of the box
    Max(f32),
    /// Allocate at least this much of the box
    Min(f32),
}

impl Display for Constraint {
    #[inline]
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match *self {
            Constraint::Max(n) => write!(f, "{}+", n),
            Constraint::Min(n) => write!(f, "{}-", n),
            Constraint::Ratio(n, m) => write!(f, "{}:{}", n, m),
            Constraint::Length(n) => write!(f, "{}", n),
            Constraint::Percentage(n) => write!(f, "{}%", n),
        }
    }
}

impl Constraint {
    /// Apply a constraint directly on a given length
    #[inline]
    pub fn apply(&self, length: f32) -> f32 {
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
    pub vertical: f32,
    /// Pads the boxes on the left and right
    pub horizontal: f32,
}

/// A raw, unsolved viewbox. You must use the [`Layout.split()`] method to solve the viewbox.
#[derive(Debug, Clone, PartialEq)]
pub struct Layout<'a> {
    /// The direction in which the boxes inside of the viewbox should go.
    direction: &'a Direction,
    /// The margin between the boxes inside of a viewbox
    margin: Margin,
    /// Tells the solver how the boxes should be allocated inside of the viewbox
    constraints: &'a [Constraint],
    /// Whether the last chunk of the computed layout should be expanded to fill the available
    /// space.
    expand_to_fill: bool,
}

impl<'a> Default for Layout<'a> {
    #[inline]
    fn default() -> Layout<'a> {
        Layout {
            direction: &Direction::Vertical,
            margin: Margin {
                horizontal: 0.0,
                vertical: 0.0,
            },
            constraints: &[],
            expand_to_fill: true,
        }
    }
}

impl<'a> Layout<'a> {
    /// Sets the constraints for the unsolved viewbox
    #[inline]
    pub fn constraints(mut self, constraints: &'a [Constraint]) -> Layout<'a> {
        self.constraints = constraints;
        self
    }

    /// Sets the vertical and horizontal margins for the unsolved viewbox
    #[inline]
    pub fn margin(mut self, margin: f32) -> Layout<'a> {
        self.margin = Margin {
            horizontal: margin,
            vertical: margin,
        };
        self
    }

    /// Sets the horizontal margin of the unsolved viewbox
    #[inline]
    pub fn horizontal_margin(mut self, horizontal: f32) -> Layout<'a> {
        self.margin.horizontal = horizontal;
        self
    }

    /// Sets the vertical margin of the unsolved viewbox
    #[inline]
    pub fn vertical_margin(mut self, vertical: f32) -> Layout<'a> {
        self.margin.vertical = vertical;
        self
    }

    /// Sets the direction of the unsolved viewbox
    #[inline]
    pub fn direction(mut self, direction: &'a Direction) -> Layout<'a> {
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
    pub fn split(self, dest_area: Rect) -> Vec<Rect> {
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

        for (i, e) in elements.iter().enumerate() {
            vars.insert(e.left, (i, 0));
            vars.insert(e.top, (i, 1));
            vars.insert(e.right, (i, 2));
            vars.insert(e.bottom, (i, 3));
        }
        let mut ccs: Vec<CassowaryConstraint> =
            Vec::with_capacity(elements.len() * 4 + self.constraints.len() * 6);
        for elt in &elements {
            ccs.push(elt.left | GE(REQUIRED) | dest_area.left);
            ccs.push(elt.top | GE(REQUIRED) | dest_area.top);
            ccs.push(elt.right | LE(REQUIRED) | dest_area.right);
            ccs.push(elt.bottom | LE(REQUIRED) | dest_area.bottom);
        }
        if let Some(first) = elements.first() {
            ccs.push(match self.direction {
                Direction::Horizontal => first.left | EQ(REQUIRED) | dest_area.left,
                Direction::Vertical => first.top | EQ(REQUIRED) | dest_area.top,
            });
        }
        if self.expand_to_fill {
            if let Some(last) = elements.last() {
                ccs.push(match self.direction {
                    Direction::Horizontal => last.right | EQ(REQUIRED) | dest_area.right,
                    Direction::Vertical => last.bottom | EQ(REQUIRED) | dest_area.bottom,
                });
            }
        }
        match self.direction {
            Direction::Horizontal => {
                for pair in elements.windows(2) {
                    ccs.push((pair[0].left + pair[0].width()) | EQ(REQUIRED) | pair[1].left);
                }
                for (i, size) in self.constraints.iter().enumerate() {
                    ccs.push(elements[i].top | EQ(REQUIRED) | dest_area.top);
                    ccs.push(elements[i].height() | EQ(REQUIRED) | dest_area.height());
                    ccs.push(match *size {
                        Constraint::Length(v) => elements[i].width() | EQ(WEAK) | v,
                        Constraint::Percentage(v) => {
                            elements[i].width() | EQ(WEAK) | (v * dest_area.width() / 100.0)
                        }
                        Constraint::Ratio(n, d) => {
                            elements[i].width() | EQ(WEAK) | (dest_area.width() * n / d)
                        }
                        Constraint::Min(v) => elements[i].width() | GE(WEAK) | v,
                        Constraint::Max(v) => elements[i].width() | LE(WEAK) | v,
                    });
                }
            }
            Direction::Vertical => {
                for pair in elements.windows(2) {
                    ccs.push((pair[0].top + pair[0].height()) | EQ(REQUIRED) | pair[1].top);
                }
                for (i, size) in self.constraints.iter().enumerate() {
                    ccs.push(elements[i].left | EQ(REQUIRED) | dest_area.left);
                    ccs.push(elements[i].width() | EQ(REQUIRED) | dest_area.width());
                    ccs.push(match *size {
                        Constraint::Length(v) => elements[i].height() | EQ(WEAK) | v,
                        Constraint::Percentage(v) => {
                            elements[i].height() | EQ(WEAK) | (v * dest_area.height() / 100.0)
                        }
                        Constraint::Ratio(n, d) => {
                            elements[i].height() | EQ(WEAK) | (dest_area.height() * n / d)
                        }
                        Constraint::Min(v) => elements[i].height() | GE(WEAK) | v,
                        Constraint::Max(v) => elements[i].height() | LE(WEAK) | v,
                    });
                }
            }
        }
        solver.add_constraints(&ccs).unwrap();
        for &(var, value) in solver.fetch_changes() {
            let (index, attr) = vars[&var];
            let value = if value.is_sign_negative() {
                0.0
            } else {
                value as f32
            };
            match attr {
                0 => {
                    results[index].left = value;
                }
                1 => {
                    results[index].top = value;
                }
                2 => {
                    results[index].right = value;
                }
                3 => {
                    results[index].bottom = value;
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
    left: Variable,
    top: Variable,
    right: Variable,
    bottom: Variable,
}

impl Element {
    #[inline]
    fn new() -> Element {
        Element {
            left: Variable::new(),
            top: Variable::new(),
            right: Variable::new(),
            bottom: Variable::new(),
        }
    }

    #[inline]
    pub fn width(&self) -> Expression {
        self.right - self.left
    }

    #[inline]
    pub fn height(&self) -> Expression {
        self.bottom - self.top
    }
}

/// A simple rectangle used in the computation of the viewbox and to give objects a hint about the
/// area they are supposed to render to.
#[derive(Debug, Clone, Copy, PartialEq, Default)]
pub struct Rect {
    /// The x coordinate to the left of the [`Rect`]
    pub left: f32,
    /// The y coordinate on top of the [`Rect`]
    pub top: f32,
    /// The x coordinate to the right of the [`Rect`]
    pub right: f32,
    /// The y coordinate on the bottom of the [`Rect`]
    pub bottom: f32,
}

impl Rect {
    /// Creates a new rect, with width and height limited to keep the area under max u16.
    /// If clipped, aspect ratio will be preserved.
    pub fn new(left: f32, top: f32, width: f32, height: f32) -> Rect {
        let max_area = f32::MAX;
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
    pub fn area(self) -> f32 {
        self.width() * self.height()
    }

    /// Calculates the width of the [`Rect`]
    #[inline]
    pub fn width(self) -> f32 {
        self.right - self.left
    }

    /// Calculates the height of the [`Rect`]
    #[inline]
    pub fn height(self) -> f32 {
        self.bottom - self.top
    }

    /// Applies a margin to the [`Rect`]
    #[inline]
    pub fn inner(self, margin: &Margin) -> Rect {
        Rect {
            left: self.left + margin.horizontal,
            top: self.top + margin.vertical,
            right: self.right - margin.horizontal,
            bottom: self.bottom - margin.vertical,
        }
    }

    /// Computes the [`Rect`] which encapsulates itself and another [`Rect`]
    #[inline]
    pub fn union(self, other: Rect) -> Rect {
        Rect {
            left: f32::min(self.left, other.left),
            top: f32::min(self.top, other.top),
            right: f32::max(self.right, other.right),
            bottom: f32::max(self.bottom, other.bottom),
        }
    }

    /// Computes the [`Rect`] which intersects itself and another [`Rect`]
    #[inline]
    pub fn intersection(self, other: Rect) -> Rect {
        Rect {
            left: f32::max(self.left, other.left),
            top: f32::max(self.top, other.top),
            right: f32::min(self.right, other.right),
            bottom: f32::min(self.bottom, other.bottom),
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
            .direction(&Direction::Vertical)
            .constraints(&[
                Constraint::Percentage(10.0),
                Constraint::Max(5.0),
                Constraint::Min(1.0),
            ])
            .split(target);

        println!("{:?}", chunks);

        assert_eq!(
            target.height(),
            chunks.iter().map(|r| r.height()).sum::<f32>()
        );
        chunks
            .windows(2)
            .for_each(|w| assert!(w[0].top <= w[1].top));
    }

    #[test]
    fn test_rect_size_truncation() {
        for width in 256..300 {
            for height in 256..30 {
                let rect = Rect::new(0.0, 0.0, f32::from(width), f32::from(height));
                rect.area(); // Should not panic.
                assert!(rect.width() < f32::from(width) || rect.height() < f32::from(height));
                // The target dimensions are rounded down so the math will not be too precise
                // but let's make sure the ratios don't diverge crazily.
                assert!(
                    (rect.width() / rect.height() - f32::from(width) / f32::from(height)).abs()
                        < 1.0
                )
            }
        }

        // One dimension below 255, one above. Area above max u16.
        let width = f32::MAX;
        let height = f32::MAX;
        let rect = Rect::new(0.0, 0.0, f32::from(width), f32::from(height));
        assert_ne!(rect.width(), 900.0);
        assert_ne!(rect.height(), 100.0);
        assert!(rect.width() < f32::from(width) || rect.height() < f32::from(height));
    }

    #[test]
    fn test_rect_size_preservation() {
        for width in 0..256 {
            for height in 0..256 {
                let rect = Rect::new(0.0, 0.0, f32::from(width), f32::from(height));
                rect.area(); // Should not panic.
                assert_eq!(rect.width(), f32::from(width));
                assert_eq!(rect.height(), f32::from(height));
            }
        }

        // One dimension below 255, one above. Area below max u16.
        let rect = Rect::new(0.0, 0.0, 300.0, 100.0);
        assert_eq!(rect.width(), 300.0);
        assert_eq!(rect.height(), 100.0);
    }
}
