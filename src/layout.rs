use std::collections::HashMap;

use cassowary::strength::{REQUIRED, WEAK};
use cassowary::WeightedRelation::*;
use cassowary::{Constraint as CassowaryConstraint, Expression, Solver, Variable};

#[derive(Debug, Hash, Clone, PartialEq, Eq)]
pub enum Direction {
    Horizontal,
    Vertical,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Constraint {
    // TODO: enforce range 0 - 100
    Percentage(f32),
    Ratio(f32, f32),
    Length(f32),
    Max(f32),
    Min(f32),
}

impl Constraint {
    pub fn apply(&self, length: f32) -> f32 {
        match *self {
            Constraint::Percentage(p) => length * p / 100.0,
            Constraint::Ratio(num, den) => {
                let r = num * length / den;
                r
            }
            Constraint::Length(l) => length.min(l),
            Constraint::Max(m) => length.min(m),
            Constraint::Min(m) => length.max(m),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Margin {
    pub vertical: f32,
    pub horizontal: f32,
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

    pub fn margin(mut self, margin: f32) -> Layout {
        self.margin = Margin {
            horizontal: margin,
            vertical: margin,
        };
        self
    }

    pub fn horizontal_margin(mut self, horizontal: f32) -> Layout {
        self.margin.horizontal = horizontal;
        self
    }

    pub fn vertical_margin(mut self, vertical: f32) -> Layout {
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
    /// # use tui::layout::{Rect, Constraint, Direction, Layout};
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
        ccs.push(elt.left() | GE(REQUIRED) | f64::from(dest_area.left()));
        ccs.push(elt.top() | GE(REQUIRED) | f64::from(dest_area.top()));
        ccs.push(elt.right() | LE(REQUIRED) | f64::from(dest_area.right()));
        ccs.push(elt.bottom() | LE(REQUIRED) | f64::from(dest_area.bottom()));
    }
    if let Some(first) = elements.first() {
        ccs.push(match layout.direction {
            Direction::Horizontal => first.left() | EQ(REQUIRED) | f64::from(dest_area.left()),
            Direction::Vertical => first.top() | EQ(REQUIRED) | f64::from(dest_area.top()),
        });
    }
    if layout.expand_to_fill {
        if let Some(last) = elements.last() {
            ccs.push(match layout.direction {
                Direction::Horizontal => last.right() | EQ(REQUIRED) | f64::from(dest_area.right()),
                Direction::Vertical => last.bottom() | EQ(REQUIRED) | f64::from(dest_area.bottom()),
            });
        }
    }
    match layout.direction {
        Direction::Horizontal => {
            for pair in elements.windows(2) {
                ccs.push((pair[0].x + pair[0].width) | EQ(REQUIRED) | pair[1].x);
            }
            for (i, size) in layout.constraints.iter().enumerate() {
                ccs.push(elements[i].y | EQ(REQUIRED) | f64::from(dest_area.y));
                ccs.push(elements[i].height | EQ(REQUIRED) | f64::from(dest_area.height));
                ccs.push(match *size {
                    Constraint::Length(v) => elements[i].width | EQ(WEAK) | f64::from(v),
                    Constraint::Percentage(v) => {
                        elements[i].width | EQ(WEAK) | (f64::from(v * dest_area.width) / 100.0)
                    }
                    Constraint::Ratio(n, d) => {
                        elements[i].width
                            | EQ(WEAK)
                            | (f64::from(dest_area.width) * f64::from(n) / f64::from(d))
                    }
                    Constraint::Min(v) => elements[i].width | GE(WEAK) | f64::from(v),
                    Constraint::Max(v) => elements[i].width | LE(WEAK) | f64::from(v),
                });
            }
        }
        Direction::Vertical => {
            for pair in elements.windows(2) {
                ccs.push((pair[0].y + pair[0].height) | EQ(REQUIRED) | pair[1].y);
            }
            for (i, size) in layout.constraints.iter().enumerate() {
                ccs.push(elements[i].x | EQ(REQUIRED) | f64::from(dest_area.x));
                ccs.push(elements[i].width | EQ(REQUIRED) | f64::from(dest_area.width));
                ccs.push(match *size {
                    Constraint::Length(v) => elements[i].height | EQ(WEAK) | f64::from(v),
                    Constraint::Percentage(v) => {
                        elements[i].height | EQ(WEAK) | (f64::from(v * dest_area.height) / 100.0)
                    }
                    Constraint::Ratio(n, d) => {
                        elements[i].height
                            | EQ(WEAK)
                            | (f64::from(dest_area.height) * f64::from(n) / f64::from(d))
                    }
                    Constraint::Min(v) => elements[i].height | GE(WEAK) | f64::from(v),
                    Constraint::Max(v) => elements[i].height | LE(WEAK) | f64::from(v),
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
                results[index].x = value;
            }
            1 => {
                results[index].y = value;
            }
            2 => {
                results[index].width = value;
            }
            3 => {
                results[index].height = value;
            }
            _ => {}
        }
    }

    if layout.expand_to_fill {
        // Fix imprecision by extending the last item a bit if necessary
        if let Some(last) = results.last_mut() {
            match layout.direction {
                Direction::Vertical => {
                    last.height = dest_area.bottom() - last.y;
                }
                Direction::Horizontal => {
                    last.width = dest_area.right() - last.x;
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
    pub x: f32,
    pub y: f32,
    pub width: f32,
    pub height: f32,
}

impl Rect {
    /// Creates a new rect, with width and height limited to keep the area under max u16.
    /// If clipped, aspect ratio will be preserved.
    pub fn new(x: f32, y: f32, width: f32, height: f32) -> Rect {
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
            x,
            y,
            width: clipped_width,
            height: clipped_height,
        }
    }

    pub fn area(self) -> f32 {
        self.width * self.height
    }

    pub fn left(self) -> f32 {
        self.x
    }

    pub fn right(self) -> f32 {
        self.x + self.width
    }

    pub fn top(self) -> f32 {
        self.y
    }

    pub fn bottom(self) -> f32 {
        self.y + self.height
    }

    pub fn inner(self, margin: &Margin) -> Rect {
        if self.width < 2.0 * margin.horizontal || self.height < 2.0 * margin.vertical {
            Rect::default()
        } else {
            Rect {
                x: self.x + margin.horizontal,
                y: self.y + margin.vertical,
                width: self.width - 2.0 * margin.horizontal,
                height: self.height - 2.0 * margin.vertical,
            }
        }
    }

    pub fn union(self, other: Rect) -> Rect {
        let x1 = f32::min(self.x, other.x);
        let y1 = f32::min(self.y, other.y);
        let x2 = f32::max(self.x + self.width, other.x + other.width);
        let y2 = f32::max(self.y + self.height, other.y + other.height);
        Rect {
            x: x1,
            y: y1,
            width: x2 - x1,
            height: y2 - y1,
        }
    }

    pub fn intersection(self, other: Rect) -> Rect {
        let x1 = f32::max(self.x, other.x);
        let y1 = f32::max(self.y, other.y);
        let x2 = f32::min(self.x + self.width, other.x + other.width);
        let y2 = f32::min(self.y + self.height, other.y + other.height);
        Rect {
            x: x1,
            y: y1,
            width: x2 - x1,
            height: y2 - y1,
        }
    }

    pub fn intersects(self, other: Rect) -> bool {
        self.x < other.x + other.width
            && self.x + self.width > other.x
            && self.y < other.y + other.height
            && self.y + self.height > other.y
    }
}
