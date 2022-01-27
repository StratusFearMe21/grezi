// Copyright 2016 John Ward under MIT

macro_rules! easer {
    ($ty:ty, $f:ident, $t:ident, $e:expr) => {
        /// Easing struct for $t
        pub struct $t {
            start: $ty,
            dist: $ty,
            step: f64,
            steps: f64,
        }

        /// Easing function for $f returning and iterator
        pub fn $f(start: $ty, end: $ty, steps: f64) -> $t {
            $t {
                start: start,
                dist: end - start,
                step: 0.0,
                steps: steps,
            }
        }

        impl Iterator for $t {
            type Item = $ty;

            fn next(&mut self) -> Option<Self::Item> {
                self.step += 1.0;
                if self.step.gt(&self.steps) {
                    None
                } else {
                    let e = $e(self.step / self.steps) * self.dist;
                    Some(self.start + e)
                }
            }
        }

        impl $t {
            /// Gets the step of the $t
            pub fn get_step(&mut self, step: f64) -> Option<$ty> {
                self.step += step;
                if self.step.gt(&self.steps) {
                    None
                } else {
                    let e = $e(self.step / self.steps) * self.dist;
                    Some(self.start + e)
                }
            }
        }
    };
}

easer!(
    vek::Vec4<f64>,
    cubic_rect_inout,
    CubicPointInOut,
    |x: f64| {
        if x < 0.5 {
            4. * (x * x * x)
        } else {
            let y = x.mul_add(2., -2.);
            (y * y * y).mul_add(0.5, 1.0)
        }
    }
);

easer!(f64, cubic_single_inout, CubicSingleInOut, |x: f64| {
    if x < 0.5 {
        4. * (x * x * x)
    } else {
        let y = x.mul_add(2., -2.);
        (y * y * y).mul_add(0.5, 1.0)
    }
});
