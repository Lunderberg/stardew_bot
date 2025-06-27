use dotnet_debugger::RustNativeObject;

use crate::Direction;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Vector<T> {
    pub right: T,
    pub down: T,
}
impl<T: 'static> RustNativeObject for Vector<T> {}

#[derive(Debug, Clone, Copy)]
pub struct Rectangle<T> {
    pub top_left: Vector<T>,
    pub shape: Vector<T>,
}
impl<T: 'static> RustNativeObject for Rectangle<T> {}

impl<T> Vector<T> {
    pub fn zero() -> Self
    where
        T: num::Zero,
    {
        Self {
            right: T::zero(),
            down: T::zero(),
        }
    }

    pub const fn new(right: T, down: T) -> Self {
        Self { right, down }
    }

    pub fn dot(self, other: Self) -> T
    where
        T: num::Num,
    {
        self.right * other.right + self.down * other.down
    }

    pub fn dist(self, other: Self) -> T
    where
        T: num::Float,
    {
        self.dist2(other).sqrt()
    }

    pub fn dist2(self, other: Self) -> T
    where
        T: num::Num + Copy,
    {
        (self - other).mag2()
    }

    pub fn mag2(self) -> T
    where
        T: num::Num + Copy,
    {
        self.right * self.right + self.down * self.down
    }

    pub fn mag(self) -> T
    where
        T: num::Float,
    {
        self.mag2().sqrt()
    }

    pub fn manhattan_dist(self, other: Self) -> T
    where
        T: PartialOrd,
        T: std::ops::Sub<Output = T>,
    {
        let diff_x = if self.right < other.right {
            other.right - self.right
        } else {
            self.right - other.right
        };
        let diff_y = if self.down < other.down {
            other.down - self.down
        } else {
            self.down - other.down
        };

        if diff_x < diff_y {
            diff_y
        } else {
            diff_x
        }
    }

    pub fn map<Func, U>(self, func: Func) -> Vector<U>
    where
        Func: Fn(T) -> U,
    {
        Vector {
            right: func(self.right),
            down: func(self.down),
        }
    }

    pub fn as_type<U>(self) -> Vector<U>
    where
        T: Into<U>,
    {
        self.map(Into::into)
    }
}

impl Vector<f32> {
    pub fn as_tile(self) -> Vector<isize> {
        self.map(|x| x.round() as isize)
    }

    pub fn closest_direction(self) -> Direction {
        Direction::iter()
            .max_by(|dir_a, dir_b| {
                let do_dot_product = |dir: Direction| {
                    let offset = dir.offset().map(|i| i as f32);
                    let offset = offset / offset.mag();
                    offset.dot(self)
                };
                let dot_a = do_dot_product(*dir_a);
                let dot_b = do_dot_product(*dir_b);
                num::traits::float::TotalOrder::total_cmp(&dot_a, &dot_b)
            })
            .expect("Direction::iter is non-empty")
    }
}

impl Vector<isize> {
    /// Iterates over all neighboring locations, including diagonals,
    /// and including the current tile.
    pub fn iter_nearby(self) -> impl Iterator<Item = Self> {
        std::iter::once(self).chain(self.iter_adjacent())
    }

    /// Iterates over all neighboring locations, including diagonals,
    /// and excluding the current tile.
    pub fn iter_adjacent(self) -> impl Iterator<Item = Self> {
        Direction::iter().map(move |dir| self + dir.offset())
    }

    /// Iterates over all neighboring locations, excluding diagonals,
    /// and excluding the current tile.
    pub fn iter_cardinal(self) -> impl Iterator<Item = Self> {
        Direction::iter()
            .filter(|dir| dir.is_cardinal())
            .map(move |dir| self + dir.offset())
    }

    pub fn closest_direction(self) -> Direction {
        Direction::iter()
            .max_by_key(|dir| self.dot(dir.offset()))
            .expect("Direction::iter is non-empty")
    }
}

impl<T> Rectangle<T> {
    pub fn new(top_left: Vector<T>, shape: Vector<T>) -> Self {
        Self { top_left, shape }
    }

    pub fn iter_points(self) -> impl Iterator<Item = Vector<T>>
    where
        T: 'static,
        T: num::PrimInt,
    {
        num::range_step(T::zero(), self.shape.right, T::one())
            .flat_map(move |i| {
                num::range_step(T::zero(), self.shape.down, T::one())
                    .map(move |j| Vector::new(i, j))
            })
            .map(move |p| p + self.top_left)
    }

    pub fn top_right(&self) -> Vector<T>
    where
        T: Copy,
        T: num::Zero,
    {
        self.top_left + Vector::new(self.shape.right, T::zero())
    }

    pub fn width(&self) -> T
    where
        T: Copy,
    {
        self.shape.right
    }

    pub fn height(&self) -> T
    where
        T: Copy,
    {
        self.shape.down
    }

    pub fn center(self) -> Vector<T>
    where
        T: Copy,
        T: num::One,
        T: std::ops::Add<Output = T>,
        T: std::ops::Div<Output = T>,
    {
        let two = T::one() + T::one();
        self.top_left + self.shape / two
    }

    pub fn contains(&self, pos: Vector<T>) -> bool
    where
        T: Copy,
        T: std::ops::Add<Output = T>,
        T: PartialOrd,
    {
        let bottom_right = self.top_left + self.shape;
        let x_range = self.top_left.right..bottom_right.right;
        let y_range = self.top_left.down..bottom_right.down;

        x_range.contains(&pos.right) && y_range.contains(&pos.down)
    }
}

impl Rectangle<isize> {
    pub fn clamp(&self, pos: Vector<isize>) -> Vector<isize> {
        let bottom_right = self.top_left + self.shape;
        let x_range = self.top_left.right..bottom_right.right;
        let y_range = self.top_left.down..bottom_right.down;

        if x_range.contains(&pos.right) && y_range.contains(&pos.down) {
            return pos;
        }

        let center = (self.top_left + bottom_right) / 2;
        let Vector {
            right: dx,
            down: dy,
        } = pos - center;
        let Vector {
            right: width,
            down: height,
        } = self.shape;

        // Hits top/bottom if
        //     abs(dy/dx) > height/width
        //     abs(dy)/abs(dx) > height/width
        //     abs(dy)*width > height*abs(dx)
        let new_offset = if dy.abs() * width > dx.abs() * height {
            // scale*abs(dy) == height/2
            // scale = height/(2*abs(dy))
            //
            // new_dx = scale*dx = dx*height/(2*abs(dy))
            // new_dy = scale*dy = dy*height/(2*abs(dy)) = height/2 * sign(dy)
            Vector::new(dx * height / (2 * dy.abs()), dy.signum() * height / 2)
        } else {
            // scale*abs(dx) == width/2
            // scale = width/(2*abs(dx))
            //
            // new_dx = scale*dx = dx*width/(2*abs(dx)) = width/2 * sign(dx)
            // new_dy = scale*dy = dy*width/(2*abs(dx))
            Vector::new(dx.signum() * width / 2, dy * width / (2 * dx.abs()))
        };

        center + new_offset
    }
}

impl<T, U> std::ops::Add<Vector<U>> for Vector<T>
where
    T: std::ops::Add<U>,
{
    type Output = Vector<<T as std::ops::Add<U>>::Output>;

    fn add(self, rhs: Vector<U>) -> Self::Output {
        Vector {
            right: self.right + rhs.right,
            down: self.down + rhs.down,
        }
    }
}

impl<T, U> std::ops::Sub<Vector<U>> for Vector<T>
where
    T: std::ops::Sub<U>,
{
    type Output = Vector<<T as std::ops::Sub<U>>::Output>;

    fn sub(self, rhs: Vector<U>) -> Self::Output {
        Vector {
            right: self.right - rhs.right,
            down: self.down - rhs.down,
        }
    }
}

impl<T, U> std::ops::Mul<U> for Vector<T>
where
    T: std::ops::Mul<U>,
    U: Copy,
{
    type Output = Vector<<T as std::ops::Mul<U>>::Output>;

    fn mul(self, rhs: U) -> Self::Output {
        self.map(|value| value * rhs)
    }
}

impl<T, U> std::ops::Div<U> for Vector<T>
where
    T: std::ops::Div<U>,
    U: Copy,
{
    type Output = Vector<<T as std::ops::Div<U>>::Output>;

    fn div(self, rhs: U) -> Self::Output {
        self.map(|value| value / rhs)
    }
}

impl<T> std::fmt::Display for Vector<T>
where
    T: std::fmt::Display,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "({}, {})", self.right, self.down)
    }
}

impl From<Vector<isize>> for Vector<f32> {
    fn from(value: Vector<isize>) -> Self {
        value.map(|x| x as f32)
    }
}

impl From<&Vector<isize>> for Vector<isize> {
    fn from(value: &Vector<isize>) -> Self {
        *value
    }
}

impl<T> std::fmt::Display for Rectangle<T>
where
    T: std::fmt::Display,
    T: std::ops::Add<Output = T>,
    T: Copy,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let bottom_right = self.top_left + self.shape;
        write!(
            f,
            "({}..{}, {}..{})",
            self.top_left.right,
            bottom_right.right,
            self.top_left.down,
            bottom_right.down,
        )
    }
}
