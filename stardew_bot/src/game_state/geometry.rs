use dotnet_debugger::RustNativeObject;

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

    pub fn new(right: T, down: T) -> Self {
        Self { right, down }
    }

    pub fn dot(self, other: Self) -> T
    where
        T: num::Num,
    {
        self.right * other.right + self.down * other.down
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
}

impl<T> Rectangle<T> {
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
