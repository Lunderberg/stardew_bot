use dotnet_debugger::RustNativeObject;

#[derive(Debug, Clone, Copy)]
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
