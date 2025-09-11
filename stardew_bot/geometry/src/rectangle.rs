use dsl::RustNativeObject;

use crate::Vector;

#[derive(Debug, Clone, Copy)]
pub struct Rectangle<T> {
    pub top_left: Vector<T>,
    pub shape: Vector<T>,
}
impl<T: 'static> RustNativeObject for Rectangle<T> {}

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
