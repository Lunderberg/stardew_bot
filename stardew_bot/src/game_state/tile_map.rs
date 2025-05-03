use dotnet_debugger::RustNativeObject;

use itertools::Itertools as _;

use super::Vector;

#[derive(Debug, Clone)]
pub struct TileMap<T> {
    values: Vec<T>,
    height: usize,
    width: usize,
}
impl<T: 'static> RustNativeObject for TileMap<T> {}

impl<T> TileMap<T> {
    #[allow(dead_code)]
    pub fn new(values: Vec<T>, width: usize, height: usize) -> Self {
        Self {
            values,
            height,
            width,
        }
    }

    pub fn full(value: T, width: usize, height: usize) -> Self
    where
        T: Copy,
    {
        Self {
            values: vec![value; width * height],
            height,
            width,
        }
    }

    #[allow(dead_code)]
    pub fn empty(width: usize, height: usize) -> Self
    where
        T: Default,
    {
        Self {
            values: (0..height * width).map(|_| Default::default()).collect(),
            height,
            width,
        }
    }

    #[allow(dead_code)]
    pub fn in_bounds(&self, index: impl AsGridPos) -> bool {
        index.get_flat_index(self.width, self.height).is_some()
    }

    pub fn get(&self, index: impl AsGridPos) -> Option<&T> {
        index
            .get_flat_index(self.width, self.height)
            .map(|flat_index| &self.values[flat_index])
    }

    pub fn iter(&self) -> impl Iterator<Item = (Vector<isize>, &T)> + '_ {
        (0..self.height)
            .cartesian_product(0..self.width)
            .map(|(j, i)| Vector::new(i as isize, j as isize))
            .map(|loc| (loc, &self[loc]))
    }

    pub fn map<Func, U>(&self, func: Func) -> TileMap<U>
    where
        Func: Fn(&T) -> U,
    {
        TileMap {
            values: self.values.iter().map(func).collect(),
            height: self.height,
            width: self.width,
        }
    }
}

pub trait AsGridPos {
    fn get_flat_index(&self, width: usize, height: usize) -> Option<usize>;

    fn display(&self) -> impl std::fmt::Debug;
}
impl AsGridPos for (isize, isize) {
    fn get_flat_index(&self, width: usize, height: usize) -> Option<usize> {
        let (i, j) = *self;
        if i < 0 || j < 0 {
            return None;
        }

        let i = i as usize;
        let j = j as usize;
        (i, j).get_flat_index(height, width)
    }

    fn display(&self) -> impl std::fmt::Debug {
        self
    }
}
impl AsGridPos for (usize, usize) {
    fn get_flat_index(&self, width: usize, height: usize) -> Option<usize> {
        let (i, j) = *self;
        (i < width && j < height).then(|| j * width + i)
    }

    fn display(&self) -> impl std::fmt::Debug {
        self
    }
}
impl AsGridPos for Vector<usize> {
    fn get_flat_index(&self, width: usize, height: usize) -> Option<usize> {
        (self.right, self.down).get_flat_index(height, width)
    }

    fn display(&self) -> impl std::fmt::Debug {
        (self.right, self.down)
    }
}
impl AsGridPos for Vector<isize> {
    fn get_flat_index(&self, width: usize, height: usize) -> Option<usize> {
        (self.right, self.down).get_flat_index(height, width)
    }

    fn display(&self) -> impl std::fmt::Debug {
        (self.right, self.down)
    }
}

impl<I, T> std::ops::Index<I> for TileMap<T>
where
    I: AsGridPos,
{
    type Output = T;

    fn index(&self, index: I) -> &Self::Output {
        let index = index
            .get_flat_index(self.width, self.height)
            .unwrap_or_else(|| {
                panic!(
                    "Index {:?} is out-of-bounds \
                 for a TileMap of shape ({}, {})",
                    index.display(),
                    self.width,
                    self.height,
                )
            });

        &self.values[index]
    }
}

impl<I, T> std::ops::IndexMut<I> for TileMap<T>
where
    I: AsGridPos,
{
    fn index_mut(&mut self, index: I) -> &mut Self::Output {
        let index = index
            .get_flat_index(self.width, self.height)
            .unwrap_or_else(|| {
                panic!(
                    "Index {:?} is out-of-bounds \
                 for a TileMap of shape ({}, {})",
                    index.display(),
                    self.width,
                    self.height,
                )
            });

        &mut self.values[index]
    }
}
