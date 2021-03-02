use codespan_reporting::files::{Files, Error};
use modular_bitfield::private::static_assertions::_core::ops::Range;
use std::collections::HashMap;
use std::hash::Hash;

pub struct LazyFiles {
    files: HashMap<String, Option<String>>
}

impl LazyFiles {
    pub fn new() -> Self {
        Self {
            files: HashMap::new()
        }
    }

    pub fn add<T: AsRef<str> + Hash>(&mut self, name: T) {
        self.files.insert(String::from(name.as_ref()), None);
    }
}

impl<'input> Files<'input> for LazyFiles {
    type FileId = &'input str;
    type Name = &'input str;
    type Source = &'input str;

    fn name(&self, id: Self::FileId) -> Result<Self::Name, Error> {
        Ok(id)
    }

    fn source(&self, id: Self::FileId) -> Result<Self::Source, Error> {
        unimplemented!()
    }

    fn line_index(&self, id: Self::FileId, byte_index: usize) -> Result<usize, Error> {
        unimplemented!()
    }

    fn line_range(&self, id: Self::FileId, line_index: usize) -> Result<Range<usize>, Error> {
        unimplemented!()
    }
}