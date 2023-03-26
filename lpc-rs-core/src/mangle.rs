/// A trait for types that can have a mangled name
pub trait Mangle {
    /// Return my name, after mangling. Mangled names are assumed to be unique.
    fn mangle(&self) -> String;
}
