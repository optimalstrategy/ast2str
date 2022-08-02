//! This module contains a simple wrapper for formatting [`Display`] objects as [`Debug`] and an extension method that can be called on strings.
//! Mainly useful for testing.
//!
//! # Example:
//! ```rust
//! # use ast2str_lib as ast2str;
//! use ast2str::utils::{DisplayAsDebugWrapper, AstToStrExt};
//!
//! assert_eq!(format!("{:?}", "foo".with_display_as_debug_wrapper()), "foo");
//! assert_eq!(format!("{:?}", "foo"), "\"foo\"");
//! assert_eq!(format!("{:?}", DisplayAsDebugWrapper("foo")), "foo");
//! ```
//! [`Display`]: https://doc.rust-lang.org/std/fmt/trait.Display.html
//! [`Debug`]: https://doc.rust-lang.org/std/fmt/trait.Debug.html
//!

/// Wraps a type that implements [`Display`] and provides a [`Debug`] implementation that calls [`Display::fmt`].
///
/// [`Display`]: https://doc.rust-lang.org/std/fmt/trait.Display.html
/// [`Debug`]: https://doc.rust-lang.org/std/fmt/trait.Debug.html
/// [`Display::fmt`]: https://doc.rust-lang.org/std/fmt/trait.Display.html#tymethod.fmt
#[allow(clippy::derive_partial_eq_without_eq)]
#[derive(Clone, PartialEq)]
pub struct DisplayAsDebugWrapper<T>(pub T);

/// This trait provides a convenience method for wrapping types that implement [`Display`] with [`DisplayAsDebugWrapper`].
/// Implemented for [`String`], [`str`], and [`Cow`] by default.
///
/// [`Display`]: https://doc.rust-lang.org/std/fmt/trait.Display.html
/// [`Cow`]: https://doc.rust-lang.org/std/borrow/enum.Cow.html
pub trait AstToStrExt {
    /// Wraps the object with a [`DisplayAsDebugWrapper`].
    fn with_display_as_debug_wrapper(&self) -> DisplayAsDebugWrapper<&'_ Self>
    where
        Self: std::fmt::Display,
    {
        DisplayAsDebugWrapper(self)
    }
}
impl AstToStrExt for String {}
impl AstToStrExt for str {}
impl<'a> AstToStrExt for std::borrow::Cow<'a, str> {}

impl<T: std::fmt::Display> crate::AstToStr for DisplayAsDebugWrapper<T> {
    fn ast_to_str_impl(&self, _: &dyn crate::Symbols) -> String {
        format!("{:?}", self)
    }
}

impl<T> std::fmt::Debug for DisplayAsDebugWrapper<T>
where
    T: std::fmt::Display,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl<T> std::ops::Deref for DisplayAsDebugWrapper<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}
