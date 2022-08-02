//! A proc macro for pretty-printing ASTs and other recursive data structures.
//!
//! # Basic Usage
//! Refer to the snippet below for a basic usage example, and see the [integration test](https://github.com/optimalstrategy/ast2str/blob/main/tests/ast.rs) and
//! these two repos ([#1.1](https://github.com/ves-lang/ves/blob/master/ves-parser/src/ast/mod.rs) +
//! [#1.2](https://github.com/ves-lang/ves/blob/master/ves-parser/tests/t32_fn_if_expr_regression.test),
//! [#2.1](https://github.com/langjam/jam0001/blob/main/dank/src/ast.rs)) for larger samples.
//!
//! ```rust
//! // Import the macro
//! use ast2str::AstToStr;
//!
//! type Span = std::ops::Range<usize>;
//!
//! // Annotate some structs and enums as desired
//! #[derive(AstToStr)]
//! struct Label {
//!    #[quoted]
//!    name: &'static str,
//!    #[default = "Unresolved"]
//!    location: Option<usize>,
//! }
//!
//! #[derive(AstToStr)]
//! enum Expr {
//!     Binary {
//!         left: Box<Expr>,
//!         #[quoted]
//!         operator: &'static str,
//!         right: Box<Expr>
//!     },
//!     Literal(#[rename = "value"] i32, #[skip] Span),
//!     List { items: Vec<Expr> },
//!     Label(#[forward] Label),
//!     Optional {
//!         #[skip_if = "Option::is_none"]
//!         value: Option<&'static str>
//!     }
//! }
//!
//! let expr = Expr::Binary {
//!     left: Box::new(Expr::Literal(5, Span::default())),
//!     operator: "+",
//!     right: Box::new(Expr::List { items: vec![
//!        Expr::Label(Label { name: "x", location: Some(0) }),
//!        Expr::Label(Label { name: "y", location: Some(1) }),
//!        Expr::Label(Label { name: "z", location: None }),
//!        Expr::Optional { value: None },
//!        Expr::Optional { value: Some("a string") },
//!     ]})
//! };
//! assert_eq!(expr.ast_to_str(), r#"
//! Expr::Binary
//! ├─left: Expr::Literal
//! │ ╰─value: 5
//! ├─operator: `+`
//! ╰─right: Expr::List
//!   ╰─items=↓
//!     ├─Label
//!     │ ├─name: `x`
//!     │ ╰─location: 0
//!     ├─Label
//!     │ ├─name: `y`
//!     │ ╰─location: 1
//!     ├─Label
//!     │ ├─name: `z`
//!     │ ╰─location: Unresolved
//!     ├─Expr::Optional
//!     ╰─Expr::Optional
//!       ╰─value: "a string"
//! "#.trim());
//!
//! // The symbols used to draw the tree can be configured using the [`Symbols`] trait:
//! assert_eq!(expr.ast_to_str_impl(&ast2str::TestSymbols), r#"
//! Expr::Binary
//!   left: Expr::Literal
//!     value: 5
//!   operator: `+`
//!   right: Expr::List
//!     items=
//!       Label
//!         name: `x`
//!         location: 0
//!       Label
//!         name: `y`
//!         location: 1
//!       Label
//!         name: `z`
//!         location: Unresolved
//!       Expr::Optional
//!       Expr::Optional
//!         value: "a string"
//! "#.trim());
//! ```
//!
//! # Available Attributes
//!
//! | Attribute    |                                                                          |
//! |--------------|--------------------------------------------------------------------------|
//! | None         | Format the value with [`AstToStr`]                                       |
//! | `#[forward]` | Skip all other fields and return the [`AstToStr`] of the annotated field |
//! | `#[skip]`    | Skip the annotated field                                                  |
//! | `#[display]` | Format the annotated field with [`Display`] instead of [`AstToStr`]      |
//! | `#[debug]`   | Format the annotated field with [`Debug`] instead of [`AstToStr`]        |
//! | `#[quoted]`  | Like `#[display]` but also wraps the value with backticks                |
//! | `#[list]`    | Format the annotated field by executing AstToStr on every element of `(&field).into_iter()` |
//! | `#[list(name_or_closure)`        | Format the annotated field by applying the callback on every element of `(&field).into_iter()` |
//! | `#[callback(name_or_closure)]`   | Apply the given function or closure to `&field` and return the result |
//! | `#[delegate = "getter"]`         | Call `self.getter()` and format the result as a field |
//! | `#[default = "value"]`           | Only applies to `Option` types. If the value is `Some(T)`, format &T with AstToStr. Otherwise, return the value of `default` |
//! | `#[skip_if = "my_condition_fn"]` | Skip the annotated field if the specified function returns `true` |
//!
//! [`AstToStr`]: struct.AstToStr.html
//! [`Display`]: https://doc.rust-lang.org/std/fmt/trait.Display.html
//! [`Debug`]: https://doc.rust-lang.org/std/fmt/trait.Debug.html
//! [`Symbols`]: trait.Symbols.html
pub use ast2str_derive;
pub use ast2str_lib;

pub use ast2str_derive::AstToStr;
pub use ast2str_lib::builder::{self, TreeBuilder};
pub use ast2str_lib::{utils, AstToStr, DefaultSymbols, Symbols, TestSymbols};
