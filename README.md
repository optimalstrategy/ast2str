# ast2str

[![crates.io][crate logo]][crate]
[![Documentation][doc logo]][doc]
[![Build Status][ci logo]][ci]

A proc macro for pretty-printing ASTs and other recursive data structures.

# Basic Usage

Refer to the snippet below for a basic usage example, and see the [integration test](./tests/ast.rs) and these two repos ([#1.1](https://github.com/ves-lang/ves/blob/master/ves-parser/src/ast/mod.rs) + [#1.2](https://github.com/ves-lang/ves/blob/master/ves-parser/tests/t32_fn_if_expr_regression.test), [#2.1](https://github.com/langjam/jam0001/blob/main/dank/src/ast.rs)) for larger samples.

```rust
// Import the macro
use ast2str::AstToStr;

type Span = std::ops::Range<usize>;

// Annotate some structs and enums as desired
#[derive(AstToStr)]
struct Label {
   #[quoted]
   name: &'static str,
   #[default = "Unresolved"]
   location: Option<usize>,
}

#[derive(AstToStr)]
enum Expr {
    Binary {
        left: Box<Expr>,
        #[quoted]
        operator: &'static str,
        right: Box<Expr>
    },
    Literal(#[rename = "value"] i32, #[skip] Span),
    List { items: Vec<Expr> },
    Label(#[forward] Label),
}

// Enjoy the tree!
let expr = Expr::Binary {
    left: Box::new(Expr::Literal(5, Span::default())),
    operator: "+",
    right: Box::new(Expr::List { items: vec![
       Expr::Label(Label { name: "x", location: Some(0) }),
       Expr::Label(Label { name: "y", location: Some(1) }),
       Expr::Label(Label { name: "z", location: None }),
    ]})
};
assert_eq!(expr.ast_to_str(), r#"
Expr::Binary
├─left: Expr::Literal
│ ╰─value: 5
├─operator: `+`
╰─right: Expr::List
  ╰─items=↓
    ├─Label
    │ ├─name: `x`
    │ ╰─location: 0
    ├─Label
    │ ├─name: `y`
    │ ╰─location: 1
    ╰─Label
      ├─name: `z`
      ╰─location: Unresolved
"#.trim());

// The symbols used to draw the tree can be configured using the `Symbols` trait:
assert_eq!(expr.ast_to_str_impl(&ast2str::TestSymbols), r#"
Expr::Binary
  left: Expr::Literal
    value: 5
  operator: `+`
  right: Expr::List
    items=
      Label
        name: `x`
        location: 0
      Label
        name: `y`
        location: 1
      Label
        name: `z`
        location: Unresolved
"#.trim());
```

# Available Attributes

| Attribute                      |                                                                                                                              |
| ------------------------------ | ---------------------------------------------------------------------------------------------------------------------------- |
| None                           | Format the value with [`AstToStr`]                                                                                           |
| `#[forward]`                   | Skip all other fields and return the [`AstToStr]` of the annotated field                                                     |
| `#[skip]`                      | Skip the annoated field                                                                                                      |
| `#[display]`                   | Format the annotated field with [`Display`] instead of [`AstToStr`]                                                          |
| `#[debug]`                     | Format the annotated field with [`Debug`] instead of [`AstToStr`]                                                            |
| `#[quoted]`                    | Like `#[display]` but also wraps the value with backticks                                                                    |
| `#[list]`                      | Format the annotated field by executing AstToStr on every element of `(&field).into_iter()`                                  |
| `#[list(name_or_closure)`      | Format the annotated field by applying the callback on every element of `(&field).into_iter(                                 |
| `#[callback(name_or_closure)]` | Apply the given function or closure to `&field` and return the result                                                        |
| `#[default = "value"]`         | Only applies to `Option` types. If the value is `Some(T)`, format &T with AstToStr. Otherwise, return the value of `default` |

[crate]: https://crates.io/crates/ast2str
[crate logo]: https://img.shields.io/crates/v/ast2str.svg
[doc]: https://docs.rs/ast2str
[doc logo]: https://docs.rs/astr2str/badge.svg
[ci]: https://github.com/optimalstrategy/ast2str/actions
[ci logo]: https://github.com/optimalstrategy/ast2str/actions/workflows/rust.yml/badge.svg
