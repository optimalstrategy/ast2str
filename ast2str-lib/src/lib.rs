pub mod builder;
pub mod utils;

pub use builder::TreeBuilder;

/// A trait for printing ASTs in a pretty manner.
pub trait AstToStr {
    /// This method is auto-implemented to call [`ast_to_str_impl`] with [`DefaultSymbols`].
    ///
    /// [`ast_to_str_impl`]: #tymethod.ast_to_str_impl
    fn ast_to_str(&self) -> String {
        self.ast_to_str_impl(&DefaultSymbols)
    }

    /// This method should serialize the struct or enum recursively, returning a tree.
    fn ast_to_str_impl(&self, s: &dyn Symbols) -> String;
}

/// A trait for supplying symbols to the AST formatting functions.
pub trait Symbols {
    /// The horizontal bar symbol used to draw horizontal tree branches, e.g. `─`.
    fn horizontal_bar(&self) -> &'static str;

    /// The vertical bar symbol used to draw the tree trunks of the tree and its subtrees, e.g. `│`.
    fn vertical_bar(&self) -> &'static str;

    /// A piece of trunk with a branch to the right, e.g. `├`.
    fn right_branch(&self) -> &'static str;

    /// A single indentation symbol, e.g. ` `.
    fn indent(&self) -> &'static str;

    /// The symbol for left upper corners, e.g. `╭`.
    fn left_upper_corner(&self) -> &'static str;

    /// The symbol for left bottom corners, e.g. `╰`.
    fn left_bottom_corner(&self) -> &'static str;

    /// The symbol for right upper corners, e.g. `╮`.
    fn right_upper_corner(&self) -> &'static str;

    /// The symbol for right bottom corners, e.g. `╯`.
    fn right_bottom_corner(&self) -> &'static str;

    /// The symbol to display when a list of items is empty, e.g. `✕`.
    fn missing_items_symbol(&self) -> &'static str;

    /// The symbol to display when a list of items is non-empty, e.g. `↓`.
    fn item_list_symbol(&self) -> &'static str;

    /// Used by the debug impl for `&dyn Symbols`.
    fn description(&self) -> &'static str {
        "dyn Symbols"
    }
}
impl<'s> std::fmt::Debug for &'s dyn Symbols {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.description())
    }
}

/// A macro for quick-n-dirty creation of `Symbols` implementations.
/// Accepts either 10 difference strings to use as symbols in the order they appear in the trait, or a single value to be used inside all methods.
#[macro_export]
macro_rules! create_symbols {
    ($Ty:ident,
        $horizontal_bar:expr,
        $vertical_bar:expr,
        $right_branch:expr,
        $indent:expr,
        $left_upper_corner:expr,
        $left_bottom_corner:expr,
        $right_upper_corner:expr,
        $right_bottom_corner:expr,
        $missing_items_symbol:expr,
        $item_list_symbol:expr
    ) => {
        impl Symbols for $Ty {
            fn description(&self) -> &'static str {
                stringify!($Ty)
            }
            fn horizontal_bar(&self) -> &'static str {
                $horizontal_bar
            }
            fn vertical_bar(&self) -> &'static str {
                $vertical_bar
            }
            fn right_branch(&self) -> &'static str {
                $right_branch
            }
            fn indent(&self) -> &'static str {
                $indent
            }
            fn left_upper_corner(&self) -> &'static str {
                $left_upper_corner
            }
            fn left_bottom_corner(&self) -> &'static str {
                $left_bottom_corner
            }
            fn right_upper_corner(&self) -> &'static str {
                $right_upper_corner
            }
            fn right_bottom_corner(&self) -> &'static str {
                $right_bottom_corner
            }
            fn missing_items_symbol(&self) -> &'static str {
                $missing_items_symbol
            }
            fn item_list_symbol(&self) -> &'static str {
                $item_list_symbol
            }
        }
    };
    ($Ty:ident, $sym:tt) => {
        $crate::create_symbols!($Ty, $sym, $sym, $sym, $sym, $sym, $sym, $sym, $sym, $sym, $sym);
    };
}

/// The default set of symbols that produces neatly-drawn trees.
///
/// # Example
/// ```bash
/// StmtKind::Var
/// ╰─declarations=↓
///   ╰─Var
///     ├─name: "x"
///     ├─kind: Let
///     ├─initializer: ExprKind::Binary
///     │ ├─op: Add
///     │ ├─left: Lit
///     │ │ ├─token: "1"
///     │ │ ╰─value: LitValue::Integer(1)
///     │ ╰─right: Lit
///     │   ├─token: "2.3"
///     │   ╰─value: LitValue::Float(2.3)
///     ╰─n_uses: 0
/// StmtKind::Print
/// ╰─value: ExprKind::Comma
///   ╰─operands=↓
///     ├─Lit
///     │ ├─token: "\"hello, world!\""
///     │ ╰─value: LitValue::Str("hello, world!")
///     ╰─ExprKind::Variable
///       ╰─name: "x"
/// ```
pub struct DefaultSymbols;

create_symbols!(
    DefaultSymbols,
    symbols::HORIZONTAL_BAR,
    symbols::VERTICAL_BAR,
    symbols::BRANCH,
    symbols::INDENT,
    symbols::LEFT_UPPER_CORNER,
    symbols::LEFT_BOTTOM_CORNER,
    symbols::RIGHT_UPPER_CORNER,
    symbols::RIGHT_BOTTOM_CORNER,
    symbols::CROSS,
    symbols::DOWNWARDS_POINTING_ARROW
);

/// A set of symbols where every symbol is either whitespace (` `) or an empty string.
///
/// # Example
/// ```bash
/// StmtKind::Var
///   declarations=
///     Var
///       name: "x"
///       kind: Let
///       initializer: ExprKind::Binary
///         op: Add
///         left: Lit
///           token: "1"
///           value: LitValue::Integer(1)
///         right: Lit
///           token: "2.3"
///           value: LitValue::Float(2.3)
///       n_uses: 0
/// StmtKind::Print
///   value: ExprKind::Comma
///     operands=
///       Lit
///         token: "\"hello, world!\""
///         value: LitValue::Str("hello, world!")
///       ExprKind::Variable
///         name: "x"
/// ```
pub struct TestSymbols;
create_symbols!(TestSymbols, " ", " ", " ", " ", " ", " ", " ", " ", "", "");

/// The symbols used by [`DefaultSymbols]`.
pub mod symbols {
    pub static HORIZONTAL_BAR: &str = "─";
    pub static VERTICAL_BAR: &str = "│";
    pub static BRANCH: &str = "├";
    pub static INDENT: &str = " ";
    pub static LEFT_UPPER_CORNER: &str = "╭";
    pub static LEFT_BOTTOM_CORNER: &str = "╰";
    pub static RIGHT_UPPER_CORNER: &str = "╮";
    pub static RIGHT_BOTTOM_CORNER: &str = "╯";
    pub static CROSS: &str = "✕";
    pub static DOWNWARDS_POINTING_ARROW: &str = "↓";
}

macro_rules! impl_ast {
    (debug $T:ty) => {
        impl<'a> AstToStr for $T {
            fn ast_to_str_impl(&self, _: &dyn Symbols) -> String {
                format!("{:?}", self)
            }
        }
    };
    (display $T:ty) => {
        impl<'a> AstToStr for $T {
            fn ast_to_str_impl(&self, _: &dyn Symbols) -> String {
                self.to_string()
            }
        }
    };
    (ptr $Ptr:ty) => {
        impl<T: AstToStr> AstToStr for $Ptr {
            fn ast_to_str_impl(&self, s: &dyn Symbols) -> String {
                (**self).ast_to_str_impl(s)
            }
        }
    };
    (debug $($T:ty),*) => {
        $(
            impl_ast!(debug $T);
        )*
    };
    (display $($T:ty),*) => {
        $(
            impl_ast!(display $T);
        )*
    };
    (ptr $($T:ty),*) => {
        $(
            impl_ast!(ptr $T);
        )*
    };
}

impl_ast!(debug str, &'a str, String, std::borrow::Cow<'a, str>, ());
impl_ast!(display i8, i16, i32, i64, i128);
impl_ast!(display u8, u16, u32, u64, u128);
impl_ast!(display f32, f64);
impl_ast!(display isize, usize);
impl_ast!(display bool);
impl_ast!(ptr Box<T>, std::rc::Rc<T>, std::sync::Arc<T>);

impl<T: AstToStr> AstToStr for Vec<T> {
    fn ast_to_str_impl(&self, s: &dyn Symbols) -> String {
        crate::builder::print_ast_list_without_node_name(self, |e| e.ast_to_str_impl(s), s)
    }
}

impl<T: AstToStr> AstToStr for Option<T> {
    fn ast_to_str_impl(&self, s: &dyn Symbols) -> String {
        if let Some(v) = self {
            v.ast_to_str_impl(s)
        } else {
            "None".to_owned()
        }
    }
}

impl<V: AstToStr> AstToStr for std::cell::RefCell<V> {
    fn ast_to_str_impl(&self, s: &dyn Symbols) -> String {
        self.borrow().ast_to_str_impl(s)
    }
}

impl<V: Copy + AstToStr> AstToStr for std::cell::Cell<V> {
    fn ast_to_str_impl(&self, s: &dyn Symbols) -> String {
        self.get().ast_to_str_impl(s)
    }
}

impl<K: AstToStr, V: AstToStr> AstToStr for std::collections::HashMap<K, V> {
    fn ast_to_str_impl(&self, s: &dyn Symbols) -> String {
        crate::builder::print_ast_list_without_node_name(
            self.iter().enumerate(),
            |(i, (k, v))| {
                crate::builder::TreeBuilder::new(&format!("entry: {}", i), s)
                    .field("key", k)
                    .field("value", v)
                    .build()
            },
            s,
        )
    }
}

impl<A: AstToStr, B: AstToStr> AstToStr for (A, B) {
    fn ast_to_str_impl(&self, s: &dyn Symbols) -> String {
        crate::builder::TreeBuilder::new("tuple", s)
            .field("field0", &self.0)
            .field("field1", &self.1)
            .build()
    }
}

impl<A: AstToStr, B: AstToStr, C: AstToStr> AstToStr for (A, B, C) {
    fn ast_to_str_impl(&self, s: &dyn Symbols) -> String {
        crate::builder::TreeBuilder::new("tuple", s)
            .field("field0", &self.0)
            .field("field1", &self.1)
            .field("field2", &self.2)
            .build()
    }
}

impl<A: AstToStr, B: AstToStr, C: AstToStr, D: AstToStr> AstToStr for (A, B, C, D) {
    fn ast_to_str_impl(&self, s: &dyn Symbols) -> String {
        crate::builder::TreeBuilder::new("tuple", s)
            .field("field0", &self.0)
            .field("field1", &self.1)
            .field("field2", &self.2)
            .field("field3", &self.3)
            .build()
    }
}

impl<T: std::fmt::Debug> AstToStr for std::ops::Range<T> {
    fn ast_to_str_impl(&self, _: &dyn Symbols) -> String {
        format!("{:?}", self)
    }
}

impl std::fmt::Display for dyn AstToStr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.ast_to_str())
    }
}
