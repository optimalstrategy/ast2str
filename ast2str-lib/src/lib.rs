pub mod builder;

/// A trait for printing ASTs in a pretty manner.
pub trait AstToStr {
    /// This method should serialize the struct or enum recursively, returning a subtree.
    fn ast_to_str(&self) -> String;
}

/// The symbols used by the [`format`] functions.
pub mod symbols {
    pub static HORIZONTAL_BAR: &str = "─";
    pub static VERTICAL_BAR: &str = "│";
    pub static BRANCH: &str = "├";
    pub static INDENT: &str = " ";
    pub static LEFT_UPPER_CORNER: &str = "╭";
    pub static LEFT_BOTTOM_CORNER: &str = "╰";
    pub static RIGHT_UPPER_CORNER: &str = "╮";
    pub static RIGHT_BOTTOM_CORNER: &str = "╯";
}

macro_rules! impl_ast {
    (debug $T:ty) => {
        impl<'a> AstToStr for $T {
            fn ast_to_str(&self) -> String {
                format!("{:?}", self)
            }
        }
    };
    (display $T:ty) => {
        impl<'a> AstToStr for $T {
            fn ast_to_str(&self) -> String {
                self.to_string()
            }
        }
    };
    (ptr $Ptr:ty) => {
        impl<T: AstToStr> AstToStr for $Ptr {
            fn ast_to_str(&self) -> String {
                (**self).ast_to_str()
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
    fn ast_to_str(&self) -> String {
        crate::builder::print_ast_list_without_node_name(self, |e| e.ast_to_str())
    }
}

impl<T: AstToStr> AstToStr for Option<T> {
    fn ast_to_str(&self) -> String {
        if let Some(v) = self {
            v.ast_to_str()
        } else {
            "None".to_owned()
        }
    }
}

impl<K: AstToStr, V: AstToStr> AstToStr for std::collections::HashMap<K, V> {
    fn ast_to_str(&self) -> String {
        crate::builder::print_ast_list_without_node_name(self.iter().enumerate(), |(i, (k, v))| {
            crate::builder::TreeBuilder::new(&format!("entry: {}", i))
                .field("key", k)
                .field("value", v)
                .build()
        })
    }
}

impl<T: std::fmt::Debug> AstToStr for std::ops::Range<T> {
    fn ast_to_str(&self) -> String {
        format!("{:?}", self)
    }
}

impl std::fmt::Display for dyn AstToStr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.ast_to_str())
    }
}
