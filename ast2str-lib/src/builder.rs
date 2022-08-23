//! This module defines the data structures and functions used to draw trees.
use std::borrow::Cow;

use crate::{AstToStr, Symbols};

/// A builder struct for formatting AST nodes.
#[derive(Debug)]
pub struct TreeBuilder<'a, 's> {
    /// The name of the node.
    name: Cow<'a, str>,
    /// The list of this node's children.
    children: Vec<String>,
    /// The symbols to use for drawing the tree.
    symbols: &'s dyn Symbols,
}

impl<'a, 's> TreeBuilder<'a, 's> {
    /// Creates a new [`TreeBuilder`] instance with the given node name and [`Symbols`].
    ///
    /// [`TreeBuilder`]: struct.TreeBuilder.html
    /// [`Symbols`]: trait.Symbols.html
    pub fn new<S: Into<Cow<'a, str>>>(name: S, symbols: &'s dyn Symbols) -> Self {
        Self {
            name: name.into(),
            children: Vec::new(),
            symbols,
        }
    }

    /// Creates a new [`TreeBuilder`] configured to use [`DefaultSymbols`].
    ///
    /// [`TreeBuilder`]: struct.TreeBuilder.html
    /// [`DefaultSymbols`]: struct.DefaultSymbols.html
    pub fn with_default_symbols<S: Into<Cow<'a, str>>>(name: S) -> Self {
        Self::new(name, &crate::DefaultSymbols)
    }

    /// Adds a new child with the given name to the tree, recursively calling [`AstToStr::ast_to_str_impl`] on the value.
    ///
    /// # Example
    /// ```
    /// # use ast2str_lib as ast2str;
    /// use ast2str::{builder::TreeBuilder};
    ///
    /// let mut tree = TreeBuilder::with_default_symbols("Expr");
    /// tree = tree.field("kind", &"ExprKind::Literal");
    /// tree = tree.field("span", &(3..10));
    /// assert_eq!(
    ///     tree.build(),
    ///     r#"
    /// Expr
    /// ├─kind: "ExprKind::Literal"
    /// ╰─span: 3..10
    /// "#.trim());
    /// ```
    ///
    /// [`AstToStr::ast_to_str_impl`]: trait.AstToStr.html#tymethod.ast_to_str_impl
    pub fn field<A: AstToStr>(mut self, name: &str, value: &A) -> Self {
        self.add_format(name.into(), value.ast_to_str_impl(self.symbols));
        self
    }

    /// Adds a new child with the given name to the tree, formatting its value as [`Display`] surrounded with backticks.
    ///
    /// # Example
    /// ```
    /// # use ast2str_lib as ast2str;
    /// use ast2str::{builder::TreeBuilder};
    ///
    /// let mut tree = TreeBuilder::with_default_symbols("Token");
    /// tree = tree.quoted("lexeme", "\"a string\"");
    /// tree = tree.field("span", &(0..8));
    /// assert_eq!(
    ///     tree.build(),
    ///     r#"
    /// Token
    /// ├─lexeme: `"a string"`
    /// ╰─span: 0..8
    /// "#.trim());
    /// ```
    ///
    /// [`Display`]: https://doc.rust-lang.org/std/fmt/trait.Display.html
    pub fn quoted<S: std::fmt::Display>(mut self, name: &str, value: S) -> Self {
        self.add_format(name.into(), format!("`{}`", value));
        self
    }

    /// Adds a new child with the given name to the tree, formatting its value as [`Display`].
    ///
    /// # Example
    /// ```
    /// # use ast2str_lib as ast2str;
    /// use ast2str::{builder::TreeBuilder};
    ///
    /// let mut tree = TreeBuilder::with_default_symbols("Variable");
    /// tree = tree.display("name", "x");
    /// assert_eq!(
    ///     tree.build(),
    ///     r#"
    /// Variable
    /// ╰─name: x
    /// "#.trim());
    /// ```
    ///
    /// [`Display`]: https://doc.rust-lang.org/std/fmt/trait.Display.html
    pub fn display<S: std::fmt::Display>(mut self, name: &str, value: S) -> Self {
        self.add_format(name.into(), value.to_string());
        self
    }

    /// Adds a new child with the given name to the tree, formatting its value as [`Debug`].
    ///
    /// # Example
    /// ```
    /// # use ast2str_lib as ast2str;
    /// use ast2str::{builder::TreeBuilder};
    ///
    /// let mut tree = TreeBuilder::with_default_symbols("Binary");
    /// tree = tree.field("left", &1);
    /// tree = tree.debug("operator", &std::cmp::Ordering::Less);
    /// tree = tree.field("right", &3);
    /// assert_eq!(
    ///     tree.build(),
    ///     r#"
    /// Binary
    /// ├─left: 1
    /// ├─operator: Less
    /// ╰─right: 3
    /// "#.trim());
    /// ```
    ///
    /// [`Debug`]: https://doc.rust-lang.org/std/fmt/trait.Debug.html]
    pub fn debug<S: std::fmt::Debug>(mut self, name: &str, value: S) -> Self {
        self.children.push(format!("{}: {:?}", name, value));
        self
    }

    /// Attempts to add a new child to the tree with [`field`] if the option is [`Some`], recursively calling [`AstToStr::ast_to_str_impl`] on its value.
    /// If the option is `None`, falls back to [`display`] with the given default as its value.
    ///
    /// # Example
    /// ```
    /// # use ast2str_lib as ast2str;
    /// use ast2str::{builder::TreeBuilder};
    ///
    /// let mut tree = TreeBuilder::with_default_symbols("VarDecl");
    /// tree = tree.quoted("name", "x");
    /// tree = tree.option("initializer", "<None>", &None::<i32>);
    /// assert_eq!(
    ///     tree.build(),
    ///     r#"
    /// VarDecl
    /// ├─name: `x`
    /// ╰─initializer: <None>
    /// "#.trim());
    ///
    /// let mut tree = TreeBuilder::with_default_symbols("VarDecl");
    /// tree = tree.quoted("name", "x");
    /// tree = tree.option("initializer", "<None>", &Some(7));
    /// assert_eq!(
    ///     tree.build(),
    ///     r#"
    /// VarDecl
    /// ├─name: `x`
    /// ╰─initializer: 7
    /// "#.trim());
    /// ```
    ///
    /// [`AstToStr::ast_to_str_impl`]: trait.AstToStr.html#tymethod.ast_to_str_impl
    /// [`field`]: struct.TreeBuilder.html#method.field
    /// [`display`]: struct.TreeBuilder.html#method.display
    pub fn option<A: AstToStr>(self, field: &str, default: &str, option: &Option<A>) -> Self {
        if let Some(child) = option {
            self.field(field, child)
        } else {
            self.display(field, default)
        }
    }

    /// Adds the given collection of items to the tree as a child, recursively calling [`AstToStr::ast_to_str_impl`] on each item.
    /// If the collection is empty, [`Symbols::missing_items_symbol`] will be displayed; otherwise, [`Symbols::item_list_symbol`].
    ///
    /// # Example
    /// ```
    /// # use ast2str_lib as ast2str;
    /// use ast2str::{builder::TreeBuilder};
    ///
    /// let mut tree = TreeBuilder::with_default_symbols("List");
    /// tree = tree.list("non_empty", &[1usize, 2, 3]);
    /// tree = tree.list("empty", &Vec::<usize>::new());
    /// assert_eq!(
    ///     tree.build(),
    ///     r#"
    /// List
    /// ├─non_empty=↓
    /// │ ├─1
    /// │ ├─2
    /// │ ╰─3
    /// ╰─empty=✕
    /// "#.trim());
    /// ```
    ///
    /// [`AstToStr::ast_to_str_impl`]: trait.AstToStr.html#tymethod.ast_to_str_impl
    /// [`Symbols::missing_items_symbol`]: trait.Symbols.html#tymethod.missing_items_symbol
    /// [`Symbols::item_list_symbol`]: trait.Symbols.html#tymethod.item_list_symbol
    /// [`DefaultSymbols`]: struct.DefaultSymbols.html
    pub fn list<'b, S: AstToStr + 'static>(
        mut self,
        name: &str,
        collection: impl IntoIterator<Item = &'b S>,
    ) -> Self {
        self.children.push(print_ast_list_generic(
            name,
            collection,
            |s| s.ast_to_str_impl(self.symbols),
            self.symbols,
        ));
        self
    }

    /// Just like [`list`], but allows the user to pass a function, applying it to every item.
    ///
    /// # Example
    /// ```
    /// # use ast2str_lib as ast2str;
    /// use ast2str::{builder::TreeBuilder};
    ///
    /// let mut tree = TreeBuilder::with_default_symbols("List");
    /// tree = tree.list_map("values", &[1, 2, 3], |x| x * x);
    /// assert_eq!(
    ///     tree.build(),
    ///     r#"
    /// List
    /// ╰─values=↓
    ///   ├─1
    ///   ├─4
    ///   ╰─9
    /// "#.trim());
    /// ```
    /// [`list`]: struct.TreeBuilder.html#method.list
    pub fn list_map<T, A: AstToStr>(
        mut self,
        name: &str,
        collection: impl IntoIterator<Item = T>,
        f: impl Fn(T) -> A,
    ) -> Self {
        self.children.push(print_ast_list_generic(
            name,
            collection,
            |t| f(t).ast_to_str_impl(self.symbols),
            self.symbols,
        ));
        self
    }

    /// Adds the given value to the tree as a child.
    ///
    /// # Example
    /// ```
    /// # use ast2str_lib as ast2str;
    /// use ast2str::{builder::TreeBuilder};
    ///
    /// let mut tree = TreeBuilder::with_default_symbols("root");
    /// tree = tree.add_child(std::f64::consts::PI);
    /// tree = tree.add_child("any ToString value works");
    /// assert_eq!(
    ///     tree.build(),
    ///     r#"
    /// root
    /// ├─3.141592653589793
    /// ╰─any ToString value works
    /// "#.trim());
    /// ```
    /// [`list`]: struct.TreeBuilder.html#method.list
    pub fn add_child<S: ToString>(mut self, child: S) -> Self {
        self.children.push(child.to_string());
        self
    }

    /// Consumes the builder, formatting the node and its children as a tree. See the other methods for examples.
    pub fn build(self) -> String {
        format(&self.name, self.children, self.symbols)
    }

    fn add_format(&mut self, key: String, value: String) {
        let key = if value.starts_with('=') {
            key
        } else {
            format!("{}: ", key)
        };
        self.children.push(format!("{}{}", key, value));
    }
}

// This enum should be private, but it has been public for a while. Although unlikely, but someone could be
// importing it in their code.
#[doc(hidden)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TreeIndent {
    Trunk,
    Branch,
}

impl TreeIndent {
    #[inline]
    pub fn is_trunk(&self) -> bool {
        *self == TreeIndent::Trunk
    }
}

/// A function used by [`TreeBuilder::list`] and [`TreeBuilder::list_map`] for list formatting.
/// Also see [`print_ast_list_without_node_name`].
pub fn print_ast_list_generic<T>(
    node: &str,
    collection: impl IntoIterator<Item = T>,
    f: impl Fn(T) -> String,
    s: &dyn Symbols,
) -> String {
    format!(
        "{}{}",
        node,
        print_ast_list_without_node_name(collection, f, s)
    )
}

/// A lower-level function behind [`print_ast_list_generic`] that _actually_ does the formatting.
/// Useful for manually implementing [`AstToStr`] for [`Vec`]-like types.
///
/// # Example
/// ```
/// # use ast2str_lib as ast2str;
/// use ast2str::{AstToStr, Symbols, builder::print_ast_list_without_node_name};
///
/// struct Wrapper(Vec<i32>);
/// impl<'a> IntoIterator for &'a Wrapper {
///     type Item = &'a i32;
///     type IntoIter = std::slice::Iter<'a, i32>;
///     fn into_iter(self) -> Self::IntoIter {
///         self.0.iter()
///     }
/// }

/// impl AstToStr for Wrapper {
///     fn ast_to_str_impl(&self, symbols: &dyn Symbols) -> String {
///         print_ast_list_without_node_name(self, |x| x.ast_to_str_impl(symbols), symbols)     
///     }
/// }
///
/// let wrapper = Wrapper(vec![1, 2, 3]);
/// assert_eq!(wrapper.ast_to_str(), r#"
/// =↓
/// ├─1
/// ├─2
/// ╰─3
/// "#.trim());
/// ```
pub fn print_ast_list_without_node_name<T>(
    collection: impl IntoIterator<Item = T>,
    f: impl Fn(T) -> String,
    s: &dyn Symbols,
) -> String {
    let mut collection = collection.into_iter().peekable();
    let symbol = if collection.peek().is_none() {
        s.missing_items_symbol()
    } else {
        s.item_list_symbol()
    };
    format(&format!("={}", symbol)[..], collection.map(f).collect(), s)
}

/// Very inefficiently formats the given node and children into a tree by indenting every line.
///
// # Example
/// ```
/// # use ast2str_lib as ast2str;
/// use ast2str::{DefaultSymbols, builder::format};
///
/// let output = format(
///     "A",
///     vec![
///         format("B", vec!["b1".to_string(), "b2".to_string(), "b3".to_string()], &DefaultSymbols),
///         format("C", vec![format("D", vec!["d1".to_string(), "d2".to_string()], &DefaultSymbols)], &DefaultSymbols)
///     ],
///     &DefaultSymbols
/// );
/// assert_eq!(output, r#"
/// A
/// ├─B
/// │ ├─b1
/// │ ├─b2
/// │ ╰─b3
/// ╰─C
///   ╰─D
///     ├─d1
///     ╰─d2
/// "#.trim());
/// ```
pub fn format(tree: &str, children: Vec<String>, symbols: &dyn Symbols) -> String {
    let mut new_tree = Vec::with_capacity(children.len());
    new_tree.push(tree.to_string());

    if !children.is_empty() {
        for child in &children[0..children.len() - 1] {
            new_tree.push(indent(child, TreeIndent::Trunk, symbols));
        }

        new_tree.push(indent(
            children.last().unwrap(),
            TreeIndent::Branch,
            symbols,
        ));
    }

    new_tree.join("\n")
}

fn indent(tree: &str, kind: TreeIndent, s: &dyn Symbols) -> String {
    let tree = tree.split('\n').collect::<Vec<&str>>();
    let mut new_tree = vec![];

    // Handle the root first
    let wood = if kind.is_trunk() {
        format!("{}{}", s.right_branch(), s.horizontal_bar())
    } else {
        format!("{}{}", s.left_bottom_corner(), s.horizontal_bar())
    };
    new_tree.push(format!("{}{}", wood, tree[0]));

    let indent = s.indent();

    if tree.len() > 1 {
        for child in tree[1..tree.len()].iter() {
            let wood = if kind.is_trunk() {
                format!("{}{}", s.vertical_bar(), indent)
            } else {
                indent.repeat(2)
            };

            new_tree.push(format!("{}{}", wood, child));
        }
    }

    new_tree.join("\n")
}

#[cfg(test)]
pub mod tests {
    use super::*;
    use crate::AstToStr;
    use pretty_assertions::assert_eq;

    #[derive(Debug)]
    enum BinOpKind {
        Plus,
        Minus,
    }
    struct Token {
        lexeme: &'static str,
    }

    struct If {
        condition: Box<Expr>,
        then: Vec<Expr>,
        otherwise: Option<Vec<Expr>>,
    }

    enum Expr {
        Binary(Box<Expr>, BinOpKind, Box<Expr>),
        Variable(Token),
        If(If),
    }

    impl AstToStr for Token {
        fn ast_to_str_impl(&self, _: &dyn crate::Symbols) -> String {
            self.lexeme.to_string()
        }
    }

    impl AstToStr for Expr {
        fn ast_to_str_impl(&self, s: &dyn crate::Symbols) -> String {
            match self {
                Expr::Binary(l, op, r) => TreeBuilder::new("Expr::Binary", s)
                    .field("left", l)
                    .debug("op", op)
                    .field("right", r)
                    .build(),
                Expr::Variable(v) => TreeBuilder::new("Expr::Variable", s)
                    .quoted("name", &v.lexeme)
                    .build(),
                Expr::If(r#if) => TreeBuilder::new("Expr::If", s)
                    .field("condition", &r#if.condition)
                    .list("then", r#if.then.iter())
                    .option("otherwise", "------", &r#if.otherwise)
                    .build(),
            }
        }
    }

    #[test]
    fn test_builder() {
        let ast = Expr::If(If {
            condition: Box::new(Expr::Binary(
                Box::new(Expr::Variable(Token { lexeme: "a" })),
                BinOpKind::Plus,
                Box::new(Expr::Variable(Token { lexeme: "b" })),
            )),
            then: vec![Expr::Binary(
                Box::new(Expr::Variable(Token { lexeme: "c" })),
                BinOpKind::Minus,
                Box::new(Expr::Variable(Token { lexeme: "d" })),
            )],
            otherwise: Some(vec![]),
        });

        assert_eq!(
            ast.ast_to_str(),
            r#"Expr::If
├─condition: Expr::Binary
│ ├─left: Expr::Variable
│ │ ╰─name: `a`
│ ├─op: Plus
│ ╰─right: Expr::Variable
│   ╰─name: `b`
├─then=↓
│ ╰─Expr::Binary
│   ├─left: Expr::Variable
│   │ ╰─name: `c`
│   ├─op: Minus
│   ╰─right: Expr::Variable
│     ╰─name: `d`
╰─otherwise=✕"#
        );
    }

    #[test]
    fn test_builder_with_custom_symbols() {
        let ast = Expr::If(If {
            condition: Box::new(Expr::Binary(
                Box::new(Expr::Variable(Token { lexeme: "a" })),
                BinOpKind::Plus,
                Box::new(Expr::Variable(Token { lexeme: "b" })),
            )),
            then: vec![Expr::Binary(
                Box::new(Expr::Variable(Token { lexeme: "c" })),
                BinOpKind::Minus,
                Box::new(Expr::Variable(Token { lexeme: "d" })),
            )],
            otherwise: Some(vec![]),
        });

        println!("{}", ast.ast_to_str_impl(&crate::TestSymbols));
        assert_eq!(
            ast.ast_to_str_impl(&crate::TestSymbols),
            r#"Expr::If
  condition: Expr::Binary
    left: Expr::Variable
      name: `a`
    op: Plus
    right: Expr::Variable
      name: `b`
  then=
    Expr::Binary
      left: Expr::Variable
        name: `c`
      op: Minus
      right: Expr::Variable
        name: `d`
  otherwise="#
        );
    }
}
