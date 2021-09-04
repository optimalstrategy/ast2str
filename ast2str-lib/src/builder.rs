use std::borrow::Cow;

use crate::{AstToStr, Symbols};

#[derive(Debug)]
pub struct TreeBuilder<'a, 's> {
    name: Cow<'a, str>,
    children: Vec<String>,
    symbols: &'s dyn Symbols,
}

impl<'a, 's> TreeBuilder<'a, 's> {
    pub fn new<S: Into<Cow<'a, str>>>(name: S, symbols: &'s dyn Symbols) -> Self {
        Self {
            name: name.into(),
            children: Vec::new(),
            symbols,
        }
    }

    pub fn with_default_symbols<S: Into<Cow<'a, str>>>(name: S) -> Self {
        Self::new(name, &crate::DefaultSymbols)
    }

    fn add_format(&mut self, key: String, value: String) {
        let key = if value.starts_with('=') {
            key
        } else {
            format!("{}: ", key)
        };
        self.children.push(format!("{}{}", key, value));
    }

    pub fn field<A: AstToStr>(mut self, name: &str, value: &A) -> Self {
        self.add_format(name.into(), value.ast_to_str_impl(self.symbols));
        self
    }

    pub fn quoted<S: std::fmt::Display>(mut self, name: &str, value: S) -> Self {
        self.add_format(name.into(), format!("`{}`", value));
        self
    }

    pub fn display<S: std::fmt::Display>(mut self, name: &str, value: S) -> Self {
        self.add_format(name.into(), value.to_string());
        self
    }

    pub fn debug<S: std::fmt::Debug>(mut self, name: &str, value: S) -> Self {
        self.children.push(format!("{}: {:?}", name, value));
        self
    }

    pub fn option<A: AstToStr>(self, field: &str, default: &str, option: &Option<A>) -> Self {
        if let Some(child) = option {
            self.field(field, child)
        } else {
            self.display(field, default)
        }
    }

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

    pub fn list_map<T>(
        mut self,
        name: &str,
        collection: impl IntoIterator<Item = T>,
        f: impl Fn(T) -> String,
    ) -> Self {
        self.children
            .push(print_ast_list_generic(name, collection, f, self.symbols));
        self
    }

    pub fn add_child<S: ToString>(mut self, child: S) -> Self {
        self.children.push(child.to_string());
        self
    }

    pub fn build(self) -> String {
        format(&self.name, self.children, self.symbols)
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
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
    format(
        &format!("={}", symbol)[..],
        collection.map(|item| f(item)).collect(),
        s,
    )
}

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

pub fn indent(tree: &str, kind: TreeIndent, s: &dyn Symbols) -> String {
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
  otherwise= "#
        );
    }
}
