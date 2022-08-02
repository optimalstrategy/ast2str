use ast2str::{utils::AstToStrExt, AstToStr};
use pretty_assertions::assert_eq;

pub type Ptr<'a, T> = Box<Expr<'a, T>>;

#[derive(Debug)]
pub enum Op {
    Add,
    Mul,
}

pub trait Numeric:
    std::fmt::Debug + Clone + Copy + PartialEq + std::ops::Add + std::ops::Mul
{
}
impl Numeric for f64 {}

#[derive(Debug, AstToStr)]
pub enum LitValue<'a, T: Numeric = f64> {
    Num(#[debug] T),
    Bool(#[display] bool),
    Str(#[quoted] &'a str),
    Null,
}

#[derive(AstToStr)]
#[allow(unused)]
pub struct Literal<'a, T: Numeric> {
    #[debug]
    value: LitValue<'a, T>,
    #[skip]
    lexeme: &'a str,
}

impl<'a, T: Numeric> Literal<'a, T> {
    pub fn new(value: LitValue<'a, T>) -> Self {
        Self {
            value,
            lexeme: "default lexeme",
        }
    }
}

pub struct Letters<'a> {
    letters: &'a str,
}

impl<'a> IntoIterator for &'_ Letters<'a> {
    type Item = &'a u8;

    type IntoIter = std::slice::Iter<'a, u8>;

    fn into_iter(self) -> Self::IntoIter {
        self.letters.as_bytes().iter()
    }
}

#[derive(AstToStr)]
pub struct Token<'a> {
    #[delegate = "lexeme"]
    #[rename = "lexeme"]
    source: &'a str,
    #[skip]
    span: std::ops::Range<usize>,
}

impl<'a> Token<'a> {
    pub fn lexeme(&self) -> &'a str {
        &self.source[self.span.clone()]
    }
}

#[derive(AstToStr)]
pub enum ExprKind<'a, T: Numeric> {
    Literal(#[forward] Literal<'a, T>),
    SpecialValue(#[forward] LitValue<'a, T>),
    Binary(
        #[rename = "left"] Ptr<'a, T>,
        #[debug]
        #[rename = "operator"]
        Op,
        #[rename = "right"] Ptr<'a, T>,
    ),
    Maybe {
        #[default = "Nothing"]
        value: Option<Ptr<'a, T>>,
    },
    SomeIterable(#[list] Letters<'a>),
    Mappable(#[list(|byte| byte.wrapping_mul(*byte))] Letters<'a>),
    Summable {
        #[callback(|x: &Vec<_>| x.iter().sum::<i32>())]
        #[rename = "sum"]
        values: Vec<i32>,
    },
    List(#[rename = "elements"] Vec<Expr<'a, T>>),
    Variable(#[rename = "name"] Token<'a>),
    Conditional {
        #[skip]
        skip_me_always: (),
        #[skip_if = "Option::is_none"]
        condition: Option<Ptr<'a, T>>,

        #[rename = "values"]
        #[skip_if = "Vec::is_empty"]
        my_values: Vec<Expr<'a, T>>,
    },
}

#[derive(AstToStr)]
pub struct Expr<'a, T>
where
    T: Numeric,
{
    #[forward]
    pub kind: ExprKind<'a, T>,
    pub some_other_field: (),
}

impl<'a, T: Numeric> Expr<'a, T> {
    pub fn new(kind: ExprKind<'a, T>) -> Self {
        Self {
            kind,
            some_other_field: (),
        }
    }
}

fn get_ast() -> Expr<'static, f64> {
    macro_rules! expr {
        (unboxed $name:ident { $($arg:ident : $value:expr),* }) => {
            Expr::new(ExprKind::$name { $($arg : $value),* })
        };
        (unboxed $name:ident $($arg:expr),*) => {
            Expr::new(ExprKind::$name($($arg),*))
        };
        ($name:ident $($arg:expr),*) => {
            Ptr::new(expr!(unboxed $name $($arg),*))
        };
    }
    macro_rules! lit {
        ($name:ident $value:expr) => {
            Literal::new(LitValue::$name($value))
        };
        ($name:ident) => {
            Literal::new(LitValue::$name)
        };
    }

    expr!(unboxed List vec![
        expr!(unboxed Binary
            expr!(Literal lit!(Num 1.0)),
            Op::Add,
            expr!(Binary
                expr!(Literal lit!(Bool true)),
                Op::Mul,
                expr!(Literal lit!(Str "a string"))
            )
        ),
        expr!(unboxed Maybe { value: None }),
        expr!(unboxed Maybe { value: Some(expr!(Literal lit!(Null))) }),
        expr!(unboxed SomeIterable Letters { letters: "abc" }),
        expr!(unboxed Mappable Letters { letters: "def" }),
        expr!(unboxed Summable { values: vec![1, 2, 3, 4] }),
        expr!(unboxed SpecialValue LitValue::Num(2.0)),
        expr!(unboxed SpecialValue LitValue::Bool(false)),
        expr!(unboxed SpecialValue LitValue::Str("another string")),
        expr!(unboxed Variable Token { source: "a variable  ", span: 2..10 }),
        expr!(unboxed Conditional { skip_me_always: (), condition: None, my_values: vec![] }),
        expr!(unboxed Conditional { skip_me_always: (), condition: None, my_values: vec![expr!(unboxed Literal lit!(Bool false))] }),
        expr!(unboxed Conditional { skip_me_always: (), condition: Some(expr!(Literal lit!(Bool true))), my_values: vec![] }),
        expr!(unboxed Conditional {
            skip_me_always: (),
            condition: Some(expr!(Literal lit!(Bool true))),
            my_values: vec![expr!(unboxed Literal lit!(Bool false))]
        }),
    ])
}

#[test]
fn test_ast_to_str() {
    let ast = get_ast();
    assert_eq!(
        ast.ast_to_str().trim().with_display_as_debug_wrapper(),
        r#"
ExprKind::List
╰─elements=↓
  ├─ExprKind::Binary
  │ ├─left: Literal
  │ │ ╰─value: Num(1.0)
  │ ├─operator: Add
  │ ╰─right: ExprKind::Binary
  │   ├─left: Literal
  │   │ ╰─value: Bool(true)
  │   ├─operator: Mul
  │   ╰─right: Literal
  │     ╰─value: Str("a string")
  ├─ExprKind::Maybe
  │ ╰─value: Nothing
  ├─ExprKind::Maybe
  │ ╰─value: Literal
  │   ╰─value: Null
  ├─ExprKind::SomeIterable
  │ ╰─field0=↓
  │   ├─97
  │   ├─98
  │   ╰─99
  ├─ExprKind::Mappable
  │ ╰─field0=↓
  │   ├─16
  │   ├─217
  │   ╰─164
  ├─ExprKind::Summable
  │ ╰─sum: 10
  ├─LitValue::Num
  │ ╰─field0: 2.0
  ├─LitValue::Bool
  │ ╰─field0: false
  ├─LitValue::Str
  │ ╰─field0: `another string`
  ├─ExprKind::Variable
  │ ╰─name: Token
  │   ╰─lexeme: "variable"
  ├─ExprKind::Conditional
  ├─ExprKind::Conditional
  │ ╰─values=↓
  │   ╰─Literal
  │     ╰─value: Bool(false)
  ├─ExprKind::Conditional
  │ ╰─condition: Literal
  │   ╰─value: Bool(true)
  ╰─ExprKind::Conditional
    ├─condition: Literal
    │ ╰─value: Bool(true)
    ╰─values=↓
      ╰─Literal
        ╰─value: Bool(false)"#
            .trim()
            .with_display_as_debug_wrapper()
    );
}

#[test]
fn test_ast_to_str_with_custom_symbols() {
    let ast = get_ast();
    assert_eq!(
        ast.ast_to_str_impl(&ast2str::TestSymbols)
            .trim()
            .with_display_as_debug_wrapper(),
        r#"
ExprKind::List
  elements=
    ExprKind::Binary
      left: Literal
        value: Num(1.0)
      operator: Add
      right: ExprKind::Binary
        left: Literal
          value: Bool(true)
        operator: Mul
        right: Literal
          value: Str("a string")
    ExprKind::Maybe
      value: Nothing
    ExprKind::Maybe
      value: Literal
        value: Null
    ExprKind::SomeIterable
      field0=
        97
        98
        99
    ExprKind::Mappable
      field0=
        16
        217
        164
    ExprKind::Summable
      sum: 10
    LitValue::Num
      field0: 2.0
    LitValue::Bool
      field0: false
    LitValue::Str
      field0: `another string`
    ExprKind::Variable
      name: Token
        lexeme: "variable"
    ExprKind::Conditional
    ExprKind::Conditional
      values=
        Literal
          value: Bool(false)
    ExprKind::Conditional
      condition: Literal
        value: Bool(true)
    ExprKind::Conditional
      condition: Literal
        value: Bool(true)
      values=
        Literal
          value: Bool(false)
    "#
        .trim()
        .with_display_as_debug_wrapper()
    );
}
