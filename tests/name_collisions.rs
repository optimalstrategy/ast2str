use ast2str::{utils::AstToStrExt, AstToStr};
use pretty_assertions::assert_eq;

#[derive(AstToStr)]
pub enum Tuple {
    Unit,
    Tuple(#[rename = "elements"] Vec<Expr>),
}

#[derive(AstToStr)]
pub enum Expr {
    Tuple(#[forward] Tuple),
}

fn get_ast() -> Expr {
    Expr::Tuple(Tuple::Tuple(vec![
        Expr::Tuple(Tuple::Unit),
        Expr::Tuple(Tuple::Tuple(vec![Expr::Tuple(Tuple::Unit)])),
    ]))
}

#[test]
fn test_ast_to_str() {
    let ast = get_ast();
    assert_eq!(
        ast.ast_to_str().trim().with_display_as_debug_wrapper(),
        r#"
Tuple::Tuple
╰─elements=↓
  ├─Tuple::Unit
  ╰─Tuple::Tuple
    ╰─elements=↓
      ╰─Tuple::Unit
        "#
        .trim()
        .with_display_as_debug_wrapper(),
    );
}
