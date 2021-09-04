use ast2str::AstToStr;

#[derive(Debug, AstToStr)]
pub enum ExprKind {
    Test,
}

#[derive(Debug, AstToStr)]
pub struct Expr {
    #[forward]
    pub kind: ExprKind,

    pub some_other_field: (),
}

#[test]
fn test_ast_to_str() {}

#[test]
fn test_ast_to_str_with_custom_symbols() {}
