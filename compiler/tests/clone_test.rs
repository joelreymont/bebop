use bebop_compiler::{env::*, ir::*};
use bebop_util::{id::*, meta::*};
use insta::*;

#[test]
fn test_expr_clone() {
    let span = Span::default();
    let foo = MetaId::from("foo");
    let foo_var = Variable { id: foo };
    let foo_expr = ExprPtr::new(Expr::Variable(foo_var), span);
    let mut bar_expr = foo_expr.clone();
    bar_expr.apply_mut(|expr| {
        if let Expr::Variable(Variable { id }) = expr {
            *id = MetaId::from("bar");
        }
    });
    foo_expr.apply(|expr| {
        assert_ron_snapshot!(expr, @r#"
        Variable(Variable(
          id: MetaId("bar"),
        ))
        "#)
    });
    bar_expr.apply(|expr| {
        assert_ron_snapshot!(expr, @r#"
        Variable(Variable(
          id: MetaId("bar"),
        ))
        "#)
    });
}

#[test]
fn test_expr_deepcopy() {
    let span = Span::default();
    let foo = MetaId::from("foo");
    let foo_var = Variable { id: foo };
    let mut foo_expr = ExprPtr::new(Expr::Variable(foo_var), span);
    let mut env = Env::default();
    let mut bar_expr = foo_expr.deepcopy(&mut env);
    bar_expr.apply_mut(|expr| {
        if let Expr::Variable(Variable { id }) = expr {
            *id = MetaId::from("bar");
        }
    });
    foo_expr.apply(|expr| {
        assert_ron_snapshot!(expr, @r#"
        Variable(Variable(
          id: MetaId("foo"),
        ))
        "#)
    });
    bar_expr.apply(|expr| {
        assert_ron_snapshot!(expr, @r#"
        Variable(Variable(
          id: MetaId("bar"),
        ))
        "#)
    });
}
