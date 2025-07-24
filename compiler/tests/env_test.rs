use bebop_compiler::{env::*, error::*, hir::*, pool::*};
use bebop_parser::ast;
use bebop_util::meta::*;
use insta::*;

#[test]
fn type_env() {
    let mut pool = ExprPool::new();
    let mut env = TypeEnv::new();
    let span = Span::default();
    let foo = Id::new(Ident::new("foo"));
    let foo_var = Variable { id: foo };
    let foo_expr = pool.add(Expr::Variable(foo_var), span);
    env.insert(foo, foo_expr, Types::Variable);
    let bar = Id::new(Ident::new("bar"));
    let bar_var = Variable { id: bar };
    let bar_expr = pool.add(Expr::Variable(bar_var), span);
    env.insert(bar, bar_expr, Types::Variable);
    assert_ron_snapshot!(env, @r#"
    TypeEnv(
      env: {
        "foo": [
          (Types("Variable"), 0),
        ],
        "bar": [
          (Types("Variable"), 1),
        ],
      },
    )
    "#);
}

#[test]
fn scope() -> Result<(), LiftError> {
    let mut scope = Scope::default();
    let mut pool = ExprPool::new();
    let span = Span::default();
    let foo = Id::new(Ident::new("foo"));
    let foo_var = Variable { id: foo };
    let foo_expr = pool.add(Expr::Variable(foo_var), span);
    scope.insert(foo, foo_expr, Types::Variable);
    let bar = Id::new(Ident::new("bar"));
    let bar_var = Variable { id: bar };
    let bar_expr = pool.add(Expr::Variable(bar_var), span);
    scope.insert(bar, bar_expr, Types::Variable);
    assert_ron_snapshot!(scope, @r#"
    Scope(
      env: TypeEnv(
        env: {
          "foo": [
            (Types("Variable"), 0),
          ],
          "bar": [
            (Types("Variable"), 1),
          ],
        },
      ),
    )
    "#);
    let local = scope.to_local();
    assert_ron_snapshot!(local, @r"
    Scope(
      env: TypeEnv(
        env: {},
      ),
    )
    ");
    Ok(())
}

#[test]
fn scope_add_vars() -> Result<(), LiftError> {
    let mut scope = Scope::default();
    let mut pool = ExprPool::new();
    let id = Loc::new(Ident::new("foo"), Span::default());
    let expr = ast::Expr::Id(id);
    scope.add_local_vars(&mut pool, &expr, None)?;
    assert_ron_snapshot!(scope, @r#"
    Scope(
      env: TypeEnv(
        env: {
          "foo": [
            (Types("Variable"), 0),
          ],
        },
      ),
    )
    "#);
    Ok(())
}
