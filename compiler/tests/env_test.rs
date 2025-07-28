use bebop_compiler::{env::*, error::*, hir::*};
use bebop_parser::ast;
use bebop_util::{id::*, meta::*};
use insta::*;

#[test]
fn type_env() {
    let mut env = TypeEnv::new();
    let span = Span::default();
    let foo = MetaId::from("foo");
    let foo_var = Variable { id: foo };
    let foo_expr = ExprPtr::new(Expr::Variable(foo_var), span);
    env.insert(foo, foo_expr, Types::Variable);
    let bar = MetaId::from("bar");
    let bar_var = Variable { id: bar };
    let bar_expr = ExprPtr::new(Expr::Variable(bar_var), span);
    env.insert(bar, bar_expr, Types::Variable);
    assert_ron_snapshot!(env, @r#"
    TypeEnv(
      env: {
        ("foo", Types("Variable")): Variable(Variable(
          id: MetaId("foo"),
        )),
        ("bar", Types("Variable")): Variable(Variable(
          id: MetaId("bar"),
        )),
      },
    )
    "#);
    assert_ron_snapshot!(env.get(&foo, Types::Variable), @r#"
    Some(Variable(Variable(
      id: MetaId("foo"),
    )))
    "#);
    assert_ron_snapshot!(env.get(&bar, Types::Variable), @r#"
    Some(Variable(Variable(
      id: MetaId("bar"),
    )))
    "#);
    assert_ron_snapshot!(env.find(&foo, Types::Variable), @r#"
    Some(Variable(Variable(
      id: MetaId("foo"),
    )))
    "#);
    assert_ron_snapshot!(env.get(&bar, Types::Variable), @r#"
    Some(Variable(Variable(
      id: MetaId("bar"),
    )))
    "#);
}

#[test]
fn scope() -> Result<(), LiftError> {
    let mut scope = Scope::default();
    let span = Span::default();
    let foo = MetaId::from("foo");
    let foo_var = Variable { id: foo };
    let foo_expr = ExprPtr::new(Expr::Variable(foo_var), span);
    scope.insert(foo, foo_expr, Types::Variable);
    let bar = MetaId::from("bar");
    let bar_var = Variable { id: bar };
    let bar_expr = ExprPtr::new(Expr::Variable(bar_var), span);
    scope.insert(bar, bar_expr, Types::Variable);
    assert_ron_snapshot!(scope, @r#"
    Scope(
      env: TypeEnv(
        env: {
          ("foo", Types("Variable")): Variable(Variable(
            id: MetaId("foo"),
          )),
          ("bar", Types("Variable")): Variable(Variable(
            id: MetaId("bar"),
          )),
        },
      ),
    )
    "#);
    assert_ron_snapshot!(scope.lookup(&foo, Types::Variable)?, @r#"
    Variable(Variable(
      id: MetaId("foo"),
    ))
    "#);
    assert_ron_snapshot!(scope.lookup(&bar, Types::Variable)?, @r#"
    Variable(Variable(
      id: MetaId("bar"),
    ))
    "#);
    assert_ron_snapshot!(scope.find(&foo, Types::all())?, @r#"
    Variable(Variable(
      id: MetaId("foo"),
    ))
    "#);
    assert_ron_snapshot!(scope.find(&bar, Types::all())?, @r#"
    Variable(Variable(
      id: MetaId("bar"),
    ))
    "#);
    let local = scope.to_local();
    assert_ron_snapshot!(local, @r"
    Scope(
      env: TypeEnv(
        env: {},
      ),
    )
    ");
    assert_ron_snapshot!(local.find(&foo, Types::all())?, @r#"
    Variable(Variable(
      id: MetaId("foo"),
    ))
    "#);
    assert_ron_snapshot!(local.find(&bar, Types::all())?, @r#"
    Variable(Variable(
      id: MetaId("bar"),
    ))
    "#);
    Ok(())
}

#[test]
fn scope_add_vars() -> Result<(), LiftError> {
    let mut scope = Scope::default();
    let loc_id = LocId::from("foo");
    let id = MetaId::from(loc_id);
    let expr = ast::Expr::Id(loc_id);
    scope.add_local_vars(&expr, None)?;
    assert_ron_snapshot!(scope, @r#"
    Scope(
      env: TypeEnv(
        env: {
          ("foo", Types("Variable")): Variable(Variable(
            id: MetaId("foo"),
          )),
        },
      ),
    )
    "#);
    assert_ron_snapshot!(scope.lookup(&id, Types::Variable)?, @r#"
    Variable(Variable(
      id: MetaId("foo"),
    ))
    "#);
    Ok(())
}
