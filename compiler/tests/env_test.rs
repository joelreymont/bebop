use bebop_compiler::{env::*, error::*, ir::*};
use bebop_parser::ast;
use bebop_util::{id::*, meta::*};
use insta::*;

#[test]
fn test_type_map() {
    let mut env = TypeMap::default();
    let span = Span::default();
    let foo = MetaId::from("foo");
    let foo_var = Variable { id: foo };
    let foo_expr = ExprPtr::new(Expr::Variable(foo_var), span);
    env.insert(foo.into_id(), foo_expr, Kind::Variable);
    let bar = MetaId::from("bar");
    let bar_var = Variable { id: bar };
    let bar_expr = ExprPtr::new(Expr::Variable(bar_var), span);
    env.insert(bar.into_id(), bar_expr, Kind::Variable);
    assert_ron_snapshot!(env, @r#"
    TypeMap(
      inner: {
        Key(
          id: "foo",
          kind: Kind("Variable"),
        ): Value(
          expr: Variable(Variable(
            id: MetaId("foo"),
          )),
        ),
        Key(
          id: "bar",
          kind: Kind("Variable"),
        ): Value(
          expr: Variable(Variable(
            id: MetaId("bar"),
          )),
        ),
      },
    )
    "#);
    assert_ron_snapshot!(env.get(&foo, Kind::Variable), @r#"
    Some(Variable(Variable(
      id: MetaId("foo"),
    )))
    "#);
    assert_ron_snapshot!(env.get(&bar, Kind::Variable), @r#"
    Some(Variable(Variable(
      id: MetaId("bar"),
    )))
    "#);
    assert_ron_snapshot!(env.find(&foo, Kind::Variable), @r#"
    Some(Variable(Variable(
      id: MetaId("foo"),
    )))
    "#);
    assert_ron_snapshot!(env.get(&bar, Kind::Variable), @r#"
    Some(Variable(Variable(
      id: MetaId("bar"),
    )))
    "#);
}

#[test]
fn test_env() -> Result<(), Error> {
    let mut scope = Env::default();
    let span = Span::default();
    let foo = MetaId::from("foo");
    let foo_var = Variable { id: foo };
    let foo_expr = ExprPtr::new(Expr::Variable(foo_var), span);
    scope.insert(foo, foo_expr, Kind::Variable);
    let bar = MetaId::from("bar");
    let bar_var = Variable { id: bar };
    let bar_expr = ExprPtr::new(Expr::Variable(bar_var), span);
    scope.insert(bar, bar_expr, Kind::Variable);
    assert_ron_snapshot!(scope, @r#"
    Env(
      env: TypeMap(
        inner: {
          Key(
            id: "foo",
            kind: Kind("Variable"),
          ): Value(
            expr: Variable(Variable(
              id: MetaId("foo"),
            )),
          ),
          Key(
            id: "bar",
            kind: Kind("Variable"),
          ): Value(
            expr: Variable(Variable(
              id: MetaId("bar"),
            )),
          ),
        },
      ),
    )
    "#);
    assert_ron_snapshot!(scope.lookup(&foo, Kind::Variable)?, @r#"
    Variable(Variable(
      id: MetaId("foo"),
    ))
    "#);
    assert_ron_snapshot!(scope.lookup(&bar, Kind::Variable)?, @r#"
    Variable(Variable(
      id: MetaId("bar"),
    ))
    "#);
    assert_ron_snapshot!(scope.find(&foo, Kind::all())?, @r#"
    Variable(Variable(
      id: MetaId("foo"),
    ))
    "#);
    assert_ron_snapshot!(scope.find(&bar, Kind::all())?, @r#"
    Variable(Variable(
      id: MetaId("bar"),
    ))
    "#);
    let local = scope.to_local();
    assert_ron_snapshot!(local, @r"
    Env(
      env: TypeMap(
        inner: {},
      ),
    )
    ");
    assert_ron_snapshot!(local.find(&foo, Kind::all())?, @r#"
    Variable(Variable(
      id: MetaId("foo"),
    ))
    "#);
    assert_ron_snapshot!(local.find(&bar, Kind::all())?, @r#"
    Variable(Variable(
      id: MetaId("bar"),
    ))
    "#);
    Ok(())
}

#[test]
fn test_add_vars() -> Result<(), Error> {
    let mut scope = Env::default();
    let loc_id = LocId::from("foo");
    let id = MetaId::from(loc_id);
    let expr = ast::Expr::Id(loc_id);
    scope.add_local_vars(&expr, None)?;
    assert_ron_snapshot!(scope, @r#"
    Env(
      env: TypeMap(
        inner: {
          Key(
            id: "foo",
            kind: Kind("Variable"),
          ): Value(
            expr: Variable(Variable(
              id: MetaId("foo"),
            )),
          ),
        },
      ),
    )
    "#);
    assert_ron_snapshot!(scope.lookup(&id, Kind::Variable)?, @r#"
    Variable(Variable(
      id: MetaId("foo"),
    ))
    "#);
    Ok(())
}

#[test]
fn test_make_unique() -> Result<(), Error> {
    let mut env = Env::default();
    let span = Span::default();
    let foo = MetaId::from("foo");
    let foo_var = Variable { id: foo };
    let foo_expr = ExprPtr::new(Expr::Variable(foo_var), span);
    env.insert(foo, foo_expr, Kind::Variable);
    let bar = MetaId::from("bar");
    let bar_var = Variable { id: bar };
    let bar_expr = ExprPtr::new(Expr::Variable(bar_var), span);
    env.insert(bar, bar_expr, Kind::Variable);
    assert_ron_snapshot!(env, @r#"
    Env(
      env: TypeMap(
        inner: {
          Key(
            id: "foo",
            kind: Kind("Variable"),
          ): Value(
            expr: Variable(Variable(
              id: MetaId("foo"),
            )),
          ),
          Key(
            id: "bar",
            kind: Kind("Variable"),
          ): Value(
            expr: Variable(Variable(
              id: MetaId("bar"),
            )),
          ),
        },
      ),
    )
    "#);
    reset_unique_id_counter();
    env.make_unique()?;
    assert_ron_snapshot!(env, @r#"
    Env(
      env: TypeMap(
        inner: {
          Key(
            id: "foo0",
            kind: Kind("Variable"),
          ): Value(
            expr: Variable(Variable(
              id: MetaId("foo0"),
            )),
          ),
          Key(
            id: "bar1",
            kind: Kind("Variable"),
          ): Value(
            expr: Variable(Variable(
              id: MetaId("bar1"),
            )),
          ),
        },
      ),
    )
    "#);
    Ok(())
}

#[test]
fn test_merge() -> Result<(), Error> {
    fn env() -> Env {
        let mut env = Env::default();
        let span = Span::default();
        let foo = MetaId::from("foo");
        let foo_var = Variable { id: foo };
        let foo_expr = ExprPtr::new(Expr::Variable(foo_var), span);
        env.insert(foo, foo_expr, Kind::Variable);
        let bar = MetaId::from("bar");
        let bar_var = Variable { id: bar };
        let bar_expr = ExprPtr::new(Expr::Variable(bar_var), span);
        env.insert(bar, bar_expr, Kind::Variable);
        env
    }
    reset_unique_id_counter();
    let mut env1 = env();
    env1.make_unique()?;
    let mut env2 = env();
    env2.make_unique()?;
    env1.merge(&env2);
    assert_ron_snapshot!(env1, @r#"
    Env(
      env: TypeMap(
        inner: {
          Key(
            id: "foo0",
            kind: Kind("Variable"),
          ): Value(
            expr: Variable(Variable(
              id: MetaId("foo0"),
            )),
          ),
          Key(
            id: "bar1",
            kind: Kind("Variable"),
          ): Value(
            expr: Variable(Variable(
              id: MetaId("bar1"),
            )),
          ),
          Key(
            id: "foo2",
            kind: Kind("Variable"),
          ): Value(
            expr: Variable(Variable(
              id: MetaId("foo2"),
            )),
          ),
          Key(
            id: "bar3",
            kind: Kind("Variable"),
          ): Value(
            expr: Variable(Variable(
              id: MetaId("bar3"),
            )),
          ),
        },
      ),
    )
    "#);
    Ok(())
}
