use bebop_compiler::hir::Expr;
use bebop_compiler::{error::*, hir::*};
use bebop_util::meta::*;
use insta::*;
use std::rc::Rc;

#[test]
fn type_env() {
    let mut env = TypeEnv::new();
    let foo = Loc::new(Ident::new("foo"), Span::default());
    let var1 = Variable {
        id: Id::from(foo.clone()),
    };
    let expr1 = Expr::Variable(var1);
    env.insert(foo, expr1, Types::Variable);
    let id2 = Loc::new(Ident::new("bar"), 0..0);
    let var2 = Variable {
        id: Id::from(id2.clone()),
    };
    env.insert(*id2.value(), Type::Variable(Rc::new(var2)));
    let id3 = Loc::new(Ident::new("baz"), 0..0);
    let var3 = Variable {
        id: Id::from(id3.clone()),
    };
    env.insert(*id3.value(), Type::Variable(Rc::new(var3)));
    assert_ron_snapshot!(env, @r#"
    "#);
}

#[test]
fn scope() -> Result<(), LiftError> {
    let mut scope = Scope::default();
    let id1 = Loc::new(Ident::new("foo"), 0..0);
    let var1 = Variable {
        id: Id::from(id1.clone()),
    };
    scope.insert(*id1.value(), Type::Variable(Rc::new(var1)));
    let id2 = Loc::new(Ident::new("bar"), 0..0);
    let var2 = Variable {
        id: Id::from(id2.clone()),
    };
    scope.insert(*id2.value(), Type::Variable(Rc::new(var2)));
    let id3 = Loc::new(Ident::new("baz"), 0..0);
    let var3 = Variable {
        id: Id::from(id3.clone()),
    };
    scope.insert(*id3.value(), Type::Variable(Rc::new(var3)));
    assert_ron_snapshot!(scope, @r#"
    "#);
    let local = scope.to_local();
    assert_ron_snapshot!(local, @r"
    Scope(
      env: [],
    )
    ");
    Ok(())
}

#[test]
fn scope_add_vars() -> Result<(), LiftError> {
    let mut scope = Scope::default();
    let id = Loc::new(Ident::new("foo"), 0..0);
    let expr = Expr::Id(id);
    scope.add_vars(&expr, None)?;
    assert_ron_snapshot!(scope, @r#"
    "#);
    Ok(())
}
