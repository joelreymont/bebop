use bebop_compiler::{error::*, hir::*};
use bebop_util::meta::*;
use insta::*;
use std::rc::Rc;

#[test]
fn type_env() {
    let mut env = TypeEnv::new();
    let id1 = Loc::new(Ident::new("foo"), Span::empty());
    let var1 = Variable { id: Id::from(id1) };
    env.insert(*id1.value(), Type::Variable(Rc::new(var1)));
    let id2 = Loc::new(Ident::new("bar"), Span::empty());
    let var2 = Variable { id: Id::from(id2) };
    env.insert(*id2.value(), Type::Variable(Rc::new(var2)));
    let id3 = Loc::new(Ident::new("baz"), Span::empty());
    let var3 = Variable { id: Id::from(id3) };
    env.insert(*id3.value(), Type::Variable(Rc::new(var3)));
    assert_ron_snapshot!(env, @r#"
    [
      ("bar", Variable(Variable(
        id: "bar",
      ))),
      ("baz", Variable(Variable(
        id: "baz",
      ))),
      ("foo", Variable(Variable(
        id: "foo",
      ))),
    ]
    "#);
}

#[test]
fn scope() -> Result<(), LiftError> {
    let mut scope = Scope::default();
    let id1 = Loc::new(Ident::new("foo"), Span::empty());
    let var1 = Variable { id: Id::from(id1) };
    scope.insert(*id1.value(), Type::Variable(Rc::new(var1)));
    let id2 = Loc::new(Ident::new("bar"), Span::empty());
    let var2 = Variable { id: Id::from(id2) };
    scope.insert(*id2.value(), Type::Variable(Rc::new(var2)));
    let id3 = Loc::new(Ident::new("baz"), Span::empty());
    let var3 = Variable { id: Id::from(id3) };
    scope.insert(*id3.value(), Type::Variable(Rc::new(var3)));
    assert_ron_snapshot!(scope, @r#"
    Scope(
      env: [
        ("bar", Variable(Variable(
          id: "bar",
        ))),
        ("baz", Variable(Variable(
          id: "baz",
        ))),
        ("foo", Variable(Variable(
          id: "foo",
        ))),
      ],
    )
    "#);
    let local = scope.to_local();
    assert_ron_snapshot!(local, @r"
    Scope(
      env: [],
    )
    ");
    Ok(())
}
