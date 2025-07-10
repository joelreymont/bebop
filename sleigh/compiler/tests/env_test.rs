use bebop_sleigh_compiler::{error::*, hir::*};
use bebop_sleigh_util::meta::*;
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
    assert_ron_snapshot!(env.get(id1.value()), @r#"
    Some(Variable(Variable(
      id: Id(
        ident: Ident("foo"),
        tag: Tag(
          size: None,
          hint: None,
        ),
      ),
    )))
    "#);
    assert_ron_snapshot!(env.get(id2.value()), @r#"
    Some(Variable(Variable(
      id: Id(
        ident: Ident("bar"),
        tag: Tag(
          size: None,
          hint: None,
        ),
      ),
    )))
    "#);
    assert_ron_snapshot!(env.get(id3.value()), @r#"
    Some(Variable(Variable(
      id: Id(
        ident: Ident("baz"),
        tag: Tag(
          size: None,
          hint: None,
        ),
      ),
    )))
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
    assert_ron_snapshot!(scope.lookup(&id1)?, @r#"
    Variable(Variable(
      id: Id(
        ident: Ident("foo"),
        tag: Tag(
          size: None,
          hint: None,
        ),
      ),
    ))
    "#);
    assert_ron_snapshot!(scope.lookup(&id2)?, @r#"
    Variable(Variable(
      id: Id(
        ident: Ident("bar"),
        tag: Tag(
          size: None,
          hint: None,
        ),
      ),
    ))
    "#);
    assert_ron_snapshot!(scope.lookup(&id3)?, @r#"
    Variable(Variable(
      id: Id(
        ident: Ident("baz"),
        tag: Tag(
          size: None,
          hint: None,
        ),
      ),
    ))
    "#);
    Ok(())
}
