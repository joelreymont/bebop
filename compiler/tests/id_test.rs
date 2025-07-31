use bebop_util::id::{Id, MetaId};
use insta::assert_ron_snapshot;

#[test]
fn test_rename() {
    let mut old = MetaId::from("foo");
    let new = Id::new("bar");
    old.rename(&new);
    assert_ron_snapshot!(old, @r#"MetaId("bar")"#);
}
