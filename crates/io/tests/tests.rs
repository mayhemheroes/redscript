use redscript_io::{Definition, ScriptBundle, Type, TypeKind};

#[test]
fn roundtrip_def() {
    let mut bundle = ScriptBundle::default();
    let name = bundle.cnames_mut().add("whatever");
    let typ = Type::new(name, TypeKind::Class);
    let idx = bundle.define(typ.clone());

    assert_eq!(bundle[idx], typ);
}

#[test]
fn roundtrip_encode() {
    let mut bundle = ScriptBundle::default();
    let name = bundle.cnames_mut().add("whatever");
    let typ = Type::new(name, TypeKind::Class);
    bundle.define(typ.clone());

    let bytes = bundle.into_writeable().to_bytes().unwrap();
    let bundle = ScriptBundle::from_bytes(&bytes).unwrap();
    let defs = bundle.definitions().cloned().collect::<Vec<_>>();
    assert_eq!(&defs, &[Definition::Type(typ)]);
}
