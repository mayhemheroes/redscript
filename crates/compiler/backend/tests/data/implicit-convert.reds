
native func TakeScriptRef(str: script_ref<String>)
native func TakeRef(instance: Class)

class Class {
  let field: String;
  func Method() {}
}

func Test() {
  TakeScriptRef("test");

  let a: wref<Class> = new Class();
  TakeRef(a);

  a.Method();
  a.field;
}
