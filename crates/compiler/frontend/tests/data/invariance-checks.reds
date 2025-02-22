
class Class {}

class Subclass extends Class {
  let children: [Class];
}

func AcceptsClassArray(arr: [Class]) {}

func PassesSubclassArray() {
  let arr: [Subclass] = [];
  AcceptsClassArray(arr);

  let scriptables: [IScriptable];
  let sub: Subclass;
  ArrayPush(scriptables, sub);
  ArrayPush(scriptables, sub.children[0]);
}
