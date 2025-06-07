
func TestArray(array: [Int32]) -> Int32 {
  switch array {
    case let [.., a, b, c]:
      return a;
    case let [a, b, ..]:
      return b;
    case let [a]:
      return a;
    default:
      return 0;
  }

  if let [.., a, b, c] = array {
    return a;
  } else if let [a, b, ..] = array {
    return b;
  } else if let [a] = array {
    return a;
  } else {
    return 0;
  }
}

func TestNullable(inst: Class) -> Class {
  switch inst {
    case let inst?:
      return inst;
    default:
      return null;
  }

  if let inst? = inst {
    return inst;
  } else {
    return null;
  }
}

func TestNullableWref(inst: wref<Class>) -> Class {
  switch inst {
    case let inst?:
      return inst;
    default:
      return null;
  }

  if let inst? = inst {
    return inst;
  } else {
    return null;
  }
}

func TestDestructure(inst: Class) -> Int32 {
  switch inst {
    case let SubclassA { fieldA }:
      return fieldA;
    case let SubclassB { fieldB }:
      return fieldB[0];
    default:
      return 0;
  }

  if let SubclassA { fieldA } = inst {
    return fieldA;
  } else if let SubclassB { fieldB } = inst {
    return fieldB[0];
  } else {
    return 0;
  }
}

func TestDestructureWref(inst: wref<Class>) -> Int32 {
  switch inst {
    case let SubclassA { fieldA }:
      return fieldA;
    case let SubclassB { fieldB }:
      return fieldB[0];
    default:
      return 0;
  }

  if let SubclassA { fieldA } = inst {
    return fieldA;
  } else if let SubclassB { fieldB } = inst {
    return fieldB[0];
  } else {
    return 0;
  }
}

func TestCast(inst: Class) -> Int32 {
  switch inst {
    case let a as SubclassA:
      return a.fieldA;
    case let b as SubclassB:
      return b.fieldB[0];
    default:
      return 0;
  }

  if let a as SubclassA = inst {
    return a.fieldA;
  } else if let b as SubclassB = inst {
    return b.fieldB[0];
  } else {
    return 0;
  }
}

func TestWhile(class: Class) -> Class {
  let cur = class;
  while let Class { nested: nested? } = cur {
    cur = nested;
  }
  return cur;
}

func TestWhileArray(array: [Int32]) {
  while let [.., last] = array {
    ArrayPop(array);
  }
}

abstract class Scriptable {
  func IsA(name: CName) -> Bool {}
}

class Class extends Scriptable {
  let nested: Class;
}

class SubclassA extends Class {
  let fieldA: Int32;
}

class SubclassB extends Class {
  let fieldB: [Int32];
}

native func OperatorEqual(lhs: Int32, rhs: Int32) -> Bool
native func OperatorGreaterEqual(lhs: Int32, rhs: Int32) -> Bool
native func OperatorSubtract(lhs: Int32, rhs: Int32) -> Int32
native func OperatorLogicAnd(lhs: Bool, rhs: Bool) -> Bool
