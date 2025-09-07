
class ClassA {}

class ClassB extends ClassA {}

class ClassC {}

func Test() {
  let a = new ClassA();
  let b = new ClassB();

  let _ = a as ClassA; // OK
  let _ = a as ClassB; // OK
  let _ = a as ClassC; // Impossible
  let _ = b as ClassA; // Redundant
  let _ = b as Int32; // Invalid
}
