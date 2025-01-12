
struct Struct {}

class ClassExtendingStruct extends Struct {}

abstract class AbstractClass<A> {
  func UnimplementedMethod(a: A)
  final func FinalMethod(a: A) {}
}

class IncompleteClass extends AbstractClass<Int32> {}

class CompleteClass extends AbstractClass<Int32> {
  func UnimplementedMethod(a: Int32) {}
}

class ClassExtendingPrimitive extends Int32 {}

@badAnnotation()
class ClassWithUnknownAnnotation {}

class ClassDefinedTwice {}
class ClassDefinedTwice {}

class ClassWithDuplicateMethod {
  func Method(x: String) {}
  func Method(x: String) {}
}

class ClassOverridingFinalMethod extends AbstractClass<Int32> {
  func UnimplementedMethod(a: Int32) {}
  final func FinalMethod(a: Int32) {}
}

class ClassWithInvalidTypeArgCounts extends AbstractClass<Int32, Int32> {
  let field: AbstractClass;

  func Method(a: AbstractClass) -> AbstractClass<String, String> {}
}

class CircularClassA extends CircularClassB {}
class CircularClassB extends CircularClassA {}

struct StructWithNonStaticMethod {
  func Method() {}
}

class ScriptedClassWithNativeMembers {
  native let field: Int32;

  native func NativeMethod()
}

class ClassWithInvalidPersistentFields {
  persistent let string: String;
  persistent let variant: Variant;
  persistent let res: ResRef;

  persistent let int: Int32;
  persistent let float: Float;
  persistent let array: [Int32];
}

class ClassWithBadVariance<+A, -B> {
  func MethodA(a: A)
  func MethodB() -> B
}
