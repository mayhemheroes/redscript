
class Class {
  private let privateField: Int32;
  let field: Int32;
  protected let protectedField: Int32;
  public let publicField: Int32;

  private func PrivateMethod() {}
  func Method() {}
  protected func ProtectedMethod() {}
  public func PublicMethod() {}

  private static func PrivateStaticMethod() {}
  public static func PublicStaticMethod() {}

  func Testing() {
    let _ = this.privateField; // OK, private field can be accessed within the class
    let _ = this.field; // OK, field should be private by default
    let _ = this.protectedField; // OK, protected field can be accessed within the class
    let _ = this.publicField; // OK, public field can be accessed within the class

    this.PrivateMethod(); // OK, private method can be accessed within the class
    this.Method(); // OK, method should be private by default
    this.ProtectedMethod(); // OK, protected method can be accessed within the class
    this.PublicMethod(); // OK, public method can be accessed within the class

    Class.PrivateStaticMethod(); // OK, private static method can be accessed within the class
    Class.PublicStaticMethod(); // OK, public static method can be accessed within the class
  }
}

class Subclass extends Class {
  func Testing() {
    let _ = this.privateField; // Error: private field cannot be accessed from subclass
    let _ = this.field; // Error: field should be private by default
    let _ = this.protectedField; // OK: protected field can be accessed from subclass
    let _ = this.publicField; // OK: public field can be accessed from subclass

    this.PrivateMethod(); // Error: private method cannot be accessed from subclass
    this.Method(); // Error: method should be private by default
    this.ProtectedMethod(); // OK, protected method can be accessed from subclass
    this.PublicMethod(); // OK, public method can be accessed from subclass

    Class.PrivateStaticMethod(); // Error: private static method cannot be accessed from subclass
    Class.PublicStaticMethod(); // OK: public static method can be accessed from subclass
  }
}


func Testing() {
  let inst = new Class();
  let _ = inst.privateField; // Error: private field cannot be accessed outside the class
  let _ = inst.field; // Error: field should be private by default
  let _ = inst.protectedField; // Error: protected field cannot be accessed outside the class
  let _ = inst.publicField; // OK: public field can be accessed outside the class

  inst.PrivateMethod(); // Error: private method cannot be accessed outside the class
  inst.Method(); // Error: method should be private by default
  inst.ProtectedMethod(); // Error: protected method cannot be accessed outside the class
  inst.PublicMethod(); // OK: public method can be accessed outside the class

  Class.PrivateStaticMethod(); // Error: private static method cannot be accessed outside the class
  Class.PublicStaticMethod(); // OK: public static method can be accessed outside the class
}
