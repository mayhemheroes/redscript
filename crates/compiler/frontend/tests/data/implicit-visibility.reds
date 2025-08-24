
class Parent {
  public func Public() {}
  protected func Protected() {}
  private func Private() {}
}

class ChildWithPrivate extends Parent {
  private func Public() {} // Error
  private func Protected() {} // Error
  private func Private() {} // OK
}

class ChildWithProtected extends Parent {
  protected func Public() {} // Error
  protected func Protected() {} // OK
  protected func Private() {} // OK
}


class ChildWithPublic extends Parent {
  public func Public() {} // OK
  public func Protected() {} // OK
  public func Private() {} // OK
}

class ChildWithImplicit extends Parent {
  func Public() {} // OK
  func Protected() {} // OK
  func Private() {} // OK
}

func Testing() {
  let class = new ChildWithImplicit();
  class.Public();
  class.Protected(); // Error
  class.Private(); // Error
}
