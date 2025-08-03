
func Test() -> Result<Int32, String> {
  return Ok.New(1).Map((x) -> x + 1);
}

abstract class Scriptable {
  public func IsA(name: CName) -> Bool {}
}

abstract class Result<+A, +E> extends Scriptable {
  public func Map<B>(f: (A) -> B) -> Result<B, E> {
    switch this {
      case let Ok { value }:
        return Ok.New(f(value));
      case let Err { error }:
        return Err.New(error);
    }
  }
}

class MyClass {}

class Ok<A, E> extends Result<A, E> {
  public let value: A;

  public static func New(value: A) -> Ok<A, E> {
    let self = new Ok<A, E>();
    self.value = value;
    return self;
  }
}

class Err<A, E> extends Result<A, E> {
  public let error: E;

  public static func New(error: E) -> Err<A, E> {
    let self = new Err<A, E>();
    self.error = error;
    return self;
  }
}

native func OperatorLogicAnd(lhs: Bool, rhs: Bool) -> Bool
native func OperatorAdd(lhs: Int32, rhs: Int32) -> Int32
