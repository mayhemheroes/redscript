
@privateConstructor("use `PrivateConstructor.New` instead of the `new` operator")
public class PrivateConstructor {
  public static func New() -> PrivateConstructor {
    return new PrivateConstructor();
  }
}

func Test() {
  let _ = new PrivateConstructor(); // Error
  let _ = PrivateConstructor.New();
}
