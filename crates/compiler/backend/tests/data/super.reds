
class Base {
  protected func Testing() -> Int32 = 0
}

class Class extends Base {
  func Testing() -> Int32 = super.Testing()
}
