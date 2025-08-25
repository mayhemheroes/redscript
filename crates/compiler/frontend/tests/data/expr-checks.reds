
native func ReturnsInt() -> Int32
native func ReturnsArray() -> [Int32]
native func HasOutParam(out x: Int32)

struct Struct {
  let x: Int32;
}

func Test() {
  ReturnsInt() = 2;

  (Struct(2)).x;

  ArraySize(ReturnsArray());
  ReturnsArray()[0] = 2;

  HasOutParam(2);
}
