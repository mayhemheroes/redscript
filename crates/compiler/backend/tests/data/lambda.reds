
func Test() {
  let a = 1;
  let f = (b: Int32) -> (c: Int32) -> a + b + c;
}

native func OperatorAdd(a: Int32, b: Int32) -> Int32
