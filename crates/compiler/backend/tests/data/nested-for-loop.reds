
native func Log(str: String)
native func OperatorAssignAdd(out l: Int32, r: Int32) -> Int32
native func OperatorAdd(l: Int32, r: Int32) -> Int32
native func OperatorLess(l: Int32, r: Int32) -> Bool

func Test() {
  for i in [0, 1] {
    for j in [0, 1] {
      Log(ToString(i + j));
    }
  }
}
