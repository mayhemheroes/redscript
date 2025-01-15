
native func Log(str: String)
native func OperatorAssignAdd(out l: Int32, r: Int32) -> Int32
native func OperatorLess(l: Int32, r: Int32) -> Bool
native func OperatorEqual(l: Int32, r: Int32) -> Bool

func Test() {
  for i in [0, 1] {
    if i == 0 {
      break;
    }
  }
}
