
func Test() {
  if let Struct { field } = new Struct(1) {
  }

  switch [] {
    case let [a]:
    default:
  }
}

struct Struct {
  let field: Int32;
}

native func OperatorEqual(lhs: Int32, rhs: Int32) -> Bool;
