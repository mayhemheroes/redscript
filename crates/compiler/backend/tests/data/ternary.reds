
func Test(val: Int32) -> Bool = val % 2 == 0 ? true : false

native func OperatorModulo(l: Int32, r: Int32) -> Int32
native func OperatorEqual(l: Int32, r: Int32) -> Bool
