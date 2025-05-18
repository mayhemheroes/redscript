
func Test1(val: Int32) -> Bool = val % 2 == 0 ? true : false
func Test2(val: wref<Dummy>) -> wref<Dummy> = IsDefined(val) ? val : null

class Dummy {}

native func OperatorModulo(l: Int32, r: Int32) -> Int32
native func OperatorEqual(l: Int32, r: Int32) -> Bool
