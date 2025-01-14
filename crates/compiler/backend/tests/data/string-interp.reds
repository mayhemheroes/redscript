
func Test(year: Int32) -> String {
  let birthYear = 1990;
  let name = "John";
  return s"My name is \(name) and I am \(year - birthYear) years old";
}

native func OperatorAdd(a: script_ref<String>, b: script_ref<String>) -> String
native func OperatorSubtract(a: Int32, b: Int32) -> Int32
