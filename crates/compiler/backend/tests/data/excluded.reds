
@if(ModuleExists("NonExisting"))
func ExcludedFunc1() {}

@if(false && true)
func ExcludedFunc2() {}

@if(true)
func IncludedFunc() {}


@if(false)
@if(true)
class ExcludedClass {
  func ExcludedMethod() {}
}

@if(true)
class IncludedClass {
  func IncludedMethod() {}
}
