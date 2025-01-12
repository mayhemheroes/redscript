

class UserClass {
  func HelloWorld(x: String) {}
}


@wrapMethod(UserClass)
func HelloWorld(x: String) {
}

@wrapMethod(NonExistingClass)
func HelloWorld(x: String) {
}

@addMethod(IScriptable)
func ForbiddenGenericAnnotatedMethod<A>(a: A) {
}
