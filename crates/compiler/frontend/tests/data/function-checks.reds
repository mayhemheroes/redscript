
native func NativeFunc(x: Int32) -> Int32 // OK

native func NativeFuncWithUnexpectedBody(x: Int32) -> Int32 {
  return x;
}

func FreeFunctionWithMissingBody(x: Int32) -> Int32

final static func UnusedFinalStaticQualifiers(s: String) -> String {
  return s;
}

func FunctionWithVariance<+A>() -> A {}
