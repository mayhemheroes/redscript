

func ValidInference() -> String {
  let f = (a) -> (b, c) -> (d) -> d(a(b), c);
  let str = f((x) -> x)(1, "")((_, y) -> y);
  return str;
}

func InferenceError() -> String {
  let f = (a) -> (b, c) -> (d) -> d(a(b), c);
  let str = f((x) -> x)("", 1)((_, y) -> y);
  return str;
}
