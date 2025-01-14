

func ValidInference() -> Int32 {
  let f = (a) -> (b, c) -> (d) -> d(a(b), c);
  let i = f((x) -> x)(1, "")((x, _) -> x);
  return i;
}

func ValidInference() -> Int32 {
  let f = (a) -> (b, c) -> (d) -> d(a(b), c);
  let i = f((x) -> x)("", 1)((x, _) -> x);
  return i;
}
