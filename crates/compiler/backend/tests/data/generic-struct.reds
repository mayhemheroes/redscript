
struct Tuple<A, B> {
  let a: A;
  let b: B;

  static func Swap(self: Tuple<A, B>) -> Tuple<B, A> =
    Tuple(self.b, self.a)
}

func Test() {
  let a = Tuple(1, "a");
  let b = Tuple("b", 2);
  a = Tuple.Swap(b);
}
