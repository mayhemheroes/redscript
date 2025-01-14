
struct Tuple<A, B> {
  let a: A;
  let b: B;

  static func Swap(self: Tuple<A, B>) -> Tuple<B, A> =
    new Tuple(self.b, self.a)
}

func Test() {
  let a = new Tuple(1, "a");
  let b = new Tuple("b", 2);
  a = Tuple.Swap(b);
}
