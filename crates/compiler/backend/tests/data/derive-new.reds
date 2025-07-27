
@deriveNew()
class Pair {
  let first: String;
  let second: Int32;
}

@deriveNew()
class GenericPair<A, B> {
  let first: A;
  let second: B;
}

func Test() {
  let a = Pair.New("a", 1);
  let b = GenericPair.New("b", 2);
}
