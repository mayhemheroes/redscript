
struct Dummy {
  let a: Int32;
  let b: String;
}

struct Tuple<A, B> {
  let first: A;
  let second: B;
}

func Test() {
  let a = new Dummy(1, "a");
  let b = new Dummy(2, "b");

  let c = new Tuple<Int32, String>(3, "c");
  let d = new Tuple(4, "d");
}
