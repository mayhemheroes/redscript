
func Test() {
  let _ = NameOf<Int32>();
  // legacy syntax
  let _ = NameOf(Int32);
  Generic<Int32>();
}

func Generic<A>() {
  let _ = NameOf<A>();
}
