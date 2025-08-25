
@neverRef()
class NeverRef {}

@mixedRef()
class MixedRef {}

func Test() {
  let _ = NeverRef(); // OK
  let _ = new NeverRef(); // Error
  let _ = MixedRef(); // OK
  let _ = new MixedRef(); // OK
}
