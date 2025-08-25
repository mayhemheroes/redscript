
@neverRef()
class NeverRef {}

@mixedRef()
class MixedRef {}

func Test() {
  let _ = NeverRef();
  let _ = MixedRef();
  let _ = new MixedRef();
}
