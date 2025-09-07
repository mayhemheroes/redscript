
@neverRef()
class NeverRef {}

@mixedRef()
class MixedRef {}

func Test() {
  let _ = NeverRef(); // OK
  let _ = new NeverRef(); // Error
  let _ = MixedRef(); // OK
  let _ = new MixedRef(); // OK

  let withRef: ref<MixedRef> = new MixedRef(); // OK
  let withRef: ref<MixedRef> = MixedRef(); // Error

  let withoutRef: MixedRef = MixedRef(); // OK
  let withoutRef: MixedRef = new MixedRef(); // Error

  let val = null;
  let _: ref<MixedRef> = val as MixedRef; // OK

  IsDefined(withRef); // OK
  IsDefined(withoutRef); // OK
}
