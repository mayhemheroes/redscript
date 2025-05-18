
native func TakesVariant(var: Variant)

func Test() {
  let x = ToVariant(1);
  let _ = IsDefined(x);
  let y = FromVariant<Int32>(x);
  let _ = VariantTypeName(x);

  TakesVariant(2);
}
