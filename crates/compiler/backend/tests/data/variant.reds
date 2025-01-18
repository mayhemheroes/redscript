
native func TakesVariant(var: Variant)

func Test() {
  let x = ToVariant(1);
  let y = FromVariant<Int32>(x);
  let _ = VariantTypeName(x);

  TakesVariant(2);
}
