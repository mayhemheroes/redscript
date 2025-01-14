
native func Cast(i: Int32) -> Float
native func Cast(i: Int32) -> String

func Test() {
  let _: Float = Cast(1);
  let _ = Cast<String>(2);
}
