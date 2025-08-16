
func Test() {
   let self = new Dummy();
   self.TakesSelf();
   self.TakesScriptRef();
}

struct Dummy {
  public static func TakesSelf(self: Dummy) {}

  public static func TakesScriptRef(self: script_ref<Dummy>) {}
}
