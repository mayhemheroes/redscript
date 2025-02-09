
class Class {}
class Subclass extends Class {}

func AcceptsClassArray(arr: [Class]) {}

func PassesSubclassArray() {
  let arr: [Subclass] = [];
  AcceptsClassArray(arr);
}
