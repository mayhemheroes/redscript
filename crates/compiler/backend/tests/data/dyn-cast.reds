
class Base {}

class Class extends Base {}

func Test() {
  let x: Base = new Class();
  x as Class;
  let y: wref<Base> = x;
  y as Class;
}
