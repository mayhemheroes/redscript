
class Base {}

class Store<T extends Base> {
  let m_data: T;

  public static func CreateOne() -> Store<T> {
    let self = new Store<T>();
    return self;
  }
}
