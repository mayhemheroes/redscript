public enum Ordering {
  Less = -1,
  Equal = 0,
  Greater = 1,
}

public abstract class Comparator<A> {
  public func Compare(lhs: A, rhs: A) -> Ordering;

  public func On<B>(f: (B) -> A) -> Comparator<B> {
    return ComparatorOn.New(this, f);
  }
}

@deriveNew()
final class ComparatorOn<A, B> extends Comparator<B> {
  let inner: Comparator<A>;
  let f: (B) -> A;

  func Compare(lhs: B, rhs: B) -> Ordering {
    return this.inner.Compare(this.f(lhs), this.f(rhs));
  }
}
