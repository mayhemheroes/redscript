
@intrinsic(Equals)
func Equals<A>(lhs: A, rhs: A) -> Bool

@intrinsic(NotEquals)
func NotEquals<A>(lhs: A, rhs: A) -> Bool

@intrinsic(ArrayClear)
func ArrayClear<A>(out array: [A])

@intrinsic(ArraySize)
func ArraySize<A>(array: [A]) -> Int32

@intrinsic(ArrayResize)
func ArrayResize<A>(array: [A], newSize: Int32)

@intrinsic(ArrayFindFirst)
func ArrayFindFirst<A>(array: [A], needle: A) -> Int32

@intrinsic(ArrayFindLast)
func ArrayFindLast<A>(array: [A], needle: A) -> Int32

@intrinsic(ArrayContains)
func ArrayContains<A>(array: [A], needle: A) -> Bool

@intrinsic(ArrayCount)
func ArrayCount<A>(array: [A], needle: A) -> Int32

@intrinsic(ArrayPush)
func ArrayPush<A>(array: [A], elem: A)

@intrinsic(ArrayPop)
func ArrayPop<A>(array: [A]) -> A

@intrinsic(ArrayInsert)
func ArrayInsert<A>(array: [A], index: Int32, elem: A)

@intrinsic(ArrayRemove)
func ArrayRemove<A>(array: [A], elem: A) -> Bool

@intrinsic(ArrayGrow)
func ArrayGrow<A>(array: [A], count: Int32)

@intrinsic(ArrayErase)
func ArrayErase<A>(array: [A], index: Int32)

@intrinsic(ArrayLast)
func ArrayLast<A>(array: [A]) -> A

@intrinsic(ArraySort)
func ArraySort<A>(array: [A])

@intrinsic(ToString)
func ToString<A>(a: A) -> String

@intrinsic(EnumInt)
func EnumInt<A>(enum: A) -> Int32

@intrinsic(IntEnum)
func IntEnum<A>(value: Int32) -> A

@intrinsic(ToVariant)
func ToVariant<A>(a: A) -> Variant

@intrinsic(FromVariant)
func FromVariant<A>(variant: Variant) -> A

@intrinsic(VariantIsRef)
func VariantIsRef<A>(variant: Variant) -> Bool

@intrinsic(VariantIsArray)
func VariantIsArray<A>(variant: Variant) -> Bool

@intrinsic(VariantTypeName)
func VariantTypeName(variant: Variant) -> String

@intrinsic(AsRef)
func AsRef<A>(a: A) -> script_ref<A>

@intrinsic(Deref)
func Deref<A>(a: script_ref<A>) -> A

@intrinsic(RefToWeakRef)
func RefToWeakRef<A extends IScriptable>(weak: A) -> wref<A>

@intrinsic(WeakRefToRef)
func WeakRefToRef<A>(weak: wref<A>) -> A

@intrinsic(IsDefined)
func IsDefined(a: IScriptable) -> Bool

abstract class Function0<+R> {
  public func Call() -> R
}

abstract class Function1<-A, +R> {
  public func Call(a: A) -> R

  public final func Compose<A1>(f: (A1) -> A) -> (A1) -> R =
    (a) -> this(f(a))

  public final func AndThen<R1>(f: (R) -> R1) -> (A) -> R1 =
    (a) -> f(this(a))
}

abstract class Function2<-A, -B, +R> {
  public func Call(a: A, b: B) -> R
}

abstract class Function3<-A, -B, -C, +R> {
  public func Call(a: A, b: B, c: C) -> R
}

abstract class Function4<-A, -B, -C, -D, +R> {
  public func Call(a: A, b: B, c: C, d: D) -> R
}

abstract class Function5<-A, -B, -C, -D, -E, +R> {
  public func Call(a: A, b: B, c: C, d: D, e: E) -> R
}

abstract class Function6<-A, -B, -C, -D, -E, -F, +R> {
  public func Call(a: A, b: B, c: C, d: D, e: E, f: F) -> R
}

abstract class Function7<-A, -B, -C, -D, -E, -F, -G, +R> {
  public func Call(a: A, b: B, c: C, d: D, e: E, f: F, g: G) -> R
}

abstract class Function8<-A, -B, -C, -D, -E, -F, -G, -H, +R> {
  public func Call(a: A, b: B, c: C, d: D, e: E, f: F, g: G, h: H) -> R
}
