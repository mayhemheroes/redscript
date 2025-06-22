
/// Test two values for equality.
///
/// ### Example
/// ```
/// Equals(n"A", n"B"); // false
/// Equals(n"A", n"A"); // true
/// ```
@intrinsic(Equals)
func Equals<A>(lhs: A, rhs: A) -> Bool

/// Test two values for inequality.
///
/// ### Example
/// ```
/// NotEquals(n"A", n"B"); // true
/// NotEquals(n"A", n"A"); // false
/// ```
@intrinsic(NotEquals)
func NotEquals<A>(lhs: A, rhs: A) -> Bool

/// Empties an array.
///
/// ### Example
/// ```
/// let arr = [1, 2, 3];
/// ArrayClear(arr); // []
/// ```
@intrinsic(ArrayClear)
func ArrayClear<A>(out array: [A])

/// Returns the size of an array.
///
/// ### Example
/// ```
/// let arr = [1, 2, 3];
/// ArraySize(arr); // 3
/// ```
@intrinsic(ArraySize)
func ArraySize<A>(out array: [A]) -> Int32

/// Resizes an array.
///
/// ### Example
/// ```
/// let arr = [1, 2, 3];
/// ArrayResize(arr, 5); // [1, 2, 3, 0, 0]
/// ```
@intrinsic(ArrayResize)
func ArrayResize<A>(out array: [A], newSize: Int32)

/// Finds the first occurrence of an element in an array.
///
/// ### Example
/// ```
/// let arr = [1, 2, 3];
/// ArrayFindFirst(arr, 2); // 1
/// ```
@intrinsic(ArrayFindFirst)
func ArrayFindFirst<A>(out array: [A], needle: A) -> Int32

/// Finds the last occurrence of an element in an array.
///
/// ### Example
/// ```
/// let arr = [1, 2, 3, 2];
/// ArrayFindLast(arr, 2); // 3
/// ```
@intrinsic(ArrayFindLast)
func ArrayFindLast<A>(out array: [A], needle: A) -> Int32

/// Checks if an element exists in an array.
///
/// ### Example
/// ```
/// let arr = [1, 2, 3];
/// ArrayContains(arr, 2); // true
/// ```
@intrinsic(ArrayContains)
func ArrayContains<A>(out array: [A], needle: A) -> Bool

/// Counts the number of occurrences of an element in an array.
///
/// ### Example
/// ```
/// let arr = [1, 2, 3, 2];
/// ArrayCount(arr, 2); // 2
/// ```
@intrinsic(ArrayCount)
func ArrayCount<A>(out array: [A], needle: A) -> Int32

/// Pushes an element onto the end of an array.
///
/// ### Example
/// ```
/// let arr = [1, 2, 3];
/// ArrayPush(arr, 4); // [1, 2, 3, 4]
/// ```
@intrinsic(ArrayPush)
func ArrayPush<A>(out array: [A], elem: A)

/// Pops an element from the end of an array.
///
/// ### Example
/// ```
/// let arr = [1, 2, 3];
/// ArrayPop(arr); // 3
/// ```
@intrinsic(ArrayPop)
func ArrayPop<A>(out array: [A]) -> A

/// Inserts an element at a specific index in an array.
///
/// ### Example
/// ```
/// let arr = [1, 2, 3];
/// ArrayInsert(arr, 1, 4); // [1, 4, 2, 3]
/// ```
@intrinsic(ArrayInsert)
func ArrayInsert<A>(out array: [A], index: Int32, elem: A)

/// Removes an element from an array.
///
/// ### Example
/// ```
/// let arr = [1, 2, 3];
/// ArrayRemove(arr, 2); // [1, 3]
/// ```
@intrinsic(ArrayRemove)
func ArrayRemove<A>(out array: [A], elem: A) -> Bool

/// Grows an array by a specified number of elements.
///
/// ### Example
/// ```
/// let arr = [1, 2, 3];
/// ArrayGrow(arr, 2); // [1, 2, 3, 0, 0]
/// ```
@intrinsic(ArrayGrow)
func ArrayGrow<A>(out array: [A], count: Int32)

/// Erases an element at a specific index in an array.
///
/// ### Example
/// ```
/// let arr = [1, 2, 3];
/// ArrayErase(arr, 1); // [1, 3]
/// ```
@intrinsic(ArrayErase)
func ArrayErase<A>(out array: [A], index: Int32)

/// Returns the last element of an array.
///
/// ### Example
/// ```
/// let arr = [1, 2, 3];
/// ArrayLast(arr); // 3
/// ```
@intrinsic(ArrayLast)
func ArrayLast<A>(out array: [A]) -> A

/// Sorts an array in ascending order.
///
/// ### Example
/// ```
/// let arr = [3, 1, 2];
/// ArraySort(arr); // [1, 2, 3]
/// ```
@intrinsic(ArraySort)
func ArraySort<A>(out array: [A])

/// Converts a value to its string representation.
///
/// ### Example
/// ```
/// let num = 42;
/// ToString(num); // "42"
/// ```
@intrinsic(ToString)
func ToString<A>(a: A) -> String

/// Converts an enum value to its integer representation.
///
/// ### Example
/// ```
/// EnumInt(Color.Red); // 0
/// ```
@intrinsic(EnumInt)
func EnumInt<A>(enum: A) -> Int32

/// Converts an integer value to its enum representation.
///
/// ### Example
/// ```
/// IntEnum<Color>(0); // Color.Red
/// ```
@intrinsic(IntEnum)
func IntEnum<A>(value: Int32) -> A

/// Converts an integer value to its enum representation.
///
/// ### Example
/// ```
/// IntEnum<Color>(0); // Color.Red
/// ```
@intrinsic(LongIntEnum)
func IntEnum<A>(value: Int64) -> A

/// Converts a value into a variant.
@intrinsic(ToVariant)
func ToVariant<A>(a: A) -> Variant

/// Converts a variant into a value.
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

/// Tests if a value is defined.
///
/// ### Example
/// ```
/// let player = new PlayerPuppet();
/// IsDefined(player); // true
/// IsDefined(null); // false
/// ```
@intrinsic(IsDefined)
func IsDefined(a: IScriptable) -> Bool

/// Tests if a variant is defined.
///
/// ### Example
/// ```
/// let variant = ToVariant(1);
/// IsDefined(variant); // true
/// ```
@intrinsic(VariantIsDefined)
func IsDefined(a: Variant) -> Bool

/// Returns the fully qualified name of a type including its full module path.
///
/// ### Example
/// ```
/// NameOf<Int32>() // "Int32"
/// ```
@intrinsic(NameOf)
func NameOf<A>() -> CName

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
