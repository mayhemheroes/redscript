Variant is a type that represents values that can hold a set of different types. You can create such instances with the `ToVariant` intrinsic. The inner value can later be retrieved using the `FromVariant` intrinsic (as long as `FromVariant` is called with precisely the correct type).

### Example

```
let int: Int32 = 42;
let variant = ToVariant(int);
let value = FromVariant<Int32>(variant);
```
