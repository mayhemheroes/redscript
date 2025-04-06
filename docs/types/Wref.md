A weak reference is a type of reference that does not prevent the underlying value from being dropped. When no more strong references (`ref`s) exist, the value will be dropped and any remaining weak references (`wref`s) will no longer be able to access it.

### Example

```
let map: wref<inkHashMap> = new inkHashMap();
IsDefined(map); // false
```
