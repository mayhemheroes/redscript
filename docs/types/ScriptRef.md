A reference to a script value. This type allows passing certain values by reference when they would otherwise be copied like arrays and strings. Script references can be created using the `AsRef` intrinsic function.

### Example

```
let myArray = [1, 2, 3];
let myRef = AsRef(myArray);
```
