
native func OperatorModulo(l: Int32, r: Int32) -> Int32

func Test(val: Int32) -> Bool {
    switch val % 4 {
        case -1:
        case 1:
        case 2:
        case 3:
            break;
        default:
            return true;
    }
    return false;
}
