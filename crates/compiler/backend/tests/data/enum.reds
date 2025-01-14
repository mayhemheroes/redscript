
enum Direction {
  Left = 0,
  Right = 1,
}

func Testing(dir: Direction) -> Int32 {
  switch dir {
    case Direction.Left:
      return EnumInt(dir);
    case Direction.Right:
      return EnumInt(dir);
  }
}
