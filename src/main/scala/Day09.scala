object Day09 extends AOCSolution {
  override var day: Int = 9

  printPartOne(IntCodeComputer(getInputAsLongArray, 1).compute(0))
  printPartTwo(IntCodeComputer(getInputAsLongArray, 2).compute(0))

}
