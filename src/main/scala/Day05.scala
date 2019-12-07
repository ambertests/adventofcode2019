import scala.annotation.tailrec

object Day05 extends AOCSolution {
  override var day = 5

  def doCompute(program:Array[Int], input:Int):String = {
    val icc = new IntCodeComputer(input)
    icc.compute(program.clone(), 0)
    icc.output.toString()
  }

  val program = getInputString.split(",").map(_.toInt)
  printPartOne(doCompute(program, 1))
  printPartTwo(doCompute(program, 5))
}
