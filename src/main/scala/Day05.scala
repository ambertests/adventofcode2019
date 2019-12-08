import scala.annotation.tailrec

object Day05 extends AOCSolution {
  override var day = 5

  def doCompute(program:Array[Int], input:Int):String = {
    val icc = IntCodeComputer(program.clone(), input)
    icc.compute(0)
  }

  val program = getInputString.split(",").map(_.toInt)
  printPartOne(doCompute(program, 1))
  printPartTwo(doCompute(program, 5))
}
