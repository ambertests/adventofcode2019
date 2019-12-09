import scala.annotation.tailrec

object Day05 extends AOCSolution {
  override var day = 5

  def doCompute(program:Array[BigInt], input:Int):String = {
    val icc = IntCodeComputer(program.clone(), input)
    icc.compute(0)
  }

  val program = getInputAsBigIntArray
  printPartOne(doCompute(program, 1))
  printPartTwo(doCompute(program, 5))
}
