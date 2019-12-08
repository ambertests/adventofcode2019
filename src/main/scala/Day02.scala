import scala.annotation.tailrec
import util.control.Breaks._

object Day02 extends AOCSolution {

  override var day: Int = 2

  def processInput(str:String): Array[Int] = {
    str.split(",").map(_.toInt)
  }


  def computeArray(arr:Array[Int]): Array[Int] = {
    val icc = IntCodeComputer(arr)
    icc.compute(0)
    icc.program
  }


  val orig:Array[Int] = processInput(getInputString)

  def checkInputs(noun: Int, verb: Int):Int = {
    val arr = orig.clone
    arr(1) = noun
    arr(2) = verb
    computeArray(arr)(0)
  }

  printPartOne(checkInputs(12, 2))

  breakable {
    0 to 99 foreach { n =>
      0 to 99 foreach { v =>
        val output = checkInputs(n, v)
        if (output == 19690720) {
          printPartTwo(100 * n + v)
          break
        }
      }
    }
  }

}
