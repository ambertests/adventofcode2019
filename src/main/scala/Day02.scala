import scala.annotation.tailrec
import util.control.Breaks._

object Day02 extends AOCSolution {

  override var day: Int = 2

  def processInput(str:String): Array[Int] = {
    str.split(",").map(_.toInt)
  }

  @tailrec
  def computeArray(start: Integer, arr:Array[Int]): Array[Int] = {
    val cmd = arr(start)
    if(cmd == 99) arr
    else {
      val index1 = arr(start + 1)
      val index2 = arr(start + 2)
      val loc = arr(start + 3)
      if (cmd == 1) {
        arr(loc) = arr(index1) + arr(index2)
      } else if (cmd == 2) {
        arr(loc) = arr(index1) * arr(index2)
      }
      computeArray(start + 4, arr)
    }
  }


  val orig:Array[Int] = processInput(getInputString)

  def checkInputs(noun: Int, verb: Int):Int = {
    val arr = orig.clone
    arr(1) = noun
    arr(2) = verb
    computeArray(0, arr)(0)
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
