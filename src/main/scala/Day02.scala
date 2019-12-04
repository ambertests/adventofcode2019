import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using
import util.control.Breaks._

object Day02 extends App {

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

  val orig:Array[Int] = processInput(Using(Source.fromResource("day02.txt")){ _.mkString}.get)

  def checkInputs(noun: Int, verb: Int):Int = {
    val arr = orig.clone
    arr(1) = noun
    arr(2) = verb
    computeArray(0, arr)(0)
  }

  /*
  before running the program, replace position 1 with the value 12 and replace position 2 with the value 2
   */
  println("Solution 2.1: " + checkInputs(12, 2))

  breakable {
    0 to 99 foreach { n =>
      0 to 99 foreach { v =>
        val output = checkInputs(n, v)
        if (output == 19690720) {
          println("Solution 2.2: " + (100 * n + v))
          break
        }
      }
    }
  }

}
