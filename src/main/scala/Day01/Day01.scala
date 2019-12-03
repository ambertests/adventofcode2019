package Day01

import scala.annotation.tailrec
import scala.io.Source

object Day01 extends App {
  var partOne: Double = 0
  var partTwo: Double = 0
  val input = Source.fromFile("src/main/scala/Day01/input.txt")

  for (line <- input.getLines()) {
    val initialFuel = Math.floor(Integer.valueOf(line)/3) - 2
    partOne += initialFuel

    @tailrec
    def totalFuel(mass: Double, total: Double): Double = {
      if (mass <= 6 ) total
      else {
        val fuel = Math.floor(mass/3) - 2
        totalFuel(fuel, total+fuel)
      }
    }

    partTwo += totalFuel(initialFuel, initialFuel)
  }

  println("Part One: " + partOne)
  println("Part Two: " + partTwo)

  input.close()
}
