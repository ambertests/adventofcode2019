import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using

object Day01 extends App {
  var partOne: Double = 0
  var partTwo: Double = 0

  def fuelForMass(mass: Double): Double = {
    Math.floor(mass/3) - 2
  }

  @tailrec
  def totalFuel(mass: Double, total: Double): Double = {
    val fuel = fuelForMass(mass)
    if (fuel <= 0) total
    else {
      totalFuel(fuel, total + fuel)
    }
  }

  Using(Source.fromResource("day01.txt")) {
    _.getLines().foreach(line => {
      val mass = Integer.valueOf(line).doubleValue()
      partOne += fuelForMass(mass)
      partTwo += totalFuel(mass, 0)
    })
  }

  println("Solution 1.1: " + partOne)
  println("Solution 1.2: " + partTwo)

}
