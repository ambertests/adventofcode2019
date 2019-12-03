import scala.collection.mutable._
import scala.io.Source
import scala.util.Using

object Day03 extends App {

  case class Coordinate(x: Int, y: Int){
    override def equals(obj: Any): Boolean = {
      obj match {
        case c: Coordinate =>
          this.x == c.x && this.y == c.y
        case _ => false
      }
    }
  }


  def manhattan(pos1: Coordinate, pos2: Coordinate): Int = {
    Math.abs(pos1.x - pos2.x) + Math.abs(pos1.y - pos2.y)
  }

  def getPath(pos: Coordinate, cmd: String) = {
    val dir = cmd.slice(0,1)
    val num = cmd.slice(1, cmd.length).toInt
    dir match {
      case "R" => for(n <- 1 to num) yield Coordinate(pos.x + n, pos.y)
      case "L" => for(n <- 1 to num) yield Coordinate(pos.x - n, pos.y)
      case "U" => for(n <- 1 to num) yield Coordinate(pos.x, pos.y + n)
      case "D" => for(n <- 1 to num) yield Coordinate(pos.x, pos.y - n)
    }
  }

  def processLine(str: String): List[Coordinate] = {
    val list = ListBuffer(Coordinate(0,0))
    str.split(",").foreach(cmd => {
      list.addAll(getPath(list.last, cmd))
    })
    //don't want to include the (0,0) origin point
    list.slice(1, list.size).toList
  }

  def getClosestIntersectionDistance(list1: List[Coordinate], list2: List[Coordinate]): Int = {
    var minDistance = Int.MaxValue
    list1.intersect(list2).foreach(c => {
      val d = manhattan(Coordinate(0,0), c)
      if (d < minDistance) minDistance = d
    })
    minDistance
  }

  def getShortestIntersectionPath(list1: List[Coordinate], list2: List[Coordinate]): Int = {
    var minPath = Int.MaxValue
    list1.intersect(list2).foreach(c => {
      val combinedPath = list1.indexOf(c) + list2.indexOf(c) + 2
      if (combinedPath < minPath) minPath = combinedPath
    })
    minPath
  }

  val lines = Using(Source.fromResource("day03.txt")){_.getLines().toList}.get
  val wire1 = processLine(lines(0))
  val wire2 = processLine(lines(1))
  println("Part 1: " + getClosestIntersectionDistance(wire1, wire2))
  println("Part 2: " + getShortestIntersectionPath(wire1, wire2))



}
