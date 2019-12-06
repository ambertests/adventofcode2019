import scala.collection.mutable.ListBuffer

object Day06 extends AOCSolution {


  override var day = 6

  def processInput(input:List[String]): Map[String, String] = {
    (for (x <- input) yield (x.split("\\)")(1) -> x.split("\\)")(0))).toMap
  }
  @scala.annotation.tailrec
  def countOrbits(orbitMap:Map[String, String], satName:String, count:Int): Int = {
    if(!orbitMap.contains(satName)) count
    else countOrbits(orbitMap, orbitMap(satName), count + 1)
  }

  def calculateOrbits(orbitMap:Map[String, String]): Int = {
    var total = 0
    for(s <- orbitMap.keys) {total += countOrbits(orbitMap, s, 0)}
    total
  }

  def minPathToSanta(orbitMap: Map[String, String]): Int = {
    val start = orbitMap("YOU")
    val end = orbitMap("SAN")

    @scala.annotation.tailrec
    def getPathToCom(s:String, path:ListBuffer[String]):List[String] = {
      if(s == "COM") path.toList
      else getPathToCom(orbitMap(s), path.addOne(s))
    }
    //first get the full path from YOU and SAN to the end point COM
    val youPath = getPathToCom(start, ListBuffer())
    val sanPath = getPathToCom(end, ListBuffer())

    //next get the first point where the two paths intersect
    val crossPoint = youPath.intersect(sanPath).head

    //the minimum path is the distance from YOU and SAN to the intersection
    youPath.indexOf(crossPoint) + sanPath.indexOf(crossPoint)
  }

  val orbitMap = processInput(getInputStrings)
  printPartOne(calculateOrbits(orbitMap))
  printPartTwo(minPathToSanta(orbitMap))
}
