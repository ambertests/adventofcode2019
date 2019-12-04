case class Coordinate(x: Int, y: Int){

  override def equals(obj: Any): Boolean = {
    obj match {
      case c: Coordinate =>
        this.x == c.x && this.y == c.y
      case _ => false
    }
  }

  def moveRight(n:Int) = Coordinate(x + n, y)
  def moveLeft(n:Int) = Coordinate(x - n, y)
  def moveUp(n:Int) = Coordinate(x, y + n)
  def moveDown(n:Int) = Coordinate(x, y - n)

  def distance(c:Coordinate): Int = {
    Math.abs(x - c.x) + Math.abs(y - c.y)
  }
}
