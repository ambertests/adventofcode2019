import scala.annotation.tailrec

class IntCodeComputer(input:Int = -1) {

  val output = new StringBuilder

  def getParamValue(pType:Int, pPosition:Int, arr:Array[Int]): Int = {
    if(pType == 0) arr(arr(pPosition))
    else arr(pPosition)
  }
  @tailrec
  final def compute(arr:Array[Int], pos: Integer): Array[Int] = {

    val opCode = arr(pos)
    val cmd = opCode % 100
    if(cmd == 99) arr
    else {
      val pType1 = (opCode / 100) % 10
      val pType2 = (opCode / 1000) % 10
      val param1 = getParamValue(pType1, pos + 1, arr)
      val param2 = {
        // ops 3 and 4 only take one param, so trying to get the second could cause an error
        if(cmd != 3 && cmd != 4){
          getParamValue(pType2, pos + 2, arr)
        }
        else 0
      }
      val writeLoc = {
        if(cmd == 3) arr(pos + 1)
        else if(Array(1,2,7,8).contains(cmd)) arr(pos + 3)
        else 0
      }
      val newPos = cmd match {
        case 1 => {
          arr(writeLoc) = param1 + param2
          pos + 4
        }
        case 2 => {
          arr(writeLoc) = param1 * param2
          pos + 4
        }
        case 3 => {
          arr(writeLoc) = input
          pos + 2
        }
        case 4 => {
          output.append(param1)
          pos + 2
        }
        case 5 => {
          if(param1 != 0) param2
          else pos + 3 //opcode + 2 parameters
        }
        case 6 => {
          if(param1 == 0) param2
          else pos + 3
        }
        case 7 => {
          if(param1 < param2) arr(writeLoc) = 1
          else arr(writeLoc) = 0
          pos + 4
        }
        case 8 => {
          if(param1 == param2) arr(writeLoc) = 1
          else arr(writeLoc) = 0
          pos + 4
        }

      }
      compute(arr, newPos)
    }
  }

}
