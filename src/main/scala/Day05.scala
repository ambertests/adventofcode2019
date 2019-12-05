import scala.annotation.tailrec

object Day05 extends AOCSolution {
  override var day = 5

  val input = 1
  val output = new StringBuilder()

  def doAddition(opCode:Int, param1:Int, param2:Int, loc:Int, arr:Array[Int]):Unit = {
    val pType1 = (opCode / 100) % 10
    val pType2 = (opCode / 1000) % 10
    val i1 = {
      if(pType1 == 0) arr(param1)
      else param1
    }
    val i2 = {
      if (pType2 == 0) arr(param2)
      else param2
    }
    arr(loc) = i1 + i2
  }
  def doMultiplication(opCode:Int, param1:Int, param2:Int, loc:Int, arr:Array[Int]):Unit = {
    val pType1 = (opCode / 100) % 10
    val pType2 = (opCode / 1000) % 10
    val i1 = {
      if(pType1 == 0) arr(param1)
      else param1
    }
    val i2 = {
      if (pType2 == 0) arr(param2)
      else param2
    }
    arr(loc) = i1 * i2
  }
  @tailrec
  def computeArray(arr:Array[Int], pos: Integer): Array[Int] = {
    /*
    Opcode 3 takes a single integer as input and saves it to the address given by its only parameter. For example, the instruction 3,50 would take an input value and store it at address 50.
    Opcode 4 outputs the value of its only parameter. For example, the instruction 4,50 would output the value at address 50.
     */
    val opCode = arr(pos)
    val cmd = opCode % 100
    if(cmd == 99) arr
    else {
      cmd match {
        case 1 => doAddition(opCode, arr(pos + 1), arr(pos + 2), arr(pos + 3), arr)
        case 2 => doMultiplication(opCode, arr(pos + 1), arr(pos + 2), arr(pos + 3), arr)
        case 3 => arr(arr(pos + 1)) = input
        case 4 => {
          if((opCode / 100) % 10 == 0) output.append(arr(arr(pos + 1)))
          else output.append(arr(pos + 1))
        }
      }


      val offset = {
        if(cmd < 3) 4
        else 2
      }
      computeArray(arr, pos + offset)
    }
  }


  val program = getInputString
  computeArray(Day02.processInput(program), 0)
  println(output.toString())
}
