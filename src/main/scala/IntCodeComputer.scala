import scala.annotation.tailrec
import scala.collection.mutable

class IntCodeComputer(val program:Array[Int],
                      val inputs:mutable.Queue[Int]) {

  val output = new StringBuilder
  var complete = false
  var currentPointer = 0

  def getParamValue(pType:Int, pPosition:Int, arr:Array[Int]): Int = {
    if(pType == 0) arr(arr(pPosition))
    else arr(pPosition)
  }

  def run(input: Int):Int = {
    val lastOutput = output.toString.toIntOption.getOrElse(0)
    output.clear()
    inputs.enqueue(input)
    val result = compute(currentPointer)
    result.toIntOption.getOrElse(lastOutput)
  }
  @tailrec
  final def compute(pointer: Int): String = {

    val opCode = program(pointer)
    val cmd = opCode % 100
    if(cmd == 99) {
      complete = true
      output.toString
    }
    else {
      val pType1 = (opCode / 100) % 10
      val pType2 = (opCode / 1000) % 10
      val param1 = getParamValue(pType1, pointer + 1, program)
      val param2 = {
        // ops 3 and 4 only take one param, so trying to get the second could cause an error
        if(cmd != 3 && cmd != 4){
          getParamValue(pType2, pointer + 2, program)
        }
        else 0
      }
      val writeLoc = {
        if(cmd == 3) program(pointer + 1)
        else if(Array(1,2,7,8).contains(cmd)) program(pointer + 3)
        else 0
      }
      var pause = false
      val newPointer = cmd match {
        case 1 => {
          program(writeLoc) = param1 + param2
          pointer + 4
        }
        case 2 => {
          program(writeLoc) = param1 * param2
          pointer + 4
        }
        case 3 => {
          if(inputs.nonEmpty) program(writeLoc) = inputs.dequeue
          else pause = true //pause and wait for new input
          pointer + 2
        }
        case 4 => {
          output.append(param1)
          pointer + 2
        }
        case 5 => {
          if(param1 != 0) param2
          else pointer + 3 //opcode + 2 parameters
        }
        case 6 => {
          if(param1 == 0) param2
          else pointer + 3
        }
        case 7 => {
          if(param1 < param2) program(writeLoc) = 1
          else program(writeLoc) = 0
          pointer + 4
        }
        case 8 => {
          if(param1 == param2) program(writeLoc) = 1
          else program(writeLoc) = 0
          pointer + 4
        }

      }
      if(pause) {
        currentPointer = pointer //we want to be able to pick up where we left off
        output.toString
      }
      else compute(newPointer)
    }
  }
}

object IntCodeComputer{
  def apply(program:Array[Int], phase:Int = -1): IntCodeComputer = {
    new IntCodeComputer(program.clone(), mutable.Queue(phase))
  }
}

