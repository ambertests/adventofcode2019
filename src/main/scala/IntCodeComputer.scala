import scala.annotation.tailrec
import scala.collection.mutable

class IntCodeComputer(val program:Array[BigInt],
                      val inputs:mutable.Queue[BigInt]) {

  val output = new StringBuilder
  var complete = false
  var currentPointer = 0
  var relativeBase = 0

  def getParamValue(pType:Int, pPosition:Int, arr:Array[BigInt]): BigInt = {
    if(pType == 0) arr(arr(pPosition).intValue)
    else if (pType == 1) arr(pPosition)
    else arr(arr(pPosition + relativeBase).intValue)
  }

  def run(input: BigInt):BigInt = {
    val lastOutput = output.toString
    output.clear()
    inputs.enqueue(input)
    val result = compute(currentPointer)
    if(result.isEmpty) BigInt(lastOutput)
    else BigInt(result)
  }
  @tailrec
  final def compute(pointer: Int): String = {

    val opCode:Int = program(pointer).intValue
    val cmd:Int = opCode % 100
    if(cmd == 99) {
      complete = true
      output.toString
    }
    else {
      val pType1 = (opCode.intValue / 100) % 10
      val pType2 = (opCode.intValue / 1000) % 10
      val param1:BigInt = getParamValue(pType1, pointer + 1, program)
      val param2:BigInt = {
        // ops 3 and 4 only take one param, so trying to get the second could cause an error
        if(cmd != 3 && cmd != 4 && cmd != 9){
          getParamValue(pType2, pointer + 2, program)
        }
        else 0
      }
      val writeLoc:Int = {
        if(cmd == 3) program(pointer + 1).intValue
        else if(Array(1,2,7,8).contains(cmd)) program(pointer + 3).intValue
        else 0
      }
      var pause = false
      var newPointer:Int = pointer
      cmd match {
        case 1 => {
          program(writeLoc) = param1 + param2
          newPointer += 4
        }
        case 2 => {
          program(writeLoc) = param1 * param2
          newPointer += 4
        }
        case 3 => {
          if(inputs.nonEmpty) program(writeLoc) = inputs.dequeue
          else pause = true //pause and wait for new input
          newPointer += 2
        }
        case 4 => {
          output.append(param1)
          newPointer += 2
        }
        case 5 => {
          if(param1 != 0) newPointer = param2.intValue
          else newPointer += 3 //opcode + 2 parameters
        }
        case 6 => {
          if(param1 == 0) newPointer = param2.intValue
          else newPointer += 3
        }
        case 7 => {
          if(param1 < param2) program(writeLoc) = 1
          else program(writeLoc) = 0
          newPointer += 4
        }
        case 8 => {
          if(param1 == param2) program(writeLoc) = 1
          else program(writeLoc) = 0
          newPointer += 4
        }
          /*
          Opcode 9 adjusts the relative base by the value of its only parameter.
          The relative base increases (or decreases, if the value is negative)
          by the value of the parameter.
           */
        case 9 => {
          relativeBase += param1.intValue
          newPointer += 2
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
  def apply(program:Array[BigInt], phase:BigInt = -1): IntCodeComputer = {
    val arr:Array[BigInt] = new Array[BigInt](50000)
    program.copyToArray(arr)
    //val arr = program.clone() :+ new Array[BigInt](50000 - program.length)
    new IntCodeComputer(arr, {if(phase >= 0)mutable.Queue(phase) else mutable.Queue()})
  }
}

