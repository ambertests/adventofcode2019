import org.scalatest.FunSpec

class Day09Test extends FunSpec {
/*
109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99 takes no input and produces a copy of itself as output.
1102,34915192,34915192,7,4,7,99,0 should output a 16-digit number.
104,1125899906842624,99 should output the large number in the middle.
 */
  describe("IntCode Updates"){
    it("can make a copy of itself"){
      val program = Array(109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99).map(x => BigInt(x))
      val icc = IntCodeComputer(program)
      println(icc.compute(0))
    }
    it("handles large number multiplication"){
      val program = Array(1102,34915192,34915192,7,4,7,99,0 ).map(x => BigInt(x))
      val icc = IntCodeComputer(program)
      assert(icc.compute(0).length == 16)
    }
    it("handles large number output"){
      val program = Array(104,1125899906842624L,99).map(x => BigInt(x))
      val icc = IntCodeComputer(program)
      assert(icc.compute(0) == "1125899906842624")
    }
  }
}
