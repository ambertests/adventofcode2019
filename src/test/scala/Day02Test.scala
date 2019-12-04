import org.scalatest.FunSpec

class Day02Test extends FunSpec {
  describe("Input processor"){
    it("turns a comma-delimited string into an array of ints"){
      assert(Day02.processInput("1,2,3") sameElements  Array(1,2,3))
    }
  }

  describe("Array computer"){
    it("handles addition correctly"){
      val result = Day02.computeArray(0, Array(1,0,0,0,99))
      assert(result sameElements Array(2,0,0,0,99))
    }
    it("handles multiplication correctly"){
      val result = Day02.computeArray(0, Array(2,3,0,3,99))
      assert(result sameElements Array(2,3,0,6,99))
    }
    it("handles array with values beyond 99"){
      assert(Day02.computeArray(0, Array(2,4,4,5,99,0)) sameElements Array(2,4,4,5,99,9801))
    }
    it("handles array with multiple command sequences"){
      assert(Day02.computeArray(0, Array(1,1,1,4,99,5,6,0,99)) sameElements Array(30,1,1,4,2,5,6,0,99))
    }
    it("handles the original example"){
      assert(Day02.computeArray(0, Array(1,9,10,3,2,3,11,0,99,30,40,50)) sameElements
        Array(3500,9,10,70,
        2,3,11,0,
        99,
        30,40,50))
    }

  }

}