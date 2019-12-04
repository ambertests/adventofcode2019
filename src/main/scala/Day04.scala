import util.control.Breaks._

object Day04 extends AOCSolution {
  override var day = 4

  def isValid(i:Int, part2:Boolean = false): Boolean = {
    val nums = for (n <- i.toString.toCharArray) yield n - '0' //offset the ascii code with the value for '0' (48)
    var hasDouble = false
    var noDecrease = true
    breakable {
      for (a <- nums.indices) {
        if (a > 0 && nums(a) < nums(a - 1)) {
          noDecrease = false
          break
        }
        if(a < nums.length - 1 && nums(a) == nums(a+1)) {
          if(part2){
            //if it's a double, check before and after to make sure it is only two
            if((a >= nums.length - 2 || nums(a+1) != nums(a+2)) &&
              (a < 1 || nums(a-1) != nums(a))) hasDouble = true
          }
          else hasDouble = true
        }
      }
    }
    hasDouble & noDecrease
  }

  val input = getInputString
  val lower = input.split("-")(0).toInt
  val upper = input.split("-")(1).toInt

  var valid = 0
  var valid2 = 0
  for (num <- lower to upper){
    if(isValid(num)) valid += 1
    if(isValid(num, part2 = true)) valid2 += 1
  }
  printPartOne(valid)
  printPartTwo(valid2)

}
