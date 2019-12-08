object Day07 extends AOCSolution {
  override var day = 7

  def amplify(program: Array[Int], phases: Array[Int]):Int = {
    var output = 0
    phases.foreach(phase => {
      val icc = IntCodeComputer(program, phase)
      icc.inputs.enqueue(output)
      output = icc.compute(0).toInt
    })
    output
  }
  def findMaxAmp(program: Array[Int]): Int = {
    List(0,1,2,3,4).permutations.map(p => amplify(program, p.toArray)).max
  }
  def amplifyFeedbackLoop(program:Array[Int], phases:Array[Int]):Int = {
    val A = IntCodeComputer(program, phases(0))
    val B = IntCodeComputer(program, phases(1))
    val C = IntCodeComputer(program, phases(2))
    val D = IntCodeComputer(program, phases(3))
    val E = IntCodeComputer(program, phases(4))
    var output = 0
    while(!E.complete){
      output = E.run(D.run(C.run(B.run(A.run(output)))))
    }
    output
  }
  def findMaxFeedbackLoop(program:Array[Int]): Int = {
    List(5,6,7,8,9).permutations.map(p => amplifyFeedbackLoop(program, p.toArray)).max
  }

  val program = getInputAsIntArray
  printPartOne(findMaxAmp(program))
  printPartTwo(findMaxFeedbackLoop(program))

}
