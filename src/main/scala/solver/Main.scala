package solver

object Solver {
  // solves expString == 0 for the variable in varName with an initial guess
  // specified. We'll assume that the given expression has a root.

  def solve(expString: String, varName: String, guess: Double): Double = {
    val ex = Parser(expString).get

    def f(x: Double) = Process.eval(ex, Map(varName -> x))
    def df(x: Double) = Process.eval(Process.differentiate(ex, varName), Map(varName -> x))

    Newton.solve(f, df, guess).get
  }

  def main(args: Array[String]): Unit = {
    println(solve("x^2 - 4", "x", 1))
  }
}
