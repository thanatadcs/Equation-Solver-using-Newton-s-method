package solver

object Newton {

  // your implementation of the Newton method that takes a function f: Double => Double
  // and its derivative df: Double => Double  (take note of the types),
  // and computes a root of f using the Newton's method with the given 
  // guess: Double starting value

  def solve(f: Double => Double, df: Double => Double, 
            guess: Double = 1.0): Option[Double] = {

    def goodEnough(guess: Double) = math.abs(f(guess)) < 1e-10

    def improve(guess: Double) = guess - f(guess)/df(guess)

    def repeat(guess: Double): Double =
      if (goodEnough(guess)) guess else repeat(improve(guess))

    Some(repeat(guess))
  }
}
