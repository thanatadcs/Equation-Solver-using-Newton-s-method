package solver

object Process {
  // gives a "pretty-print" string form of the expression
  def stringify(e: Expression): String = e match {
    case Constant(c) => c.toString
    case Var(name) => name
    case Sum(l, r) => stringify(l) + " + " + stringify(r)
    case Prod(l @ Sum(_, _), r @ Sum(_, _)) => "(" + stringify(l) + ") * (" + stringify(r) + ")"
    case Prod(l @ Sum(_, _), r) => "(" + stringify(l) + ") * " + stringify(r)
    case Prod(l, r @ Sum(_, _)) => stringify(l) + " * (" + stringify(r) + ")"
    case Prod(l, r) => stringify(l) + " * " + stringify(r)
    case Power(b, e) => stringify(b) + "^" + stringify(e)
  }

  // evaluates a given expression e: Expression using
  // the variable settings in varAssn: Map[String, Double],
  // returning the evaluation result as a Double.

  // Example: eval(e, Map("x" -> 4.0)) evaluates the expression 
  // with the variable "x" set to 4.0.
  def eval(e: Expression, varAssn: Map[String, Double]): Double =  e match {
    case Constant(c) => c
    case Var(v) => varAssn.get(v).get
    case Sum(e1, e2) => eval(e1, varAssn) + eval(e2, varAssn)
    case Prod(e1, e2) => eval(e1, varAssn) * eval(e2, varAssn)
    case Power(e1, e2) => math.pow(eval(e1, varAssn), eval(e2, varAssn))
  }

  // symbolically differentiates an expression e: Expression with
  // respect to the variable varName: String
  def differentiate(e: Expression, varName: String): Expression = simplify(e) match {
    // d/dx [x] = 1
    case Var(varName) => Constant(1)
    // d/dx [c*var] = c d/dx [var]
    case Prod(Constant(n), Var(varName)) => Constant(n)
    case Prod(Var(varName), Constant(n)) => Constant(n)
    // d/dx [var] = 0 where var != varName
    case Var(v) => Constant(0)
    // d/dx [c] = 0
    case Constant(c) => Constant(0)
    // d/dx [e1 + e2] = d/dx [e1] + d/dx [e2]
    case Sum(e1, e2) => simplify( Sum(differentiate(e1, varName), differentiate(e2, varName)) )
    // d/dx [e1 * e2] = (d/dx [e1]) * e2 + e1 * (d/dx [e2])
    case Prod(e1, e2) => simplify( Sum(Prod(differentiate(e1, varName), e2), Prod(differentiate(e2, varName), e1)) )
    // d/dx [f(x)^h] = hf(x)^(h-1) d/dx [f(x)]
    case Power(b, Constant(c)) => simplify( Prod(Prod(Constant(c), Power(b, Constant(c - 1))), differentiate(b, varName)) )
    // d/dx [c^[f(x)]] = ln(c) * c^([f(x)])
    case Power(Constant(c), e) => simplify( Prod(Constant(math.log(c)), Power(Constant(c), e)) )
  }

  // forms a new expression that simplifies the given expression e: Expression
  // the goal of this function is produce an expression that is easier to
  // evaluate and/or differentiate.  If there's a canonical form you'd like to
  // follow, use this function to put an expression in this form.
  // you may leave this function blank if can't find any use. 
  def simplify(e: Expression): Expression = e match {
    // Multiplication by 0
    case Prod(Constant(0), e) => Constant(0)
    case Prod(e2, Constant(0)) => Constant(0)
    // Multiplication by 1
    case Prod(Constant(1), e) => e
    case Prod(e, Constant(1)) => e
    // Addition by 0
    case Sum(e, Constant(0)) => e
    case Sum(Constant(0), e) => e
    // Constant addition
    case Sum(Constant(c1), Constant(c2)) => Constant(c1 + c2)
    // Distributive property
    case Prod(Constant(n), Sum(e1, e2)) => Sum(Prod(Constant(n), simplify(e1)), Prod(Constant(n), simplify(e2)))
    case Prod(Sum(e1, e2), Constant(n)) => Sum(Prod(Constant(n), simplify(e1)), Prod(Constant(n), simplify(e2)))
    // var^1 = var
    case Power(Var(v), Constant(1)) => Var(v)
    // c^1 = c
    case Power(Constant(c), Constant(1)) => Constant(c)
    // var^0 = 1
    case Power(Var(v), Constant(0)) => Constant(1)
    // c^0 = 1
    case Power(Constant(c), Constant(0)) if c != 0 => Constant(1)

    case Sum(e1, e2) => Sum(simplify(e1), simplify(e2))
    case Prod(e1, e2) => Prod(simplify(e1), simplify(e2))
    case Power(e1, e2) => Power(simplify(e1), simplify(e2))

    case _ => e
  }
}
