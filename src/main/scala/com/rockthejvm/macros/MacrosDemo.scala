package com.rockthejvm.macros

// metaprogramming
import quoted.*

// 2 + 3 / 4 + 2 * 8 * sin(30)
// trait Expr
// case class Num(value: Double) extends Expr
// case class Sum(lhs: Expr, rhs: Expr) extends Expr
// case class Sub(lhs: Expr, rhs: Expr) extends Expr
// case class Mul(lhs: Expr, rhs: Expr) extends Expr
// case class Div(lhs: Expr, rhs: Expr) extends Expr
// case class Sin(expr: Expr) extends Expr
// ...
/*
Sum(
  Num(2),
  Sum(
    Div(Num(3), Num(4)),
    Mul(
      Num(2),
      Mul(
        Num(8),
        Sin(Num(30))
      )
    )
  )
) == AST
 */

// text -> AST -> binary
// text -> AST -> ....                     -> AST -> binary
// text -> AST (quoting) -> AST -> splicing

object MacrosDemo {

  // quoting + reinserting the AST BACK
  inline def firstMacro(number: Int, string: String): String =
    ${ firstMacroImpl('number, 'string) }

  // manipulating the AST
  // this method is invoked AT COMPILE TIME
  def firstMacroImpl(numAST: Expr[Int], stringAST: Expr[String])(using Quotes): Expr[String] = {
    val numValue    = numAST.valueOrAbort // will trigger a compile error if this is not computable at compile time
    val stringValue = stringAST.valueOrAbort

    val newString =
      if (numValue > 3) stringValue.repeat(numValue)
      else stringValue.take(numValue / 2)

    Expr("The macro impl is: " + newString)
  }

  // quoting and quote matching
  inline def pmOptions(inline opt: Option[Int]) =
    ${ pmOptionsImpl('opt) }

  def pmOptionsImpl(opt: Expr[Option[Int]])(using Quotes): Expr[String] = {
    val result = opt match {
      case '{ Some(42) }                   => "got the meaning of life"
      case '{ Some($x) }                   => s"got a variable: ${x.show}"
      case '{ ($o: Option[a]).map[b]($f) } => "mapping an option"
      case _                               => "got something else"
    }

    Expr(result)
  }

  // "reflection"
  inline def callMethodDynamically[A](instance: A, methodName: String, arg: Int): String =
    ${ callMethodDynamicallyImpl('instance, 'methodName, 'arg) }

  def callMethodDynamicallyImpl[A](instance: Expr[A], methodName: Expr[String], arg: Expr[Int])(using
      q: Quotes
  ): Expr[String] = {
    import q.reflect.*

    // typed AST = Expr[A]
    // "untyped" AST = Term
    val term       = instance.asTerm
    val method     = Select.unique(term, methodName.valueOrAbort)
    val invocation = Apply(method, List(arg.asTerm)) // instance.method(arg)

    invocation.asExprOf[String] // Term -> Expr[String]
  }
}
