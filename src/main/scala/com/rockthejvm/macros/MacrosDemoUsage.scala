package com.rockthejvm.macros

import MacrosDemo.*

object MacrosDemoUsage {
  val firstMacroUsage = firstMacro(2 + 3, "Scala")

  val optionDescription   = pmOptions(Some(2))
  val optionDescription_2 = pmOptions(Option(2))
  val optionDescription_3 = pmOptions(Option(10).map(_ + 1))

  case class SimpleWrapper(x: Int) {
    def magicMethod(y: Int) =
      s"This simple wrapper called a magic returning ${x + y}"
  }

  val meaningOfLife = 42
  val result        = callMethodDynamically(SimpleWrapper(10), "magicMethod", meaningOfLife)
  // result = SimpleWrapper(10).magicMethod(meaningOfLife) <-- found at COMPILE TIME!!!
  // code does not compile
  // val resultInvalid = callMethodDynamically(SimpleWrapper(10), "magicMethod2", meaningOfLife)
}
