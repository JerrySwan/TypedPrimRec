package org.tc

// Typed Primitive Recursive Combinators: examples
object Examples {
  
  val add2 : CTerm[Int => Int] = {
    val e1 : CTerm[Int => Int => Int] = Apply(K(),Succ)
    val e2 : CTerm[(Int => Int) => Int => Int] = Apply(S(),e1)
    Apply(e2,Succ)
  }
  
  val mul2 : CTerm[Int => Int] = {
    val e1 : CTerm[(Int => Int) => Int => Int] = Apply(Iter,Zero)
    Apply(e1,add2)
  }
  
  def main(args : Array[String]) : Unit = {
    val num0 = Zero
    val num1 = Apply(Succ,num0)
    val num2 = Apply(Succ,num1)
    val num3 = Apply(Succ,num2)
    println( "num0      = " + num0.evaluate)
    println( "num1      = " + num1.evaluate)
    println( "num2      = " + num2.evaluate)
    println( "num3      = " + num3.evaluate)
    println("-------------")
    println( "add2 num0 = " + Apply(add2,num0).evaluate )
    println( "add2 num1 = " + Apply(add2,num1).evaluate )
    println( "add2 num2 = " + Apply(add2,num2).evaluate )
    println( "add2 num3 = " + Apply(mul2,num3).evaluate )
    println("-------------")
    println( "mul2 num0 = " + Apply(mul2,num0).evaluate )
    println( "mul2 num1 = " + Apply(mul2,num1).evaluate )
    println( "mul2 num2 = " + Apply(mul2,num2).evaluate )
    println( "mul2 num3 = " + Apply(mul2,num3).evaluate )
  }
}