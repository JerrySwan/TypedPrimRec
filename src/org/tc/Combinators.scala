package org.tc
// Typed Primitive Recursive Combinators
trait CTerm[Return] {
  def evaluate : Return
}
case class K[T1,T2,T3]() extends CTerm[T1 => T2 => T1] {
  override def evaluate : T1 => T2 => T1 = (e : T1) => (f : T2) => e
}
case class S[T1,T2,T3]() extends CTerm[(T1 => T2 => T3) => (T1 => T2) => T1 => T3] {
  override def evaluate : (T1 => T2 => T3) => (T1 => T2) => T1 => T3 = (e : T1 => T2 => T3) => (f : T1 => T2) => (g : T1) => e(g)(f(g))
}
case object Zero extends CTerm[Int] {
  override def evaluate : Int = 0
}
case object Succ extends CTerm[Int => Int] {
  override def evaluate : Int => Int = (x : Int) => x+1
}
case object Iter extends CTerm[Int => (Int => Int) => Int => Int] {
  override def evaluate : Int => (Int => Int) => Int => Int = (z : Int) => (s : Int => Int) => {
    (n : Int) => { if (n <= 0) z else s( evaluate(z)(s)(n-1) ) }
  }
}

// apply
case class Apply[A,B](op : CTerm[A => B], arg : CTerm[A]) extends CTerm[B] {
  override def evaluate : B = op.evaluate(arg.evaluate)
}