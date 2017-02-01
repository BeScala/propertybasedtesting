package testing.exercises.testing02

import state.RNG.Simple
import state.{RNG, State}

import Gen._

case class Gen[A](sample: State[RNG, A]) {
  // monad related
  def map[B](a2b: A => B): Gen[B] = ???
}

object Gen {
  // useful values
  val boolean: Gen[Boolean] = ???

  val nonNegativeInt: Gen[Int] = ???

  // monad related
  def unit[A](a: => A): Gen[A] = ???

  // specific
  def choose(start: Int, stopExclusive: Int): Gen[Int] = ???

  // generic
  def listOfSize[A](n: Int): Gen[A] => Gen[List[A]] = ??? // Hint: use sequence
}

object Testing02 extends App {
  private def show[A](ga: Gen[A]): Unit =
    println(ga.sample.run(Simple(System.currentTimeMillis()))._1)

  show(boolean)
  show(choose(0, 10))
  show(listOfSize(10)(boolean))
}
