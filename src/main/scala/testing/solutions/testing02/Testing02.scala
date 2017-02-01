package testing.solutions.testing02

import state.RNG.Simple
import state.{RNG, State}

import Gen._

case class Gen[A](sample: State[RNG, A]) {
  // monad related
  def map[B](a2b: A => B): Gen[B] =
  Gen(sample.map(a2b))
}

object Gen {
  // useful values
  val boolean: Gen[Boolean] =
  Gen(State(RNG.boolean))

  val nonNegativeInt: Gen[Int] =
    Gen(State(RNG.nonNegativeInt))

  // monad related
  def unit[A](a: => A): Gen[A] =
  Gen(State.unit(a))

  // specific
  def choose(start: Int, stopExclusive: Int): Gen[Int] =
  nonNegativeInt.map { nonNegative =>
    start + nonNegative % (stopExclusive - start)
  }

  // generic
  def listOfSize[A](n: Int): Gen[A] => Gen[List[A]] =
  ga => Gen(State.sequence(List.fill(n)(ga.sample)))
}

object Testing02 extends App {
  private def show[A](ga: Gen[A]): Unit =
    println(ga.sample.run(Simple(System.currentTimeMillis()))._1)

  show(boolean)
  show(choose(0, 10))
  show(listOfSize(10)(boolean))
}
