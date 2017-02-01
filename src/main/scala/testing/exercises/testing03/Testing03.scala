package testing.exercises.testing03

import state.RNG.Simple
import state.{RNG, State}

import Gen._

case class Gen[A](sample: State[RNG, A]) {
  // monad related
  def map[B](a2b: A => B): Gen[B] =
  Gen(sample.map(a2b))

  def flatMap[B](a2gb: A => Gen[B]): Gen[B] = ???

  // generic
  def listOfSize(n: Int): Gen[List[A]] =
  Gen.listOfSize(n)(this)

  def listOfGenSize(gn: Gen[Int]): Gen[List[A]] = ???
}

object Gen {
  // useful values
  val boolean: Gen[Boolean] =
  Gen(State(RNG.boolean))

  val nonNegativeInt: Gen[Int] =
    Gen(State(RNG.nonNegativeInt))

  val double: Gen[Double] = ???

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

  def union[A]: (Gen[A], Gen[A]) => Gen[A] = ???

  def weighted[A]: ((Gen[A], Double), (Gen[A], Double)) => Gen[A] = ???
}

object Testing03 extends App {
  private def show[A](ga: Gen[A]): Unit =
    println(ga.sample.run(Simple(System.currentTimeMillis()))._1)

  show(boolean)
  show(choose(0, 10))
  show(listOfSize(10)(boolean))
  show(boolean.listOfSize(10))
  show(boolean.listOfGenSize(choose(0, 10)))
  show(union(choose(0, 5), choose(5, 10)))
}
