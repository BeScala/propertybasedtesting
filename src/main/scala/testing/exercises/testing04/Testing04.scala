package testing.exercises.testing04

import state.RNG.Simple
import state.{RNG, State}
import laziness.Stream
import Gen._
import Prop._

sealed trait Result {
  def isFalsified: Boolean
}

case object Passed extends Result {
  def isFalsified: Boolean = false
}

case class Falsified(falsifiedMessage: FalsifiedMessage, successCount: SuccessCount) extends Result {
  def isFalsified: Boolean = true
}

case class Prop(check: (NumberOfTestCases, RNG) => Result) {
  thisProp =>

  def &&(thatProp: Prop) = ???

  def ||(thatProp: Prop) = ???

}

object Prop {
  type NumberOfTestCases = Int
  type SuccessCount = Int
  type FalsifiedMessage = String

  def randomStream[A](ga: Gen[A], rng: RNG): Stream[A] =
    Stream.unfold(rng)(rng => Some(ga.sample.run(rng)))

  def randomIndexedStream[A](ga: Gen[A], rng: RNG): Stream[(A, Int)] =
    randomStream(ga, rng).zip(Stream.from(0))

  def randomIndexedStreamOfLength[A](n: Int)(ga: Gen[A], rng: RNG): Stream[(A, Int)] =
    randomIndexedStream(ga, rng).take(n)

  def forAll[A](ga: Gen[A])(pa: A => Boolean): Prop = Prop {
    (numberOfTestCases, rng) =>
      randomIndexedStreamOfLength(numberOfTestCases)(ga, rng).map {
        ???.asInstanceOf[PartialFunction[(A, Int), Result]]
      } find(_.isFalsified) getOrElse Passed
  }

  def falsifiedMessage[A](a: A): String =
    s"test ${a} falsified the property"

  def exceptionMessage[A](a: A, e: Exception): String =
    s"""test ${a} has thrown Exception("${e.getMessage}")"""
}


case class Gen[A](sample: State[RNG, A]) {
  // monad related
  def map[B](a2b: A => B): Gen[B] =
  Gen(sample.map(a2b))

  def flatMap[B](a2gb: A => Gen[B]): Gen[B] =
    Gen(sample.flatMap { a =>
      val gb: Gen[B] = a2gb(a)
      gb.sample
    })

  // generic
  def listOfSize(n: Int): Gen[List[A]] =
  Gen.listOfSize(n)(this)

  def listOfGenSize(gn: Gen[Int]): Gen[List[A]] =
    gn flatMap { n =>
      listOfSize(n)
    }
}

object Gen {
  // useful values
  val boolean: Gen[Boolean] =
  Gen(State(RNG.boolean))

  val nonNegativeInt: Gen[Int] =
    Gen(State(RNG.nonNegativeInt))

  val double: Gen[Double] =
    Gen(State(RNG.double))

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

  def union[A]: (Gen[A], Gen[A]) => Gen[A] =
    (leftGa, rightGa) => boolean.flatMap { b =>
      if (b) leftGa else rightGa
    }

  def weighted[A]: ((Gen[A], Double), (Gen[A], Double)) => Gen[A] =
    (leftGaAndDouble, rightGaAndDouble) => {
      val leftDouble = leftGaAndDouble._2
      val rightDouble = rightGaAndDouble._2

      val leftDoubleWeight = leftDouble.abs / (leftDouble.abs + rightDouble.abs)

      double.flatMap { double =>
        lazy val leftGen = leftGaAndDouble._1
        lazy val rightGen = rightGaAndDouble._1

        if (double < leftDoubleWeight) leftGen
        else rightGen
      }

    }
}

object Testing04 extends App {
  private def show[A](ga: Gen[A]): Unit =
    println(ga.sample.run(Simple(System.currentTimeMillis()))._1)

  show(boolean)
  show(choose(0, 10))
  show(listOfSize(10)(boolean))
  show(boolean.listOfSize(10))
  show(boolean.listOfGenSize(choose(0, 10)))
  show(union(choose(0, 5), choose(5, 10)))

  private def check(prop: Prop): Unit = {
    val numberOfTestCases = 100
    println(prop.check(numberOfTestCases, Simple(System.currentTimeMillis())) match {
      case Passed =>
        s"\033[32mOK: the property passed ${numberOfTestCases} tests\033[0m"
      case Falsified(falsifiedMessage, successCount) =>
        s"\033[31mKO: after ${successCount} passed tests, ${falsifiedMessage}\033[0m"
    })
  }

  println()

  check(forAll(double)(0.0 <= _))

  check(forAll(double)(_ >= 0.5))

  check(forAll(double)(_ >= { if(math.random < 0.5) 1/0 else 0}))

  check(forAll(choose(0, 10))(0 <= _) && forAll(choose(0, 10))(_ < 10))

  check(forAll(choose(0, 10))(0 <= _) && forAll(choose(0, 10))(_ < 9))

}
