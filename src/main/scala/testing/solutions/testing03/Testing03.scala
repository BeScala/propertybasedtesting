package testing.solutions.testing03

import state.RNG.Simple
import state.{RNG, State}

import Gen._

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

object Testing03 extends App {
  private def show[A](ga: Gen[A]): Unit =
    println(ga.sample.run(Simple(System.nanoTime()))._1)

  show(boolean)
  show(choose(0, 10))
  show(listOfSize(10)(boolean))
  show(boolean.listOfSize(10))
  show(boolean.listOfGenSize(choose(0, 10)))
  show(union(choose(0, 5), choose(5, 10)))
}
