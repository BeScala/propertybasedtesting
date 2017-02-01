package testing.solutions.testing05

import state.RNG.Simple
import state.{RNG, State}
import laziness.Stream
import Gen._
import Prop._
import SGen._

sealed trait Result {
  def isFalsified: Boolean
}

case object Passed extends Result {
  def isFalsified: Boolean = false
}

case class Falsified(falsifiedMessage: FalsifiedMessage, successCount: SuccessCount) extends Result {
  def isFalsified: Boolean = true
}

case class Prop(check: (MaxSize, NumberOfTestCases, RNG) => Result) {
  thisProp =>

  def &&(thatProp: Prop) = Prop {
    (maxSize, numberOfTestCases, rng) => thisProp.check(maxSize, numberOfTestCases, rng) match {
      case Passed => thatProp.check(maxSize, numberOfTestCases, rng)
      case result => result
    }
  }

  def ||(thatProp: Prop) = Prop {
    (maxSize, numberOfTestCases, rng) => thisProp.check(maxSize, numberOfTestCases, rng) match {
      case Falsified(_, _) => thatProp.check(maxSize, numberOfTestCases, rng)
      case result => result
    }
  }

}

object Prop {
  type NumberOfTestCases = Int
  type SuccessCount = Int
  type FalsifiedMessage = String

  val trueProp: Prop = Prop((_, _, _) => Passed)

  def randomStream[A](ga: Gen[A], rng: RNG): Stream[A] =
    Stream.unfold(rng)(rng => Some(ga.sample.run(rng)))

  def randomIndexedStream[A](ga: Gen[A], rng: RNG): Stream[(A, Int)] =
    randomStream(ga, rng).zip(Stream.from(0))

  def randomIndexedStreamOfLength[A](n: Int)(ga: Gen[A], rng: RNG): Stream[(A, Int)] =
    randomIndexedStream(ga, rng).take(n)

  def forAll[A](ga: Gen[A])(pa: A => Boolean): Prop = Prop {
    (_, numberOfTestCases, rng) =>
      randomIndexedStreamOfLength(numberOfTestCases)(ga, rng).map {
        case (a, i) => try {
          print(".")
          if (pa(a)) Passed
          else Falsified(falsifiedMessage(a), i)
        } catch {
          case e: Exception =>
            Falsified(exceptionMessage(a, e), i)
        }
      } find(_.isFalsified) getOrElse Passed
  }

  def forAll[A](sga: SGen[A])(pa: A => Boolean): Prop = Prop {
    (maxSize, numberOfTestCases, rng) =>
      val numberOfTestCasesPerSize = (numberOfTestCases - 1) / maxSize + 1
      Stream.from(0).take((numberOfTestCases min maxSize) + 1)
        .map { s =>
          (s, forAll(sga.forSize(s))(pa))
        }.map { case (s, prop) =>
        Prop { (maxSize, numberOfTestCases, rng) =>
          println(s"${if(s==0) "" else "\n"}size = ${s}")
          prop.check(maxSize, numberOfTestCasesPerSize, rng)
        }
      }.foldRight(trueProp)(_ && _)
        .check(maxSize, numberOfTestCases, rng)
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

  def doubleOfSize(n: Int): Gen[Double] =
    Gen(State.apply(RNG.doubleOfSize(n)))

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

case class SGen[A](forSize: Size => Gen[A]) {
  // monad related
  def map[B](a2b: A => B): SGen[B] =
    SGen(s => {
      forSize(s) map { a =>
        a2b(a)
      }
    })

  def flatMap[B](a2sgb: A => SGen[B]): SGen[B] =
    SGen(s => {
      forSize(s) flatMap { a =>
        a2sgb(a).forSize(s)
      }
    })

}

object SGen {
  type Size = Int
  type MaxSize = Int

  // monad related
  def unit[A](a: => A): SGen[A] =
  SGen(s =>
    Gen.unit(a)
  )

  def list[A](ga: Gen[A]): SGen[List[A]] =
    SGen(s =>
      ga.listOfSize(s)
    )

  def nonEmptyList[A](ga: Gen[A]): SGen[List[A]] =
    SGen(s =>
      ga.listOfSize(s max 1)
    )

  val double: SGen[Double] = SGen(s =>
    Gen.doubleOfSize(s)
  )

}

object Testing05 extends App {
  private def show[A](ga: Gen[A]): Unit =
    println(ga.sample.run(Simple(System.nanoTime()))._1)

  show(boolean)
  show(choose(0, 10))
  show(listOfSize(10)(boolean))
  show(boolean.listOfSize(10))
  show(boolean.listOfGenSize(choose(0, 10)))
  show(union(choose(0, 5), choose(5, 10)))

  private def check(prop: Prop): Unit = {
    val numberOfTestCases = 100
    val maxSize = 10
    println(prop.check(maxSize, numberOfTestCases, Simple(System.nanoTime())) match {
      case Passed =>
        s"\n\033[32mOK: the property passed ${numberOfTestCases} tests\033[0m"
      case Falsified(falsifiedMessage, successCount) =>
        s"\n\033[31mKO: after ${successCount} passed tests, ${falsifiedMessage}\033[0m"
    })
  }

  println()

  check(forAll(Gen.double)(0.0 <= _))

  check(forAll(Gen.double)(_ >= 0.5))

  check(forAll(Gen.double)(_ >= { if(math.random < 0.5) 1/0 else 0}))

  check(forAll(choose(0, 10))(0 <= _) && forAll(choose(0, 10))(_ < 10))

  check(forAll(choose(0, 10))(0 <= _) && forAll(choose(0, 10))(_ < 9))

  check(forAll(SGen.double)(0.0 <= _))

  check(forAll(SGen.double)(_ >= 0.5))

  check(forAll(SGen.double)(_ >= { if(math.random < 0.1) 1/0 else 0}))

  val smallInt: Gen[Int] = choose(-10, 10)

  val noIntInListGreaterThanMax = { (is: List[Int]) =>
    val m = is.max
    !is.exists(i => i > m)
  }

  val maxPropKO: Prop = forAll(list(smallInt))(noIntInListGreaterThanMax)

  val maxPropOK: Prop = forAll(nonEmptyList(smallInt))(noIntInListGreaterThanMax)

  val betterNoIntInListGreaterThanMax = (is: List[Int]) => {
    !is.exists(i => i > is.max)
  }

  val betterMaxPropOK: Prop = forAll(nonEmptyList(smallInt))(betterNoIntInListGreaterThanMax)

  check(maxPropKO)

  check(maxPropOK)

  check(betterMaxPropOK)

  val sortedListIsSortedOK = { (list: List[Int]) =>
    val sortedList = list.sorted
    list.isEmpty || list.tail.isEmpty || sortedList.zip(sortedList.tail).forall { case (a, b) => a <= b }
  }

  val sortedPropOK = forAll(list(smallInt))(sortedListIsSortedOK)

  val sortedListIsSortedKO = { (list: List[Int]) =>
    val sortedList = list.sorted
    list.isEmpty || list.tail.isEmpty || sortedList.zip(sortedList.tail).forall { case (a, b) => a < b }
  }

  val sortedPropKO = forAll(list(smallInt))(sortedListIsSortedKO)

  check(sortedPropOK)

  check(sortedPropKO)

}
