package testing.exercises.testing01

import Prop._

trait Prop {
  thisProp =>
  def check: Boolean

  def &&(thatProp: Prop): Prop = ???
}

object Prop {
  val trueProp: Prop = ???

  val falseProp: Prop = ???
}

object Testing01 extends App {
  println((trueProp && falseProp).check)
}
