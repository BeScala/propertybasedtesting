package testing.solutions.testing01

import Prop._

trait Prop {
  thisProp =>
  def check: Boolean

  def &&(thatProp: Prop): Prop = new Prop {
    override def check: Boolean =
      thisProp.check && thatProp.check
  }
}

object Prop {
  val trueProp: Prop = new Prop {
    override def check: Boolean = true
  }
  val falseProp: Prop = new Prop {
    override def check: Boolean = false
  }
}

object Testing01 extends App {
  println((trueProp && falseProp).check)
}
