package com.github.meleshkin.license

import com.github.meleshkin.license.HardwareBinder.ABSTRACT_HW

object HardwareBinder extends App{
  type ABSTRACT_HW[SA, SB, H] = (State[SA], HAL[H]) => State[SB]
  type HW = ABSTRACT_HW[String, String, State[String]]

  val st1:ABSTRACT_HW[String, String, State[String]] = (s, h) => h.updateWithCpuInfo(s)
  val st2: ABSTRACT_HW[String, String, State[String]]  = (s, h) => h.updateWithGraphicCards(s)

  val h: HAL[State[String]] = new MockHal
  val s: State[String] = State(Nil)


  def run[SA, SB, H](f: ABSTRACT_HW[SA, SB, H])(s: State[SA], h: HAL[H]) = {
    f(s, h)
  }
  
  run(st1)(s, h)
    .andThen(st2)(h)
}

case class State[+A](get: List[A]) {
  def update[B >: A](b: B): State[B] = {
    State(b :: get)
  }

  def andThen[B](f: ABSTRACT_HW[A, B, State[B]])(h: HAL[State[B]]): State[B] = {
    f(this, h)
  }

}
case object State {
  def apply[A](get: List[A]): State[A] = new State[A](get)
}

trait HAL[S] {
  def updateWithCpuInfo(state: S): S
  def updateWithGraphicCards(state: S): S
}


class MockHal extends HAL[State[String]] {
  override def updateWithCpuInfo(state: State[String]): State[String] = {
    println("Update With Cpu Info")
    val s: String = "Some cpu info"
    state.update(s)
  }

  override def updateWithGraphicCards(state: State[String]): State[String] = {
    println("Update With Graphic cards")
    val s: String = "Some videocard"
    state.update(s)
  }
}
