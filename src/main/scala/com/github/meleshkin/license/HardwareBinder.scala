package com.github.meleshkin.license

object HardwareBinder extends App{
  type ABSTRACT_HW[S, H] = (State[S], HAL[H]) => State[S]
  type HW = ABSTRACT_HW[String, State[String]]

  val st1: HW = (s, h) => h.updateWithCpuInfo(s)
  val st2: HW = (s, h) => h.updateWithGraphicCards(s)

  val h: HAL[State[String]] = new MockHal
  val s: State[String] = State(Nil)


  def run[A, B](f: ABSTRACT_HW[A, B])(s: State[A], h: HAL[B]) = {
    f(s, h)
  }

  run(st1)(s, h)
}

case class State[+A](get: List[A]) {
  def update[B >: A](b: B): State[B] = {
    State(b :: get)
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
    State(s :: Nil)
  }

  override def updateWithGraphicCards(state: State[String]): State[String] = {
    println("Update With Graphic cards")
    val s: String = "Some videocard"
    State(s :: Nil)
  }
}
