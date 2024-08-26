package com.github.meleshkin.license

import com.github.meleshkin.license.HardwareBinder.ABSTRACT_HW

import java.security.MessageDigest

object HardwareBinder extends App{
  type ABSTRACT_HW[SA, SB, H] = (State[SA], HAL[H]) => State[SB]
  type HW = ABSTRACT_HW[String, String, State[String]]

  val st1: ABSTRACT_HW[String, String, State[String]] = (s, h) => h.updateWithCpuInfo(s)
  val st2: ABSTRACT_HW[String, String, State[String]]  = (s, h) => h.updateWithGraphicCards(s)
  val st3: ABSTRACT_HW[String, String, State[String]] = (s, h) => h.updateWithNetworkInterfaces(s)


  def combine[S](a: ABSTRACT_HW[S, S, State[S]], b: ABSTRACT_HW[S, S, State[S]]): ABSTRACT_HW[S, S, State[S]] = (s, h) => {
    b(a(s, h), h)
  }
  def sequence[S](s: List[ABSTRACT_HW[S, S, State[S]]]): ABSTRACT_HW[S, S, State[S]] = {
    s.reduce((z, acc) => combine(z, acc))
  }

  //val st: ABSTRACT_HW[String, String, State[String]] = combine(st1, st2)

  def run[SA, SB, H](f: ABSTRACT_HW[SA, SB, H])(s: State[SA], h: HAL[H]) = {
    f(s, h)
  }

  def run2[SA, SB, H](f: ABSTRACT_HW[SA, SB, H])(s: State[SA], h: HAL[H]) = {
    f(s, h)
  }


  val h: HAL[State[String]] = new MockHal
  val s: State[String] = State(Nil)

  /*
  run(st1)(s, h)
    .andThen(st2)(h)
   */

  //run2(st3)(s, h)
  val finalState: State[String] = run2(sequence(st1 :: st2 :: st3 :: Nil))(s, h)
  val bytes = MD5Digest.bytes(finalState)
  println(bytes.mkString(" "))
}
case class State[+A](get: List[A]) {
  def update[B >: A](b: B): State[B] = {
    State(b :: get)
  }

  def andThen[B, C >: A](f: ABSTRACT_HW[C, B, State[B]])(h: HAL[State[B]]): State[B] = {
    f(this, h)
  }

}
case object State {
  def apply[A](get: List[A]): State[A] = new State[A](get)
}

trait HAL[S] {
  def updateWithCpuInfo(state: S): S
  def updateWithGraphicCards(state: S): S

  def updateWithNetworkInterfaces(state: S): S

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

  override def updateWithNetworkInterfaces(state: State[String]): State[String] = {
    println("Update with NI")
    val macs = "00:01:02:A1:B2:C3" :: "00:01:02:A1:B2:C4" :: "00:01:02:A1:B2:C5" :: Nil
    val sorted = macs.sorted
    state.update(sorted.mkString(","))
  }
}

object MD5Digest {
  def digest(state: State[String]): MessageDigest = {
    val md = MessageDigest.getInstance("MD5")
    state.get.foreach(x => md.update(x.getBytes()))
    md
  }

  def bytes(state: State[String]): Array[Byte] = {
    digest(state).digest()
  }
}
