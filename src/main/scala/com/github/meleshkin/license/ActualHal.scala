package com.github.meleshkin.license

import java.net.NetworkInterface
import scala.jdk.StreamConverters.StreamHasToScala

class ActualHal extends HAL[State[String]] {

  case class NIData(name: String, mac: Array[Byte])

  override def updateWithCpuInfo(state: State[String]): State[String] = ???

  override def updateWithGraphicCards(state: State[String]): State[String] = ???

  override def updateWithNetworkInterfaces(state: State[String]): State[String] = {

    val shouldUseThisNI: NetworkInterface => Boolean = ni => {
      ni.getName != null &&
      ni.getName.nonEmpty &&
      !ni.isVirtual &&
      !ni.isPointToPoint
      ni.getHardwareAddress != null
    }

    val nis = NetworkInterface.networkInterfaces().toScala(LazyList)
    val niData = nis.filter(shouldUseThisNI).map(ni => NIData(ni.getName, ni.getHardwareAddress))
    val niDataSorted: Seq[NIData] = niData.sortWith((a, b) => a.name < b.name)
    val res = niDataSorted
      .map(_.mac
      .map(_.toHexString.toUpperCase))
      .map(_.mkString(":"))
      .mkString(",")

    println(s"Updating with NI: \n  $res")

    state.update(res)
  }
}
