package com.dic.common

object FileName {
  val digit = 5
  def padZero(i: Int) = i.toString.reverse.padTo(digit, "0").reverse.mkString
  def apply(finger: String, id: Int): String = finger + "_" + padZero(id) + ".jpg"
  def apply(str: String): Option[(String, String)] = {
    try {
      val reg = """([a-zA-Z0-9]+)_([0-9]+).jpg""".r
      val reg(x, y) = str
      Some((x, y))
    } catch {
      case e: scala.MatchError => None
    }
  }
}
