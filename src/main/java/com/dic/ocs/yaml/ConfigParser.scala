package com.dic.ocs.yaml

import scala.util.parsing.combinator._
import scala.util.matching.Regex

object ConfigParser extends RegexParsers {
  override def skipWhitespace = false
  def `\n` = """ *(\r?\n)""".r
  def `:` = """: *(\r?\n)?""".r

  def head: Parser[String] = str <~ `:` ^^ { _.toString() }
  def document: Parser[Map[String, Map[String, String]]] = repsep(group, `\n`) ^^
    { _.toMap }
  def group: Parser[(String, Map[String, String])] = head ~ keyvalues ^^
    { case h ~ k => (h, k) }
  def keyvalues: Parser[Map[String, String]] = repsep("  " ~> keyvalue, `\n`) ^^
    { _.toMap }
  def keyvalue: Parser[(String, String)] = str ~ ":" ~ str2 ^^
    { case key ~ _ ~ value => (key, value) }

  def str: Parser[String] = """[\p{Graph}&&[^:]]*""".r ^^ { _.toString() }
  def str2: Parser[String] = """[\p{Print}]*""".r ^^ { _.toString() }
  //(String, String)
  def parse(text: String): Map[String, Map[String, String]] = {
    parseAll(document, text) match {
      case Success(r, _) => r
      case Failure(msg, _) => throw new Exception("YAML Parser failure :" + msg)
      case Error(msg, _) => throw new Exception("YAML Parser error :" + msg)
    }
  }

}
