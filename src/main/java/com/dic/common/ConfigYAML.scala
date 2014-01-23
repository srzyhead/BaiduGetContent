package com.dic.common

import java.io.File
import com.dic.ocs.yaml.ConfigParser

object ConfigYAML {
  //, "GBK"
  var config: Map[String, Map[String, String]] =
    ConfigParser.parse(Common.readFromFile("etc" + File.separator + "Config.yaml")
      .filter(x => !(x matches """^#[\p{Print}]*"""))
      .filter(x => !(x matches """[ ]+""")).mkString("\n"))
  //      var config: Map[String, Map[String, String]] =
  //    ConfigParser.parse(Source.fromFile("etc" + File.separator + "Config.yaml", "GBK")
  //      .getLines.filter(x => !(x matches """^#[\p{Print}]*"""))
  //      .filter(x => !(x matches """[ ]+""")).mkString("\n"))

  def getValue(seq: String*): Option[String] = {
    if (seq.length <= 1) {
      throw new Exception("para not enough")
    } else {
      seq.tail.foldLeft[Option[String]](Some(seq.head))((x: Option[String], y: String) => {
        x match {
          case Some(v) => config get v getOrElse Map[String, String]() get y
          case None => None
        }
      })
    }
  }

  def getItems(x: String): Option[Map[String, String]] = config.get(x)

  override def toString =
    config.map((r: (String, Map[String, String])) => {
      //    	r._1+":\n"
      r._2.foldLeft[String](r._1 + ":\n")((e1: String, e2: (String, String)) => {
        e1 + "  " + e2._1 + ":" + e2._2 + "\n"
      })
    }).mkString("")

}