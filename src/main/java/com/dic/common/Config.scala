package com.dic.common

import java.net.InetAddress
import java.lang.reflect.Field
import java.io.File

object Config extends com.dic.common.Logging {

  private val fileType = "YAML"

  def getValue(seq: String*): String = {
    (fileType match {
      case "YAML" => ConfigYAML.getValue(seq: _*)
      case "XML" => ConfigXML.getValue(seq: _*)
    }) match {
      case Some(v) => v
      case None => throw new Exception("Config:" + seq.mkString(",") + " is not assigned")
    }
  }

  def getItems(x: String): Option[Map[String, String]] = {
    fileType match {
      case "YAML" => ConfigYAML.getItems(x)
      case "XML" => ConfigXML.getItems(x)
    }
  }

  def envMap = {
    (
      Config.getClass.getDeclaredFields.map(_.getName).toList.filter((t: String) => {
        """[^\w]""".r findFirstIn t match {
          case None => true
          case _ => false
        }
      }).filter(x => x matches "[A-Z0-9_]*")
      .map(x => (x, ((x: Field) => {
        x.setAccessible(true)
        x.get(Config).toString()
      })(Config.getClass.getDeclaredField(x))))).toMap
  }
}
