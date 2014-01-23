package com.dic.common

import java.io.File

object ConfigXML {

  var config: Map[String, Map[String, String]] =
    (xml.XML.loadFile("etc" + File.separator + "Config.xml") \ "Param")
      .iterator.toList.map((t: scala.xml.Node) => {
        ((t \ "Name").text,
          (t \ "Item").iterator.toList.map((t2: scala.xml.Node) => {
            ((t2 \ "Name").text, (t2 \ "Value").text)
          }).toMap)
      }).toMap

  def getValue(seq: String*): Option[String] = {
    if (seq.length <= 1) {
      None
    } else {
      seq.tail.foldLeft[Option[String]](Some(seq.head))((x: Option[String], y: String) => {
        x match {
          case Some(v) => config get v getOrElse Map[String, String]() get y
          case None => None
        }
      })
    }
  }

  def toXML =
    <Config>
      {
        config.map(keyVal => {
          <Param>
            <Name>{ keyVal._1 } </Name>
            <Item>
              {
                keyVal._2.map(keyVal2 => {
                  <Name>{ keyVal2._1 } </Name>
                  <Value>{ keyVal2._2 } </Value>
                })
              }
            </Item>
          </Param>
        })
      }
    </Config>

  def getItems(x: String): Option[Map[String, String]] = config.get(x)

  override def toString =
    toXML.text
}