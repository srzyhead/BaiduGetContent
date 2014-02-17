package com.dic.driver

import com.dic.common.Logging
import com.dic.common.Common
import scala.io.Source
import org.jsoup.Jsoup
import java.net.URL
import java.io.File
import java.io.FileOutputStream
import java.io.FileReader
import java.io.FileInputStream
import java.util.HashMap
import java.io.OutputStream
import java.io.ByteArrayOutputStream

case class TieBaURL(id: String, pn: Int, seelz: Int) {
  override def toString = "http://tieba.baidu.com/p/" + id + "?pn=" + pn + "&see_lz=" + seelz
}

object TieBaURL {
  def apply(url: String) = {
    //     """http://tieba.baidu.com/p/2801427040?pn=1&see_lz=0"""
    val r1 = """http://tieba\.baidu\.com/p/([0-9]+)pn=([0-9]+)&see_lz=([0-1])""".r
    val r1(id, pn, seelz) = url
    new TieBaURL(id, pn.toInt, seelz.toInt)
  }
}

object BaiduGetContent extends Logging {

  val SRC = "src"
  val HEIGHT = "height"
  val WIDTH = "width"

  //  URLEncoder.encode(s, "gbk")
  //  URLDecoder.decode(s, "gbk")

  def main(args: Array[String]): Unit = {
    deal0
  }

  def deal = {
    //    Õ½±¨
    //    http://tieba.baidu.com/p/2872799090
    //    http://tieba.baidu.com/p/2790203265

    //    val html = HtmlDeal0(Common.readFromURL("http://tieba.baidu.com/p/2790203265", "GBK"))
    //    Common.writeToFile("hhhh.html", html)
    //    htmlCodeComeFromFile("hhhh.html", "hhhh.pdf")
  }

  def deal0 = {
    //    "http://tieba.baidu.com/p/" + id + "?pn=" + i + "&see_lz=" + seelz
    val id = "2801427040"
    val seelz = 1
    val totalPage = getPn(TieBaURL(id, 1, seelz).toString)
    println("Total page: " + totalPage)
    val t = (1 to totalPage).toList.foldLeft("")((text, i) => {
      log.debug("deal page:" + i)
      text + HtmlDeal0(Common.readFromURL(TieBaURL(id, i, seelz).toString, "GBK"))
    })
    Common.writeToFile("hhhh.html", t)
    println("createing pdf ...")
    htmlCodeComeFromFile("hhhh.html", "hhhh.pdf")
    println("create pdf over")
  }

  def HtmlDeal0(text: String): String = {
    val doc = Jsoup.parse(text)
    //    val content = doc.getElementsByAttributeValue("class", "p_content")
    //      .toArray().toList.asInstanceOf[List[org.jsoup.nodes.Element]]
    val content = doc.getElementsByAttributeValueMatching("id", "post_content_[0-9]*")
      .toArray().toList.asInstanceOf[List[org.jsoup.nodes.Element]]
    content.map(x => ElementDeal(x)).mkString("\n")
  }

  def getPn(url: String): Int = {
    val doc = Jsoup.parse(Common.readFromURL(url, "GBK"))
    val tags = doc.getElementsByTag("span").toArray().toList.asInstanceOf[List[org.jsoup.nodes.Element]]
    val l = tags.filter(x => x.attr("class") == "red" && x.attr("style") == "")
    l.apply(0).html().toInt
  }

  def ElementDeal(e: org.jsoup.nodes.Element) = {
    //    height="420" width="560"
    val t = e.childNodes().toArray().toList.asInstanceOf[List[org.jsoup.nodes.Node]]
    t.map(x => {
      if (x.attr("class") == "pic_src_wrapper") {
        val l = x.childNodes().toArray().toList.asInstanceOf[List[org.jsoup.nodes.Node]]
        val i = l.filter(_.attr("class") == "BDE_Image").apply(0)
        imgNodeDeal(i) //+ "<div><br /></div>"
      } else if (x.attr("class") == "BDE_Image") {
        imgNodeDeal(x) // + "<div><br /></div>"
      } else {
        val elem = new org.jsoup.nodes.Element(org.jsoup.parser.Tag.valueOf("div"), "")
        elem.append(x.toString()).toString() //+ "<div><br /></div>"
        //        x.toString() + "<br />"
      }
    }).mkString("")
  }

  def HtmlDeal1(text: String) = {
    val doc = Jsoup.parse(text)
    val content = doc.getElementsByAttributeValue("class", "image_original_original")
      .toArray().toList.asInstanceOf[List[org.jsoup.nodes.Element]]
    content.map(_.toString()).mkString("\n")
  }

  def htmlCodeComeFromFile(filePath: String, pdfPath: String) {
    com.itextpdf.text.FontFactory.registerDirectories()
    val document = new com.itextpdf.text.Document(com.itextpdf.text.PageSize.A4)
    val writer = com.itextpdf.text.pdf.PdfWriter.getInstance(document, new FileOutputStream(pdfPath))
    document.open()
    com.itextpdf.tool.xml.XMLWorkerHelper.getInstance().parseXHtml(writer, document,
      new FileInputStream(filePath),
      new FileInputStream("auto.css"),
      null, new com.itextpdf.tool.xml.XMLWorkerFontProvider())
    document.close()
  }

  def imgNodeDeal(node: org.jsoup.nodes.Node): String = {
    val r1 = """http://imgsrc.baidu.com/[\p{Graph}]+?/([0-9a-z]*.jpg)""".r

    val attrSrc = node.attr(SRC)
    val attrHeight = node.attr(HEIGHT)
    val attrWidth = node.attr(WIDTH)

    val jpgStr = """http://imgsrc.baidu.com/forum/pic/item/""" + r1.findFirstMatchIn(attrSrc).get.group(1)
    //    val hw = getImgHeightWidth(jpgStr)
    //    val realHeight = hw._1.toString
    //    val realWidth = hw._2.toString

    val attrs = new org.jsoup.nodes.Attributes
    attrs.put(new org.jsoup.nodes.Attribute(SRC, jpgStr))
    attrs.put(new org.jsoup.nodes.Attribute(HEIGHT, attrHeight))
    attrs.put(new org.jsoup.nodes.Attribute(WIDTH, attrWidth))
    val elem = new org.jsoup.nodes.Element(org.jsoup.parser.Tag.valueOf("img"), "", attrs)

    elem.toString()
  }
  /**
   * download Img to path
   *
   * @param url
   *
   * @param path
   *
   * @return String
   *
   */
  def downloadImg(path: String, url: String): Unit = {
    val con = new URL(url).openConnection()
    con.setConnectTimeout(7000)
    con.setReadTimeout(7000)
    val s = javax.imageio.ImageIO.read(con.getInputStream())

    val f = new File(path)
    if (!f.exists()) {
      val d = f.getParentFile()
      if (!d.exists()) {
        d.mkdirs()
      }
      f.createNewFile()
    }
    javax.imageio.ImageIO.write(s, "jpg", f)
  }

  /**
   * download Img
   *
   * @param url
   *
   * @param path
   *
   * @return String
   *
   */
  def getImg(url: String): Array[Byte] = {
    val con = new URL(url).openConnection()
    con.setConnectTimeout(7000)
    con.setReadTimeout(7000)
    val s = javax.imageio.ImageIO.read(con.getInputStream())
    val baos = new ByteArrayOutputStream()
    javax.imageio.ImageIO.write(s, "jpg", baos)
    baos.toByteArray()
  }

  def getImgHeightWidth(url: String): (Int, Int) = {
    val con = new URL(url).openConnection()
    con.setConnectTimeout(7000)
    con.setReadTimeout(7000)
    val s = javax.imageio.ImageIO.read(con.getInputStream())
    (s.getHeight(), s.getWidth())
  }

  def imgBase64Encode(url: String): String = {
    val img = getImg(url)
    val text = org.apache.commons.codec.binary.Base64.encodeBase64(img)
    (0 until text.length).toList.map(x => text.apply(x).toChar).mkString("")
  }
}

