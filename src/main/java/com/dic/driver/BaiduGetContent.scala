package com.dic.driver

import com.dic.common.Logging
import com.dic.common.Common
import scala.io.Source
import org.jsoup.Jsoup
import java.net.URL
import java.io.File

object BaiduGetContent extends Logging {

  def main(args: Array[String]): Unit = {
    deal
  }

  def deal = {
    val l = (1 to 5).toList
    val t = l.foldLeft("")((text, i) => {
      log.debug("deal page:" + i)
      val html = HtmlDeal0(Common.readFromURL("http://tieba.baidu.com/p/2790203265?see_lz=1&pn=" + i, "GBK"))
      text + HtmlDeal1(html)
    })
    Common.writeToFile("hhhh.html", t)
  }

  def deal1 = {
    downloadImg("baidu/tttt.jpg", "http://imgsrc.baidu.com/forum/w%3D580%3Bcp%3Dtieba%2C10%2C403%3Bap%3D%B2%A9%B5%C3%D6%AE%C3%C5%B0%C9%2C90%2C411/sign=7365e929d01373f0f53f6f979434288b/7202dc88d43f879406e4397cd31b0ef41bd53a14.jpg")
  }

  def HtmlDeal0(text: String): String = {
    val doc = Jsoup.parse(text)
    val content = doc.getElementsByAttributeValue("class", "p_content")
      .toArray().toList.asInstanceOf[List[org.jsoup.nodes.Element]]
    content.map(_.toString()).mkString("\n")
  }

  def HtmlDeal1(text: String) = {
    val r1 = """[A-Za-z0-9]*.jpg$""".r
    val doc = Jsoup.parse(text)
    val y = doc.getElementsByTag("img")
      .toArray().toList.asInstanceOf[List[org.jsoup.nodes.Element]].map(_.attr("src"))
    val l2 = y.filter(x => r1.findFirstIn(x).nonEmpty)
      .map(x => (x, "." + File.separator + "hhhh_files" + File.separator + r1.findFirstIn(x).get))

    //    val l2 = y.zip(fileName)
    l2.map(x => {
      if (!((new File(x._2)).exists()))
        downloadImg(x._2, x._1)
    })
    l2.foldLeft(text)((t, x) => {
      t.replace(x._1, x._2)
    })
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
    try {
      log.debug("download file " + url)
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
    } catch {
      case ex: Exception => {
        log.error(ex.getMessage(), ex)
      }
    }
  }
}

