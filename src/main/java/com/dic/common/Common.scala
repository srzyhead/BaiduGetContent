package com.dic.common
import java.io.File
import java.lang.reflect.Field
import java.io.BufferedReader
import java.io.InputStreamReader
import java.io.FileInputStream
import scala.io.Source
import java.net.URL

object Common {

  def toUpperAndLower(s: String) = {
    s.toCharArray().toList.flatMap(x => {
      if (x.toString matches "[a-zA-Z]") {
        ("[" + x.toUpper + x.toLower + "]").toCharArray().toList
      } else {
        List(x)
      }
    }).mkString
  }

  def writeToFile(file: File, s: String, code: String): Unit = {
    val path = file.getAbsolutePath()
    val f = new File(path)
    if (!f.exists()) {
      val d = f.getParentFile()
      if (!d.exists()) {
        d.mkdirs()
      }
      f.createNewFile()
    }
    val pw = new java.io.PrintWriter(f, code)
    try {
      pw.write(s)
      pw.flush()
    } finally {
      pw.close
    }
  }
  def writeToFile(f: File, s: String): Unit = writeToFile(f, s, "GBK")
  def writeToFile(filePath: String, s: String): Unit = writeToFile(new File(filePath), s)
  def writeToFile(filePath: String, s: String, code: String): Unit = writeToFile(new File(filePath), s, code)

  def readFromFile(f: File, code: String): List[String] = {
    val t = Source.fromFile(f, code)
    val l = t.getLines.toList
    t.close
    l
  }
  def readFromFile(f: File): List[String] = readFromFile(f, "UTF-8")
  def readFromFile(fileName: String): List[String] = readFromFile(new File(fileName), "UTF-8")
  def readFromFile(fileName: String, code: String): List[String] = readFromFile(new File(fileName), code)

  def readFromFileAll(f: File, code: String): String = {
    val t = Source.fromFile(f, code)
    val l = t.getLines.toList mkString "\n"
    t.close
    l
  }
  def readFromFileAll(f: File): String = readFromFileAll(f, "UTF-8")
  def readFromFileAll(fileName: String): String = readFromFileAll(new File(fileName), "UTF-8")
  def readFromFileAll(fileName: String, code: String): String = readFromFileAll(new File(fileName), code)

  def readFromURL(url: String, code: String): String = {
    val f = scala.io.Source.fromURL(url, code)
    val s = f.getLines.mkString("\n")
    f.close
    s
  }

  def getOrException[T](o: Option[T], s: String = "value"): T =
    o match {
      case Some(v) => v
      case None => throw new Exception(s + " is not assigned")
    }

  def beanMap(o: Any) = {
    (o.getClass.getDeclaredFields.map(_.getName).toList.filter((t: String) => {
      """[^\w]""".r findFirstIn t match {
        case None => true
        case _ => false
      }
    }) //.filter(x => x matches "[A-Z0-9_]*")
      .map(x => (x, ((x: Field) => {
        x.setAccessible(true)
        x.get(o) match {
          case l: List[Any] => l.mkString("\n")
          case s: String => s
          case o: Any => o.toString
        }
      })(o.getClass.getDeclaredField(x))))).toMap
  }

  def downloadImg(path: String, url: String): Unit = {
    val f = new File(path)
    if (!f.exists()) {
      val d = f.getParentFile()
      if (!d.exists()) {
        d.mkdirs()
      }
      f.createNewFile()
    }

    val con = new URL(url).openConnection()
    val cookieVal = "ipb_member_id=1339576; ipb_pass_hash=cb8815213c147312ffa420cb43a6a28b; yay=21c16ff38d37766d157da4ec092136162774abeb; lv=1386683956-1387538293; uconfig=tl_m-uh_y-rc_0-cats_0-xns_0-ts_m-tr_1-prn_y-dm_l-rx_0-ry_0-ar_0-sa_y-oi_n-qb_n-tf_n-hp_-hk_-xl_"

    con.setRequestProperty("User-Agent", "Mozilla/4.0 (compatible; MSIE 8.0; Windows NT 5.1; Trident/4.0; .NET CLR 2.0.50727; .NET4.0C; .NET4.0E)")
    con.setRequestProperty("Cookie", cookieVal)

    con.setConnectTimeout(7000)
    con.setReadTimeout(7000)
    val s = javax.imageio.ImageIO.read(con.getInputStream())
    javax.imageio.ImageIO.write(s, "jpg", f)
  }
}