package com.dic.common

import org.apache.log4j.{ PropertyConfigurator, Logger }
import java.io.File

object LogUtil {

  PropertyConfigurator.configure("etc" + File.separator + "log.properties")

  def getLogger(clazz: Class[_]): Logger = {
    Logger.getLogger(clazz)
  }

}

trait Logging {
  //  protected lazy val log: Log = Log.forClass(getClass)
  protected lazy val log: Logger = LogUtil.getLogger(getClass())

}