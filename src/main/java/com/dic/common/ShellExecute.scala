package com.dic.common

import scala.sys.process._

abstract class ShellExecute extends com.dic.common.Logging {

  val shell: String
  //  lazy val envMap: Map[String, String] = Config.envMap

  //  val pb = Process(Seq("sh", "-c", """dir"""), None,
  //    "OCSConfig" -> new File(Config.configFileName).getAbsolutePath(),
  //    "LOG_SHM_KEY" ->
  //      Common.getOrException("LogShmKey", Config.getValue("root", "config", "LogShmKey")))
  // new File(".")

  lazy val pb = System.getProperty("os.name").matches("Windows[a-zA-Z0-9 ]*") match {
    case true => Process(Seq("cmd", "/c", shell))
    case false => Process(Seq("bash", "-c", shell))
  }

  def execute(): Unit = {
    log.debug("shell:" + shell)
    log.debug("------------")
    val exit = pb.!
    log.debug("------------")
    log.debug("exit:" + exit)

    exit match {
      case 0 =>
      case _ => {
        log.info("shell execute error!")
        //        throw new Exception("shell execute error!")
      }
    }
  }

  //  def executeBlocked() = {
  //    pb !
  //  }

  //  def executeNoBlocked() = {
  //    pb run
  //  }

  //  val exit = "ls" !

  //ProcessLogger(
  //    (o: String) => {
  //      out.append(o)
  //      log.debug("out " + out.toString)
  //      
  //    },
  //    (e: String) => println("err " + e))
}