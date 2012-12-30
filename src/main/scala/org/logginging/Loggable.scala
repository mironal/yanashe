package org.logginging


import java.io.PrintStream

trait Loggable {
  val out = System.out
  val err = System.err

  private def now() = new java.util.Date()

  private def logMsg(level: Int,  msg: Any) = {
    def log[T <: {def println(msg:String)}] (out: T,  msg:Any) = {
      out.println("[" + now + "]" + msg)
    }
    level match {
      case 0 => log(out,  "[info]" + msg)
      case 1 => log(out,  "[debug]" + msg)
      case 2 => log(err,  "[error]" + msg)
      case _ => log(err,  "[unknown]" + msg)
    }
  }

  def info(msg: Any) = logMsg(0,  msg)
  def debug(msg: Any) = logMsg(1,  msg)
  def error(msg: Any) = logMsg(2,  msg)


  /* utils */
  def timeCapture(start: Long)(end: Long)(msg: Any) = {
    def makeTimes(diffNs: Long) = {
      Map(
        "ns" -> diffNs,
        "us" -> diffNs / 1000,
        "ms" -> diffNs / 1000000
      ).mkString(",  ")
    }
    msg + makeTimes(end - start)
  }

}
