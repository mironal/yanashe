package org.logginging

import java.io.PrintStream

trait Loggable {

  val out = System.out
  val err = System.err

  val befor = "["
  val after = "]"

  val infoMsg = befor + "info" + after
  val debugMsg = befor + "debug" + after
  val errorMsg = befor + "debug" + after

  private def now() = new java.util.Date()

  private def logMsg(out: PrintStream, level: String)(msg: String) = {
    out.println("[" + now + "]"  + level + msg)
  }

  def info(msg: String) = logMsg(out, infoMsg)_
  def debug(msg: String) = logMsg(out, debugMsg)_
  def error(msg: String) = logMsg(err, errorMsg)_




  /* utils */
  def captureTime(start: Long)(end: Long)(msg: String) = {
      def makeTimes(diffNs: Long) = {
        Map(
          "ns" -> diffNs,
          "us" -> diffNs / 1000,
          "ms" -> diffNs / 1000000
        ).mkString(", ")
      }
      msg + makeTimes(end - start)
  }
}
