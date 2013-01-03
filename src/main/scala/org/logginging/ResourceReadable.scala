package org.logginging

import spray.json._

import java.io.File

trait ResourceReadalbe extends Loggable {


    def readText(filename: String ): Option[String] = {
        val in = getClass.getResourceAsStream(filename)
        try {
          Some(scala.io.Source.fromInputStream(in).getLines.mkString)
        }catch {
          case e => error("[readText] filename => " + filename); None
        }finally{
          if(in != null){
            in.close
          }
        }
    }

    def readJson(filename: String): Option[JsValue] = {
      try {
        readText(filename) match {
            case Some(jsonText) => Some(jsonText.asJson)
            case None => error("[readJson] filename => " + filename); None
        }
      }catch {
        // とりあえずprint
        case e => e.printStackTrace()
        None
      }
    }

}
