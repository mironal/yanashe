package org.logginging

import spray.json._

trait ResourceReadalbe extends Loggable {

    def readText(filename: String ): Option[String] = {
        val in = getClass.getResourceAsStream(filename)
        try {
          Some(scala.io.Source.fromInputStream(in).getLines.mkString)
        }catch {
            case e => error(e.getMessage)
            None
        }finally{
            in.close
        }
    }

    def readJson(filename: String): Option[JsValue] = {
        readText(filename) match {
            case Some(jsonText) => Some(jsonText.asJson)
            case None => None
        }
    }

}
