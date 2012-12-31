package org.logginging

import spray.json._

/*
 *
 * 返答用のファイルのフォーマット
 * [
 *   {"keyword":"keyword0", "responses":["resp0", "resp1", "resp2"]},
 *   {"keyword":"keyword1", "responses":["resp0", "resp1", "resp2"]}
 * ]
 *
 */
case class ResponseMatcher(keyword:String, responses:Seq[String]) {

  def matchMe(token:String): Boolean = {
    token.matches(keyword)
  }

  def matchMeAny(tokens:Seq[String]):Boolean = tokens.exists(matchMe)
}

object JsonProtocol extends DefaultJsonProtocol {
  implicit val rslvFormat = jsonFormat2(ResponseMatcher)
}

