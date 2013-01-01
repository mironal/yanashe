package org.logginging

import spray.json._


abstract class Matcher(responses: Seq[String]) {
  def matchMe(token: String): Boolean
  def matchMeAny(tokens: Seq[String]): Boolean = tokens.exists(matchMe)
  def takeResponses(tokens: Seq[String]): Option[Seq[String]] = {
    matchMeAny(tokens) match {
      case true => Some(responses)
      case false => None
    }
  }
}

/*
 *
 * 返答用のファイルのフォーマット
 * [
 *   {"keyword":"keyword0", "responses":["resp0", "resp1", "resp2"]},
 *   {"keyword":"keyword1", "responses":["resp0", "resp1", "resp2"]}
 * ]
 *
 */
case class ResponseMatcher(keyword: String, responses: Seq[String])
  extends Matcher(responses){

  def matchMe(token: String): Boolean = {
    token.matches(keyword)
  }

}

object JsonProtocol extends DefaultJsonProtocol {
  implicit val rslvFormat = jsonFormat2(ResponseMatcher)
}

