package org.logginging

import spray.json._

abstract class Matcher[A](responses: Seq[String]) {
  def matchMe(token: A): Boolean
  def takeResponses(token: A): Option[Seq[String]] = {
    matchMe(token) match {
      case true => Some(responses)
      case false => None
    }
  }
}

/* 正規表現によるマッチングを行う.  */
class SimpleMatcher(keyword: String, responses: Seq[String])
  extends Matcher[String](responses){

  def matchMe(token: String): Boolean = {
    token.matches(keyword)
  }
}

object SimpleMatcher {
  def apply(keyword: String,  responses: Seq[String]) = {
    new SimpleMatcher(keyword, responses)
  }
}

object JsonProtocol extends DefaultJsonProtocol {
  implicit val readingFormat = jsonFormat2(ReadingMatcher)
  implicit val surfaceFormat = jsonFormat2(SurfaceFromMatcher)
}


/*
 * 単語をカタカナで評価して正規表現でマッチングを行う.
 *
 * 返答用のファイルのフォーマット
 * [
 *   {"keyword":"keyword0", "responses":["resp0", "resp1", "resp2"]},
 *   {"keyword":"keyword1", "responses":["resp0", "resp1", "resp2"]}
 * ]
 * keywordはカタカナのみ.
 *
 * 複数の単語を同時に指定する場合は
 *  "keyword":"^(ホゲ|フガ)$"
 * のようにする.
 */
case class ReadingMatcher(keyword: String, responses: Seq[String])
  extends SimpleMatcher(keyword, responses)

/*
 * 単語の表現をそのままに正規表現でマッチングを行う.
 *
 * 返答用のファイルのフォーマット
 * [
 *   {"keyword":"keyword0", "responses":["resp0", "resp1", "resp2"]},
 *   {"keyword":"keyword1", "responses":["resp0", "resp1", "resp2"]}
 * ]
 * keywordに「打つ」と指定した場合、「打つ」のみに反応し
 * 「鬱」や「うつ」、「撃つ」には反応しない.
 *
 * 複数を同時に指定する場合は
 *  "keyword":"^(撃つ|打つ)$"
 * とする.
 */
case class SurfaceFromMatcher(keyword: String, responses: Seq[String])
  extends SimpleMatcher(keyword, responses)

