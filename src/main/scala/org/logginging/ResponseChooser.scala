package org.logginging

import spray.json._
import JsonProtocol._
import org.atilika.kuromoji._

import spray.json._

import JsonProtocol._
import scala.collection.JavaConverters._

case class ResponseChooser() extends Loggable with ResourceReadalbe {

  def takeResponse(text: String): Option[String] = {
    val tokenize = (text:String) => {
      val tokenizer = Tokenizer.builder().build()
      val tokens = tokenizer.tokenize(text).asScala
      val log = "\n" + tokens.map(x => "[Token] => "
                               +  x.getAllFeatures).mkString("\n")
      info("[Tokens] => " + log)
      tokens
    }
    def random(list:Seq[String]): Option[String] = {
      val rand = scala.util.Random
      val size = list.size
      size match {
        case 0 => None
        case 1 => Some(list(0))
        case _ => Some(list(rand.nextInt(size)))
      }
    }
    val matchers = takeMatcher()
    val pickup = pickupResponses(matchers)_
    val choose = tokenize andThen makeReadingList andThen pickup andThen random
    choose(text)
  }

  private def takeMatcher(): List[Matcher] = {
    val emptyMatcher = List[Matcher]()
    val readingMatcher = readJson("/response.json") match {
      case Some(json) => json.convertTo[List[ReadingMatcher]]
      case None => emptyMatcher
    }
    readingMatcher
  }

  // 辞書に登録されている単語のみ抽出.
  private val knownList = (tokens: Seq[Token]) => tokens.filter(_.isKnown)
  /*
   * readingのみ抽出してList[String]を作る.
   * readingとは読みをカタカナで表記したもの. 山 => ヤマ
   */
  private val readingList = (tokens: Seq[Token]) => tokens.map(_.getReading)
  /*
   * surfaceFromのみ抽出してList[String]を作る.
   * surfaceFromとは解析した結果そのままの文字. 山
   */
  private val surfaceFromList = (tokens: Seq[Token]) =>
                                                tokens.map(_.getSurfaceForm)

  private def makeReadingList(tokens: Seq[Token]): Seq[String] = {
    /* tokensをknownListにしてからreadingListにする */
    (knownList andThen readingList)(tokens)
  }

  private def pickupResponses(machers: List[Matcher])(tokens: Seq[String]): List[String] = {
    val take = (m: Matcher, tokens: Seq[String]) =>
        m.takeResponses(tokens).getOrElse(Seq[String]())

    def joinResponse(matchers: List[Matcher], responses: List[String]): List[String] = {
      matchers match {
        case Nil => responses
        case x::xs => joinResponse(xs, responses ++ take(x, tokens))
      }
    }
    joinResponse(machers, List())
  }

}
