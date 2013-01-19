package org.logginging

import spray.json._
import JsonProtocol._
import org.atilika.kuromoji._

import spray.json._

import JsonProtocol._
import scala.collection.JavaConverters._

case class ResponseChooser() extends Loggable with ResourceReadalbe {

  def takeResponse(text: String): Option[String] = {
    val tokenize = (text: String) => {
      val tokenizer = Tokenizer.builder().build()
      val tokens = tokenizer.tokenize(text).asScala.toList
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

    val tokens = tokenize(text)

    val readingList = makeReadingList(tokens)
    val surfaceFormList = makeSurfaceFormList(tokens)

    val pickupByReading = pickupResponses(readingList) _
    val pickupBySurface = pickupResponses(surfaceFormList) _

    val matchers = takeMatcher()

    val responses = matchers.map{ x: Matcher[_] =>
      x match {
        case m: ReadingMatcher => pickupByReading(m)
        case m: SurfaceFromMatcher => pickupBySurface(m)
        case m => error("[Unknon Matcher] => " + m); Seq[String]()
      }
    }.flatten
    random(responses)
  }

  private def takeMatcher(): List[Matcher[_]] = {
    val emptyMatcher = List[Matcher[_]]()
    val readingMatcher = readJson("/response.json") match {
      case Some(json) => json.convertTo[List[ReadingMatcher]]
      case None => error("[takeMatcher] ReadingMatcher is empty."); emptyMatcher
    }

    val surfaceFormMatcher = readJson("/surfaceForm.json") match {
      case Some(json) => json.convertTo[List[SurfaceFromMatcher]]
      case None => error("[takeMatcher] SurfaceFormMatcher is empty."); emptyMatcher
    }
    readingMatcher ++ surfaceFormMatcher
  }


  private def makeReadingList(tokens: List[Token]): List[String] = {
    /* tokensをknownListにしてからreadingListにする */
    (knownList andThen readingList)(tokens)
  }

  private def makeSurfaceFormList(tokens: List[Token]): List[String] = {
    (knownList andThen surfaceFromList)(tokens)
  }

  private def pickupResponses(tokens: List[String])(matcher: Matcher[String]): Seq[String] = {
    val take = (token: String) => {
      matcher.takeResponses(token).getOrElse(Seq[String]())
    }
    def join(tokens: List[String], response: Seq[String]): Seq[String] = {
      tokens match {
        case Nil => response
        case x::xs => join(xs, response ++ take(x))
      }
    }
    val resp = join(tokens, Seq[String]())
    info("[PickupResponses] " + matcher + " => " + formatResponse(resp))
    resp
  }

  private def formatResponse(response: Seq[String]): String = {
    response.size match {
      case 0 => "Empty"
      case _ => response.mkString(", ")
    }
  }

  // 辞書に登録されている単語のみ抽出.
  private val knownList = (tokens: List[Token]) => tokens.filter(_.isKnown)
  /*
   * readingのみ抽出してList[String]を作る.
   * readingとは読みをカタカナで表記したもの. 山 => ヤマ
   */
  private val readingList = (tokens: List[Token]) => tokens.map(_.getReading)
  /*
   * surfaceFromのみ抽出してList[String]を作る.
   * surfaceFromとは解析した結果そのままの文字. 山
   */
  private val surfaceFromList = (tokens: List[Token]) =>
                                                tokens.map(_.getSurfaceForm)
}
