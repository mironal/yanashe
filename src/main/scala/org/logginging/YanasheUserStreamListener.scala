package org.logginging

import twitter4j._
import spray.json._

import JsonProtocol._
import org.atilika.kuromoji._
import scala.collection.JavaConverters._

class YanasheUserStreamListener extends UserStreamAdapter
  with Loggable {

  val twitter = TwitterFactory.getSingleton();
  val myId = twitter.getId
  val rand = scala.util.Random
  val reload = new java.util.concurrent.atomic.AtomicBoolean

  var matcher = reloadMatcher()

  def requestReloadMatcher() {
    reload.set(true)
  }

  private def reloadMatcher():List[Matcher] = {
    try{
     readJson("/response.json").convertTo[List[ResponseMatcher]]
   }catch{
     case e =>
      /* 読み込みに失敗したら空のリストを返す. */
      error("json reload error" + e.getMessage)
      List[Matcher]()
   }
  }

  val tokenizer = Tokenizer.builder().build()

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

  private val getText = (status: Status) => status.getText


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

  private def chooseResponse(status:Status):Option[String] = {
    def tokenize(text:String): Seq[Token] = {
      val tokens = tokenizer.tokenize(text).asScala
      tokens.foreach(x => info("[Token] => " +  x.getAllFeatures))
      tokens
    }
    def random(list:Seq[String]): Option[String] = {
      val size = list.size
      size match {
        case 0 => None
        case 1 => Some(list(0))
        case _ => Some(list(rand.nextInt(size)))
      }
    }
    val pickup = pickupResponses(matcher)_
    val choose = getText andThen tokenize andThen makeReadingList andThen pickup andThen random
    choose(status)
  }

  def readJson(filename:String) = {
    val in = getClass.getResourceAsStream(filename)
      try{
      scala.io.Source.fromInputStream(in)
      .getLines().mkString.asJson
    }finally{
      in.close
    }
  }

  def reply(text:String, replyTo:Status): Option[Status] = {
    val status =  new StatusUpdate("@" + replyTo.getUser().getScreenName
          + " " + text)
      .inReplyToStatusId(replyTo.getId)
    try {
      Some(twitter.updateStatus(status))
    } catch {
      case e =>
      /* とりあえずプリントしとく. */
      error(e.getMessage)
      None
    }
  }

  def formatStatus(status: Status): String = {
    Map(
      "text" -> status.getText,
      "createdAd" -> status.getCreatedAt.toString,
      "id" -> status.getId.toString,
      "userId" -> status.getUser().getId.toString,
      "ScreenName" -> status.getUser().getScreenName
    ).mkString(", ")
  }

  /*
  * statusは必ず存在するが、replyは存在するかどうかわからないので
  * Optionにする.
  */
  def captureStatus(status: Status)(reply: Option[Status])(msg: Any): String = {
    // 必要な物だけ抽出.
    msg + Map(
      "[Receive]" -> formatStatus(status),
      "[Reply]" -> {
        reply match {
          case Some(s) => formatStatus(s)
          case None    => "None"
        }
      }
    ).mkString(", ")
  }

  override def onStatus(status:Status) {

    /* Retweetや自分のツイートには反応しない.*/
    if(status.isRetweet){
      info("[Is Retweet] => " + formatStatus(status))
      return
    }
    if(status.getUser.getId == myId){
      info("[Tweet by me] => " + formatStatus(status))
      return
    }
    /* 処理開始時刻をキャプチャ. */
    val capturedStart = timeCapture(System.nanoTime)_

    /* matcher再読み込み. */
    if(reload.get){
      info("[Reload matcher]")
      matcher = reloadMatcher()
      reload.set(false)
    }

    val capturedStatus = captureStatus(status)_

    val response = chooseResponse(status) match {
      case Some(response) => reply(response, status)
      case None => None
    }

    /*
      処理に掛かった時間を表示.
      標準出力に掛かった時間は含めたくないので
      このタイミングで実行.
     */
    val capturedTime = capturedStart(System.nanoTime)

    /* statusとかの情報を表示. */
    info(capturedStatus(response)("[Status] => "))
    info(capturedTime("[Diff time] => "))
  }
}

