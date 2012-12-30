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

  private def reloadMatcher() = {
    try{
     readJson("/response.json").convertTo[List[ResponseMatcher]]
   }catch{
     case e =>
      /* 読み込みに失敗したら空のリストを返す. */
      error("json reload error" + e.getMessage)
      List[ResponseMatcher]()
   }
  }

  val tokenizer = Tokenizer.builder().build()

  private def takeResponse(status:Status) = {
    def getText(status:Status) = status.getText
    def takeResponseList(token:Seq[String]) =
      matcher.filter(_.matchMeAny(token)).map(_.responses).flatten
    def splitToken(tweet:String): Seq[String] = {
      val tokens = tokenizer.tokenize(tweet).asScala
      tokens.foreach(x => info("[Token] => " +  x.getAllFeatures))
      tokens.map(_.getReading).filter(_ != null)
    }
    def randomPickup(list:Seq[String]) = {
      if(list.isEmpty){
        None
      }else{
        Some(list(rand.nextInt(list.size)))
      }
    }
    randomPickup(takeResponseList(splitToken(getText(status))))
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

  def reply(text:String, replyTo:Status) = {
    val status =  new StatusUpdate("@" + replyTo.getUser().getScreenName + " " + text)
      .inReplyToStatusId(replyTo.getId)
    try {
      Some(twitter.updateStatus(status))
    } catch {
      case e =>
      /* とりあえずプリントしとく. */
      e.printStackTrace
      None
    }
  }


  override def onStatus(status:Status) {
    /*
     * statusは必ず存在するが、replyは存在するかどうかわからないので
     * Optionにする.
     */
    def captureStatus(reply:Option[Status])(status:Status) = {
      // 必要な物だけ抽出.
      def formatStatus(status:Option[Status]) = {
        status match {
          case Some(s) =>
          Map(
            "text" -> s.getText,
            "createdAd" -> s.getCreatedAt.toString,
            "id" -> s.getId.toString,
            "userId" -> s.getUser().getId.toString,
            "ScreenName" -> s.getUser().getScreenName

          ).mkString(", ")
          case None => "None"
        }
      }
      val msg = Map(
        "[Status]" -> formatStatus(Some(status)),
        "[Seply]" -> formatStatus(reply)
      ).mkString(", ")
      info(msg)
    }

    def captureTime(start:Long)(end:Long) = {
      def makeTimes(diffNs: Long) = {
        Map(
          "ns" -> diffNs,
          "us" -> diffNs / 1000,
          "ms" -> diffNs / 1000000
        ).mkString(", ")
      }
      info("[Time diff] => " + makeTimes(end - start))
    }


    /* Retweetや自分のツイートには反応しない.*/
    if(status.isRetweet){
      info("[Is Retweet] => ")
      return
    }
    if(status.getUser.getId == myId){
      info("[Tweet by me] => ")
      return
    }

    /* 処理開始時刻をキャプチャ. */
    val printTime = captureTime(System.nanoTime)_

    /* matcher再読み込み. */
    if(reload.get){
      info("[Reload matcher]")
      matcher = reloadMatcher()
      reload.set(false)
    }

    val printStatus = takeResponse(status) match {
      case Some(response) =>
        captureStatus(reply(response, status))_
      case None => /* ignore */
        captureStatus(None)_
    }

    /*
      処理に掛かった時間を表示.
      標準出力に掛かった時間は含めたくないので
      このタイミングで実行.
     */
    printTime(System.nanoTime)

    /* statusとかの情報を表示. */
    printStatus(status)
  }
}

