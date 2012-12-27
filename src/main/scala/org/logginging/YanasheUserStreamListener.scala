package org.logginging

import twitter4j._
import spray.json._

import JsonProtocol._
import org.atilika.kuromoji._
import scala.collection.JavaConverters._

class YanasheUserStreamListener extends UserStreamAdapter with Loggable {

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
      error("json load error." + e.getMessage)
      List[ResponseMatcher]()
   }
  }

  val tokenizer = Tokenizer.builder().build()

  private def takeResponse(status:Status) = {
    def getText(status:Status) = status.getText
    def takeResponseList(token:Seq[Token]) =
      matcher.filter(_.matchMeAny(token.map(_.getReading))).map(_.responses).flatten
    def formatToken(tokens: Seq[Token]) = {
      tokens.map(_.getAllFeatures).mkString("; ")
    }
    def splitToken(tweet:String) = {
      val token = tokenizer.tokenize(tweet).asScala
      info("[token] => " + formatToken(token))
      token
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
          "user" -> s.getUser.getScreenName,
          "userId" -> s.getUser().getId.toString
        ).mkString(", ")
        case None => "None"
      }
    }
    val msg = Map(
      "status" -> formatStatus(Some(status)),
      "reply" -> formatStatus(reply)
    ).mkString(", ")
    info(msg)
  }


  override def onFriendList(friendIds: Array[Long]) {
    info("onFriendList size:" + friendIds.size)
    friendIds.foreach(info)
  }

  override def onFollow(source: User, followedUser: User) {
    info("onFollow => source : @" + source.getScreenName +
                    " target : @" + followedUser.getScreenName)
  }

  override def onStatus(status:Status) {

    /* Retweetや自分のツイートには反応しない.*/
    if(status.isRetweet){
      info("is Retweet")
      return
    }
    if(status.getUser.getId == myId){
      info("tweet by me")
      return
    }

    /* 処理開始時刻をキャプチャ. */
    val capturedStart = captureTime(System.nanoTime)_

    /* matcher再読み込み. */
    if(reload.get){
      info("reload matcher")
      matcher = reloadMatcher()
      reload.set(false)
    }

    val printStatus = takeResponse(status) match {
      case Some(response) =>
        captureStatus(reply(response, status))_
      case None =>
        captureStatus(None)_
    }
    /* 処理終了時刻をキャプチャ. */
    val capturedTime = capturedStart(System.nanoTime)

    /* statusとかの情報を表示. */
    printStatus(status)

    info(capturedTime("[diff time] => "))
  }
}

