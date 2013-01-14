package org.logginging

import twitter4j._

class YanasheUserStreamListener extends UserStreamAdapter
  with Loggable {

  val twitter = TwitterFactory.getSingleton();
  val myId = twitter.getId
  info("[Start " + getClass.getName + "] => name : " + twitter.getScreenName)

  val chooser = ResponseChooser()

  private val getText = (status: Status) => status.getText

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
    if(isMe(status.getUser)) {
      info("[Tweet by me] => " + formatStatus(status))
      return
    }

    /* 処理開始時刻をキャプチャ. */
    val capturedStart = timeCapture(System.nanoTime)_

    val capturedStatus = captureStatus(status)_
    val choose = getText andThen chooser.takeResponse

    val response =  choose(status) match {
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

  /*
   *  自動フォロー返し.
   */
  override def onFollow(source: User, followedUser: User) {
    def log(source: User, followedUser: User) = {
      "[source]:" + formatUser(source) + " " +
      "[followedUser]:" + formatUser(followedUser)
    }
    info("[onFollow] => " + log(source, followedUser))
    if(isMe(followedUser)){
      info("[Request create friendship] => " + formatUser(source))
      twitter.createFriendship(source.getId)
    }
  }

  override def onException(ex: Exception) {
    error("[onException] => " + ex.getStackTrace.mkString(", "))
  }

  private def isMe(user: User): Boolean = {
    user.getId == myId
  }

  private def formatUser(user: User) = {
    user.getScreenName
  }
}

