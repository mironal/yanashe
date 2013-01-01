package org.logginging

import twitter4j._

object Yanashe extends App {

  val listener = new YanasheUserStreamListener()

  val twitterStream = new TwitterStreamFactory().getInstance()
  twitterStream.addListener(listener)
  twitterStream.user

  def waitInput(c: String) : Unit = {
    c match {
      case "q" =>
       twitterStream.shutdown
       case "r" =>
      case _ => waitInput(readLine())
    }
  }
  waitInput(readLine())
}

