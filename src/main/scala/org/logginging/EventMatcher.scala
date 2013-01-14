package org.logginging

import java.util.Date
import java.text.{DateFormat, SimpleDateFormat}

case class Event(token: String, now: Date)

/*
 * 返答用のファイルのフォーマット
 * [
 *   {"keyword":"keyword0",
 *    "from":"YYYY-MM-DD",
 *    "to":"YYYY-MM-DD",
 *    "responses":["resp0", "resp1", "resp2"]},
 *   {"keyword":"keyword1",
 *    "from":"YYYY-MM-DD",
 *    "to":"YYYY-MM-DD",
 *    "responses":["resp0", "resp1", "resp2"]},
 * ]
 *
 * YYYY-MM-DDは2011-10-20とかのこと
 *
 */
case class EventMatcher(keyword: String,
                   from: String,
                   to: String,
                   responses: Seq[String])
              extends Matcher[Event](responses) {
  def matchMe(event: Event): Boolean = {
      false
  }
}
