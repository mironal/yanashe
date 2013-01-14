package org.logginging

import org.scalatest.FunSuite
import java.text.{DateFormat, SimpleDateFormat}

class EventMatcherTest extends FunSuite {
  test("instance") {
    val m = EventMatcher("keyword", "2000-04-10", "2000-04-20", Seq("a", "b"))
    assert(m.keyword == "keyword")
  }

  test("event instance") {
    val fmt = new SimpleDateFormat("yyyy-MM-dd")

    val e = Event("token", fmt.parse("2000-04-10"))
    assert(e.token == "token")
    assert(e.now == fmt.parse("2000-04-10"))
  }

}
