package org.logginging

import org.scalatest.FunSuite

class ResponseMatcherTest extends FunSuite {
  test("read from json") {

  }

  test("simple match") {
    val m = ResponseMatcher("hoge", Seq("hoge0", "hoge1"))
    assert(m.matchMe("hoge") == true)
    assert(m.matchMe("huga") == false)
  }


  test("regex match") {
    val m = ResponseMatcher("^hoge$", Seq("hoge0", "hoge1"))
    assert(m.matchMe("hoge") == true)
    assert(m.matchMe("huga") == false)
    assert(m.matchMe("hogehoge") == false)
  }

  test("regex match or") {
    val m = ResponseMatcher("^(hoge|huga)$", Seq("hoge0", "hoge1"))
    assert(m.matchMe("hoge") == true)
    assert(m.matchMe("huga") == true)
    assert(m.matchMe("hogehoge") == false)
  }

  test("simple match token") {
    val token = Seq("hogehoge", "hoge", "huga")

    val m = ResponseMatcher("hoge", Seq("hoge0", "hoge1"))
    assert(m.matchMeAny(token) == true)
  }

  test("simple match token no match") {
    val token = Seq("hogehoge", "hoge", "huga")

    val m = ResponseMatcher("foo", Seq("hoge0", "hoge1"))
    assert(m.matchMeAny(token) == false)
  }

  test("simple match jp") {
    val m = ResponseMatcher("ホゲ", Seq("hoge", "huga"))
    assert(m.matchMe("ホゲ") == true)
    assert(m.matchMe("フガ") == false)
  }
}
