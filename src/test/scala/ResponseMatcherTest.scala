package org.logginging

import org.scalatest.FunSuite

class SimpleMatcherTest extends FunSuite {
  test("read from json") {

  }

  test("simple match") {
    val m = SimpleMatcher("hoge", Seq("hoge0", "hoge1"))
    assert(m.matchMe("hoge") == true)
    assert(m.matchMe("huga") == false)
  }


  test("regex match") {
    val m = SimpleMatcher("^hoge$", Seq("hoge0", "hoge1"))
    assert(m.matchMe("hoge") == true)
    assert(m.matchMe("huga") == false)
    assert(m.matchMe("hogehoge") == false)
  }

  test("regex match or") {
    val m = SimpleMatcher("^(hoge|huga)$", Seq("hoge0", "hoge1"))
    assert(m.matchMe("hoge") == true)
    assert(m.matchMe("huga") == true)
    assert(m.matchMe("hogehoge") == false)
  }

  test("simple match token") {
    val token = Seq("hogehoge", "hoge", "huga")

    val m = SimpleMatcher("hoge", Seq("hoge0", "hoge1"))
    assert(m.matchMeAny(token) == true)
  }

  test("simple match token no match") {
    val token = Seq("hogehoge", "hoge", "huga")

    val m = SimpleMatcher("foo", Seq("hoge0", "hoge1"))
    assert(m.matchMeAny(token) == false)
  }

  test("simple match jp") {
    val m = SimpleMatcher("ホゲ", Seq("hoge", "huga"))
    assert(m.matchMe("ホゲ") == true)
    assert(m.matchMe("フガ") == false)
  }

  test("regex or hiragana") {
    val m = SimpleMatcher("^(ホゲ|ほげ)$", Seq("hoge", "huga"))
    assert(m.matchMe("ホゲ") == true)
    assert(m.matchMe("ほげ") == true)
    assert(m.matchMe("フガ") == false)

  }
}
