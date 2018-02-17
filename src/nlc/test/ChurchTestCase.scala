package nlc.test

import junit.framework.Assert._
import junit.framework.TestCase

import nlc.debruijn._
import nlc.church._

/**
 * `ChurchTestCase` is a test case for Church encodings.
 *
 * @author Spencer Hubbard
 */
class ChurchTestCase extends TestCase {
  /* test Church booleans */

  // test first case for if
  def testIf1() {
    val x = Ref(0)
    val y = Ref(1)
    val e = app3(i, t, x, y)
    val s = "failure - if true x y should reduce to x"
    assertEquals(s, e.normalForm, x)
  }

  // test second case for if
  def testIf2() {
    val x = Ref(0)
    val y = Ref(1)
    val e = app3(i, f, x, y)
    val s = "failure - if false x y should reduce to y"
    assertEquals(s, e.normalForm, y)
  }

  // TODO: exhaustively test not, and, or

  // test not
  def testNot() {
    val e = App(not, t)
    val s = "failure - not true should reduce to false"
    assertEquals(s, e.normalForm, f)
  }

  // test and
  def testAnd() {
    val e = app2(and, t, f)
    val s = "failure - and true false should reduce to false"
    assertEquals(s, e.normalForm, f)
  }

  // test or
  def testOr() {
    val e = app2(or, t, f)
    val s = "failure - or true false should reduce to true"
    assertEquals(s, e.normalForm, t)
  }

  // test complex boolean expression
  def testBoolean1() {
    val e = app2(and, app2(or, f, t), App(not, f))
    val s = "failure - and (or false true) (not false) should reduce to true"
    assertEquals(s, e.normalForm, t)
  }

  // test complex boolean expression
  def testBoolean2() {
    val e = app2(or, App(not, f), app2(and, t, f))
    val s = "failure - or (not false) (and true false) should reduce to true"
    assertEquals(s, e.normalForm, t)
  }

  /* test Church numerals */

  // test isZero
  def testIsZero1() {
    val e = App(isZero, numeral(0))
    val s = "failure - isZero zero should reduce to true"
    assertEquals(s, e.normalForm, t)
  }

  // test isZero
  def testIsZero2() {
    val e = App(isZero, numeral(1))
    val s = "failure - isZero one should reduce to false"
    assertEquals(s, e.normalForm, f)
  }

  // test add
  def testAdd() {
    val e = app2(add, numeral(1), numeral(2))
    val s = "failure - add one two should reduce to three"
    assertEquals(s, e.normalForm, numeral(3))
  }

  // test mult
  def testMult() {
    val e = app2(mult, numeral(2), numeral(2))
    val s = "failure - mult two two should reduce to four"
    assertEquals(s, e.normalForm, numeral(4))
  }

  // test succ
  def testSucc() {
    val e = App(succ, numeral(1))
    val s = "failure - succ one should reduce to two"
    assertEquals(s, e.normalForm, numeral(2))
  }

  // test pred
  def testPred() {
    val e = App(pred, numeral(1))
    val s = "failure - pred one should reduce to zero"
    assertEquals(s, e.normalForm, numeral(0))
  }

  // test complex numerical expression
  def testNumeral1() {
    val e = app2(mult, app2(add, numeral(2), numeral(3)), numeral(3))
    val s = "failure - mult (add two three) three should reduce to fifteen"
    assertEquals(s, e.normalForm, numeral(15))
  }

  /* test Church tuples */

  // TODO: implement unit tests for Church tuples

  /* test Church sums */

  // TODO: implement unit tests for Church sums

}
