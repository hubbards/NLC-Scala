package nlc.test

import junit.framework.Assert._
import junit.framework.TestCase

import nlc.debruijn._

/**
 * `DeBruijnTestCase` is a test case for (nameless) lambda calculus expressions.
 *
 * @author Spencer Hubbard
 */
class DeBruijnTestCase extends TestCase {
  // test normal order reduction for reference
  def testRefNormalForm() {
    val e = Ref(1)
    val s = "failure - x should be in normal form"
    assertEquals(s, e.normalForm, e)
  }

  // test normal order reduction for (irreducible) abstraction
  def testAbsNormalForm() {
    val e = Abs(App(Ref(1), Ref(0)))
    val s = "failure - \\x.yx should be in normal form"
    assertEquals(s, e.normalForm, e)
  }

  // test normal order reduction for (irreducible) application
  def testAppNormalForm() {
    val e = App(Ref(1), Abs(App(Ref(2), Ref(0))))
    val s = "failure - x \\x.yx should be in normal form"
    assertEquals(s, e.normalForm, e)
  }

  // test normal order reduction for simple redex
  def testRedex1() {
    val e = App(Abs(Ref(0)), Ref(1))
    e.normalForm match {
      case Ref(i) =>
        assertEquals("failure - (\\x.x) y should reduce to y", i, 1)
      case _ =>
        fail("failure - (\\x.x) y should reduce to reference")
    }
  }

  // test normal order reduction for redex with closed expression on left
  def testRedex2() {
    val e = App(Abs(Abs(App(Ref(0), Ref(1)))), Ref(2))
    e.normalForm match {
      case Abs(App(Ref(i), Ref(j))) =>
        val s = "failure - (\\xy.yx) z should reduce to \\y.yz"
        assertEquals(s, i, 0)
        assertEquals(s, j, 3)
      case _ =>
        fail("failure - (\\xy.yx) z should reduce to abstraction")
    }
  }

  // test normal order reduction for redex with non-closed expression on left
  def testRedex3() {
    val e = App(Abs(Abs(App(Ref(2), Ref(1)))), Ref(0))
    e.normalForm match {
      case Abs(App(Ref(i), Ref(j))) =>
        val s = "failure - (\\xy.zx) z should reduce to \\y.zz"
        assertEquals(s, i, 1)
        assertEquals(s, j, 1)
      case _ =>
        fail("failure - (\\xy.zx) z should reduce to abstraction")
    }
  }
}
