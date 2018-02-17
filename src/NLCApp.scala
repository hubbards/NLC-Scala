import scala.{ App => _ }

import nlc.debruijn._
import nlc.church._

/**
 * TODO: comment
 *
 * @author Spencer Hubbard
 */
object NLCApp extends scala.App {
  // demo data type
  sealed trait Data
  case class B(b: Boolean) extends Data
  case class N(n: Int) extends Data { require(n >= 0) }
  case class P(b: Boolean, n: Int) extends Data { require(n >= 0) }

  // demo function
  def fun(d: Data): Int =
    d match {
      case B(true) => 1
      case B(false) => 0
      case N(n) => 3 * n
      case P(b, n) => n + (if (b) 1 else 0)
    }

  // encode demo data type as lambda calculus expression
  def data(d: Data): Exp =
    d match {
      case B(b) => App(in13, boolean(b))
      case N(n) => App(in23, numeral(n))
      case P(b, n) => App(in33, app2(pair, boolean(b), numeral(n)))
    }

  // encode demo function as lambda calculus expression
  val fun: Exp = {
    val f = Abs(app3(i, Ref(0), one, zero))
    val g = App(mult, three)
    val h = Abs(app2(add, App(snd, Ref(0)), app3(i, App(fst, Ref(0)), one, zero)))
    app3(case3, f, g, h)
  }

  // evaluate lambda-encoded demo function at given value
  def eval(d: Data): Exp = App(fun, data(d)).normalForm

  // test lambda-encoded demo function
  def test(d: Data): Boolean =  numeral(fun(d)) == eval(d)

  // examples
  def example(d: Data) { println(s"test $d : ${test(d)}") }
  example(N(4))
  example(B(true))
  example(B(false))
  example(P(true, 5))
  example(P(false, 5))
}
