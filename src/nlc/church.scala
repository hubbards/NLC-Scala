package nlc

import debruijn._

/**
 * Church encoding for booleans, numerals, tuples, and sums.
 *
 * @author Spencer Hubbard
 */
object church {
  /* Church booleans */

  // true
  val t: Exp = abs2(Ref(1))

  // false
  val f: Exp = abs2(Ref(0))

  /**
   * Encode a Church boolean for a given boolean.
   * 
   * @param b the given boolean.
   * @return the Church boolean.
   */
  def boolean(b: Boolean): Exp = if (b) t else f

  // if
  val i: Exp = abs3(app2(Ref(2), Ref(1), Ref(0)))

  val not: Exp = Abs(app3(i, Ref(0), f, t))

  val and: Exp = abs2(app3(i, Ref(1), Ref(0), Ref(1)))

  val or: Exp = abs2(app3(i, Ref(1), Ref(1), Ref(0)))

  /* Church numerals */

  val zero: Exp = abs2(Ref(0))

  val one: Exp = abs2(App(Ref(1), Ref(0)))

  val two: Exp = abs2(App(Ref(1), App(Ref(1), Ref(0))))

  val three: Exp = abs2(App(Ref(1), App(Ref(1), App(Ref(1), Ref(0)))))

  /**
   * Encodes a Church numeral for a given integer.
   *
   * @throws IllegalArgumentException if the given integer `n` is negative.
   * @param n the given integer.
   * @return the Church numeral.
   */
  def numeral(n: Int): Exp = {
    // helper function
    def helper(n: Int, e: Exp): Exp =
      if (n == 0) e else App(Ref(1), helper(n - 1, e))
    if (n < 0)
      throw new IllegalArgumentException("given integer must be non-negative")
    else
      abs2(helper(n, Ref(0)))
  }

  val succ: Exp = abs3(App(Ref(1), app2(Ref(2), Ref(1), Ref(0))))

  val pred: Exp =
    abs3(app3(
      Ref(2),
      abs2(App(Ref(0), App(Ref(1), Ref(3)))),
      Abs(Ref(1)),
      Abs(Ref(0))))

  val add: Exp = abs4(app2(Ref(3), Ref(1), app2(Ref(2), Ref(1), Ref(0))))

  val mult: Exp = abs3(App(Ref(2), App(Ref(1), Ref(0))))

  val isZero: Exp = Abs(app2(Ref(0), Abs(f), t))

  /* Church tuples */

  val pair: Exp = abs3(app2(Ref(0), Ref(2), Ref(1)))

  val fst: Exp = Abs(App(Ref(0), t))

  val snd: Exp = Abs(App(Ref(0), f))

  val tuple3: Exp = abs4(app3(Ref(0), Ref(3), Ref(2), Ref(1)))

  val sel13: Exp = Abs(App(Ref(0), abs3(Ref(2))))

  val sel23: Exp = Abs(App(Ref(0), abs3(Ref(1))))

  val sel33: Exp = Abs(App(Ref(0), abs3(Ref(0))))

  /* Church sums */

  val either: Exp = pair

  val inL: Exp = abs3(App(Ref(1), Ref(2)))

  val inR: Exp = abs3(App(Ref(0), Ref(2)))

  val case3: Exp = tuple3

  val in13: Exp = abs4(App(Ref(2), Ref(3)))

  val in23: Exp = abs4(App(Ref(1), Ref(3)))

  val in33: Exp = abs4(App(Ref(0), Ref(3)))
}
