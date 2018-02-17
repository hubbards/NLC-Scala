package nlc

/**
 * TODO: comment
 *
 * @author Spencer Hubbard
 */
object debruijn {
  private val abs: Exp => Exp = Abs.apply _

  /**
   * An abstraction that takes two arguments.
   */
  def abs2: Exp => Exp =
    abs compose abs

  /**
   * An abstraction that takes three arguments.
   */
  def abs3: Exp => Exp =
    abs2 compose abs

  /**
   * An abstraction that takes four arguments.
   */
  def abs4: Exp => Exp =
    abs3 compose abs

  /**
   * An application of a function with two arguments.
   */
  def app2(f: Exp, w: Exp, x: Exp): Exp =
    App(App(f, w), x)

  /**
   * An application of a function with three arguments.
   */
  def app3(f: Exp, w: Exp, x: Exp, y: Exp): Exp =
    App(app2(f, w, x), y)

  /**
   * An application of a function with four arguments.
   */
  def app4(f: Exp, w: Exp, x: Exp, y: Exp, z: Exp): Exp =
    App(app3(f, w, x, y), z)

  /**
   * `Exp` represents a (nameless) lambda calculus expression.
   */
  sealed trait Exp {
    /**
     * An expression is in beta-normal form if it contains no redexes. A redex is
     * an application where the left expression is an abstraction. Note that this
     * method does not return if there is no beta-normal form for this expression.
     *
     * @return the beta-normal form of this expression if it exists.
     */
    def normalForm: Exp =
      this.step match {
        case None => this
        case Some(e) => e.normalForm
      }

    /**
     * Substitutes a given expression for every reference to a given variable in
     * this expression.
     *
     * @param r the given variable.
     * @param e the given expression.
     * @return the result of the substitution.
     */
    def sub(r: Ref, e: Exp): Exp

    // TODO: restrict access to increment and step

    /**
     * Increments the free variables in this expression which is nested within
     * the given number of abstractions.
     */
    def inc(d: Int): Exp

    /**
     * Applies a single step of normal order reduction to this expression if it is
     * a redex.
     */
    def step: Option[Exp]
  }

  case class Ref(v: Int) extends Exp {
    // check preconditions
    require(v >= 0)

    def sub(r: Ref, e: Exp): Exp =
      if (v == r.v) e else if (v < r.v) Ref(v) else Ref(v - 1)

    def inc(d: Int): Ref =
      if (v < d) Ref(v) else Ref(v + 1)

    val step: Option[Exp] = None
  }

  case class Abs(b: Exp) extends Exp {
    def sub(r: Ref, e: Exp): Exp =
      Abs(b.sub(r inc 0, e inc 0))

    def inc(d: Int): Abs =
      Abs(b inc d + 1)

    def step: Option[Exp] =
      b.step map { Abs(_) }
  }

  case class App(l: Exp, r: Exp) extends Exp {
    def sub(r: Ref, e: Exp): Exp =
      App(l.sub(r, e), this.r.sub(r, e))

    def inc(d: Int): App =
      App(l inc d, r inc d)

    def step: Option[Exp] =
      l match {
        case Abs(b) => Some(b.sub(Ref(0), r))
        case _ => l.step match {
          case Some(e) => Some(App(e, r))
          case None => r.step map { App(l, _) }
        }
      }
  }
}