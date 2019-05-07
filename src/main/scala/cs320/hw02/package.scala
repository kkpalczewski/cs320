package cs320

package object hw02 extends Homework02 {
  // applies a binary numeric function on all combinations of numbers from
  // the two input lists, and return the list of all of the results
  def binOp(
    op: (Int, Int) => Int,
    ls: List[Int],
    rs: List[Int]
  ): List[Int] = ls match {
    case Nil => Nil
    // :: means simple concatenation
    // here l is head of the list, and rest is tail
    case l :: rest =>
      def f(r: Int): Int = op(r, l)
      rs.map(f) ++ binOp(op, rest, rs)
  }

  def lookup(name: String, env: Map[String, List[Int]]): List[Int] =
    env.get(name) match {
      case Some(v) => v

      case None => error(s"free identifier: $name")
    }

  def interp(muwae: MUWAE, env: Map[String, List[Int]]): List[Int] = muwae match{
    case Add(l, r) =>
      def add(al: Int, ar: Int): Int = {ar + al}
      binOp(add, interp(l, env), interp(r, env))

    case Sub(l, r) =>
      def sub(al: Int, ar: Int): Int = {ar - al}
      binOp(sub, interp(l, env), interp(r, env))
    // !!! We don't have to check if in this list are identifiers, because type of nums is List[Int] !!!

    case Num(nums) => nums

    case Id(x) => lookup(x, env)

    case With(x, i, b) => interp(b, env + (x -> interp(i, env)))

    case Min(l, c, r) =>
      def minn(al: Int, ar: Int): Int = al.min(ar)
        // we compute min of first two combinations and than add another
      binOp(minn,binOp(minn, interp(l, env), interp(c, env)), interp(r, env))

    case Max(l, c, r) =>
      def maxx(al: Int, ar: Int): Int = al.max(ar)
      // we compute min of first two combinations and than add another
      binOp(maxx,binOp(maxx, interp(l, env), interp(c, env)), interp(r, env))
      }

  def run(str: String): List[Int] = {
    val env = Map()
    interp(MUWAE(str), Map())
  }

  def tests: Unit = {
    test(run("{+ 3 7}"), List(10))
    test(run("{- 10 {3 5}}"), List(7, 5))
    test(run("{with {x {+ 5 5}} {+ x x}}"), List(20))
    test(run("{min 3 4 5}"), List(3))
    test(run("{max {+ 1 2} 4 5}"), List(5))
    test(run("{min {1 4} {2 9} 3}"), List(1, 1, 2, 3))
    test(run("{max {1 6} {2 5} {3 4}}"), List(3, 4, 5, 5, 6, 6, 6, 6))
    /* Write your own tests */
    //max with bounded identifiers
    test(run("{with {x {+ 3 4}} {min x {10 10} 20}}"), List(7, 7))
    test(run("{min 80 40 {with {x 10} {+ x {10 50}}}}"), List(20, 40))
    test(run("{with {x {min 1 {4 3} 2}} x}"), List(1, 1))
    test(run("{+ {10 20} {100 200}}"), List(110, 210, 120, 220))

  }
}
