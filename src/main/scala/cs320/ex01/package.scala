package cs320

//import cs320.ex01.{boundIdsRec, freeIdsRec}

package object ex01 extends Exercise01 {
  // Problem 1
  //def freeIdsRec(expr: )

  def freeIds(expr: WAE): Set[String] = {
    def freeIdsRec(e: WAE, boundId: Set[String]): Set[String] = e match{
      case Num(num) => Set()
      case Add(left, right) => freeIdsRec(left, boundId) ++ freeIdsRec(right, boundId)                  //     | {+ e e}
      case Sub(left, right) => freeIdsRec(left, boundId) ++ freeIdsRec(right, boundId)                  //     | {- e e}
      case With(name, expr, body) => freeIdsRec(expr, boundId) ++ freeIdsRec(body, boundId + name)//     | {with {x e} e}
      case Id(id) =>
        if(boundId.contains(id)) Set()
        else Set(id)
    }
    freeIdsRec(expr, Set())
  }
  // Problem 2

  def bindingIds(expr: WAE): Set[String] = expr match{
    case Num(num: Int) => Set()
    case Add(left: WAE, right: WAE) => bindingIds(left) ++ bindingIds(right)                  //     | {+ e e}
    case Sub(left: WAE, right: WAE) => bindingIds(left) ++ bindingIds(right)                  //     | {- e e}
    case With(name: String, expr: WAE, body: WAE) => Set(name) ++ bindingIds(expr) ++ bindingIds(body)//     | {with {x e} e}
    case Id(id: String) => Set()
  }

  // Problem 3


  def boundIds(expr: WAE): Set[String] = {
    def boundIdsRec(e: WAE, boundId: Set[String]): Set[String] = e match{
      case Num(num) => Set()
      case Add(left, right) => boundIdsRec(left, boundId) ++ boundIdsRec(right, boundId)                  //     | {+ e e}
      case Sub(left, right) => boundIdsRec(left, boundId) ++ boundIdsRec(right, boundId)                  //     | {- e e}
      case With(name, expr, body) => boundIdsRec(expr, boundId) ++ boundIdsRec(body, boundId + name)//     | {with {x e} e}
      case Id(id: String) =>
        if(boundId.contains(id)) Set(id)
        else Set()
    }
    boundIdsRec(expr, Set())
  }

  // Tests
  def tests: Unit = {
    // tests for freeIds
    test(freeIds(WAE("{with {x 3} {+ x {- 3 x}}}")), Set())
    test(freeIds(WAE("{with {x 3} {- a {+ 4 x}}}")), Set("a"))
    test(freeIds(WAE("{with {x 3} {- b {- a x}}}")), Set("a", "b"))
    test(freeIds(WAE("{with {x 3} {- a {- b {+ x b}}}}")), Set("a", "b"))
    test(freeIds(WAE("{with {x 3} {- y {with {y 7} {+ x {- b a}}}}}")), Set("a", "b", "y"))
    test(freeIds(WAE("{with {x t} {- x {with {y y} {+ x {- b a}}}}}")), Set("a", "b", "t", "y"))
    test(freeIds(WAE("{with {x {with {y 3} {- x y}}} {+ x y}}")), Set("x", "y"))
    test(freeIds(WAE("{+ {with {x 10} {with {x 3} {- y {with {y 7} {+ x {- c b}}}}}} {with {a a} a}}")), Set("a", "b", "c", "y"))
    test(freeIds(WAE("{+ {with {x 10} {with {x 3} {- y {with {y 7} {+ x {- c b}}}}}} {with {a d} a}}")), Set("b", "c", "d", "y"))
    test(freeIds(WAE("{+ {with {x 10} {with {x 3} {- y {with {y 7} {+ x {- c b}}}}}} {with {a d} z}}")), Set("b", "c", "d", "y", "z"))

    // tests for bindingIds
    test(bindingIds(WAE("{+ 3 {- x y}}")), Set())
    test(bindingIds(WAE("{with {y 3} {with {x x} y}}")), Set("x", "y"))
    test(bindingIds(WAE("{with {y 3} {with {y x} {+ x y}}}")), Set("y"))
    test(bindingIds(WAE("{with {y 3} {with {y {with {x {+ 3 y}} {- x y}}} {+ x y}}}")), Set("x", "y"))
    test(bindingIds(WAE("{with {z 3} {with {w {with {z {+ 3 y}} {- x y}}} {with {w y} {+ 7 w}}}}")), Set("w", "z"))

    // tests for boundIds
    test(boundIds(WAE("{with {x 3} {+ y 3}}")), Set())
    test(boundIds(WAE("{with {x 3} {+ x {- x y}}}")), Set("x"))
    test(boundIds(WAE("{with {x 3} {+ x {with {y 7} {- x y}}}}")), Set("x", "y"))
    test(boundIds(WAE("{with {x 3} {with {y x} {- 3 y}}}")), Set("x", "y"))
    test(boundIds(WAE("{with {x 3} {+ y {with {y x} {- 3 7}}}}")), Set("x"))
    test(boundIds(WAE("{with {x x} {+ y {with {y y} {- 3 {with {z 7} {- z x}}}}}}")), Set("x", "z"))
    test(boundIds(WAE("{with {x {with {y 3} {+ x y}}} {+ y {with {y y} {- 3 7}}}}")), Set("y"))
    test(boundIds(WAE("{with {x a} {with {y b} {with {z c} {+ d {- x {+ y z}}}}}}")), Set("x", "y", "z"))
    test(boundIds(WAE("{+ {with {x 10} {with {x 3} {- y {with {y 7} {+ x {- c b}}}}}} {with {a d} a}}")), Set("a", "x"))
    test(boundIds(WAE("{+ {with {x 10} {with {x 3} {- y {with {y 7} {+ x {- c b}}}}}} {with {a d} z}}")), Set("x"))

    /* Write your own tests */
  }
}
