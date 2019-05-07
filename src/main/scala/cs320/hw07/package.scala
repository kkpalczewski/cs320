package cs320

package object hw07 extends Homework07 {

    // values and environment
  trait KXCFAEValue
  case class NumV(n: Int) extends KXCFAEValue
  case class CloV(param: List[String], body: KXCFAE, env: Env) extends KXCFAEValue
  case class ContV(proc: Cont) extends KXCFAEValue
  case class ListV(valList: List[KXCFAEValue]) extends KXCFAEValue //value for storing list of parameters
  case class ThrowV() extends KXCFAEValue
  type Env = Map[String, KXCFAEValue]
  type Cont = KXCFAEValue => KXCFAEValue

  //TODO: Ogarnij tą funkcję
  // numAdd: (KXCFAEValue, KXCFAEValue) => KXCFAEValue
  def numVAdd(x: KXCFAEValue, y: KXCFAEValue): KXCFAEValue = (x, y) match {
    case (NumV(n), NumV(m)) => NumV(n + m)
    case _ => error(s"not both numbers: $x, $y")
  }

  //TODO: ogarnij tą funkcję
  // numSub: (KXCFAEValue, KXCFAEValue) => KXCFAEValue
  def numVSub(x: KXCFAEValue, y: KXCFAEValue): KXCFAEValue = (x, y) match {
    case (NumV(n), NumV(m)) => NumV(n - m)
    case _ => error(s"not both numbers: $x, $y")
  }

  // multipleInterp: (List[KXCFAEValue], Env, Cont): List[KXCFAEValue]
  def multipleInterp(kxcfaeToInterp: List[KXCFAE],kxcfaeInterpreted: List[KXCFAEValue], env: Env, k: Cont): KXCFAEValue = kxcfaeToInterp match {
      //TODO: zlikwiduj namiastke
    case head :: tail => interp(kxcfaeToInterp.last, env, newElem =>
      multipleInterp(kxcfaeToInterp.init, newElem::kxcfaeInterpreted, env, k))
    case Nil => k(ListV(kxcfaeInterpreted))
  }

  def multipleMappingEnv(args: List[String], values: List[KXCFAEValue], env: Env): Env = args match{
    case head :: tail => multipleMappingEnv(args.tail, values.tail, env + (args.head -> values.head))
    case _ => env
  }

  def interp(kxcfae: KXCFAE, env: Env, k: Cont): KXCFAEValue = kxcfae match {
    case Num(n) => k(NumV(n))
    case Add(l, r) => interp(l, env, lv =>
      interp(r, env, rv =>
        k(numVAdd(lv, rv))))
    case Sub(l, r) =>interp(l, env, lv =>
      interp(r, env, rv =>
        k(numVSub(lv, rv))))
    case Id(x) =>k(env.getOrElse(x, error(s"free identifier: $x")))

    case Fun(p, b) => k(CloV(p, b, env))
    case App(f, a) =>
      interp(f, env, fv =>
        // take list of arguments a, and convert it to list of values av
        multipleInterp(a, List(), env, av =>
          av match {
            case ListV(values) =>
              fv match {
                case CloV(p, b, fenv) =>
                  if(values.length == p.length){
                    interp(b, multipleMappingEnv(p, values, fenv), k)
                  }
                  else{
                    error(s"wrong arity")
                  }
                case ContV(kv) =>
                  if(values.length == 1){kv(values.head)}
                  else {error(s"wrong arity")}
                case v => error("not a closure: $v")
              }
            case _ => error(s"improper list of arguments")
          }
        )
      )
      //TODO: ogarnij tą funkcję
    case If0(c, t, f) => interp(c, env, k) match {
      case NumV(0) => k(interp(t, env, k))
      case _ => k(interp(f, env, k))
    }
    case Withcc(x, b) =>
      interp(b, env + (x -> ContV(k)), k)

    case Try(tryE: KXCFAE, catchE: KXCFAE) =>
      val out = interp(tryE, env, k)
      out match {
        case ThrowV() => interp(catchE, env, k)
        case _ => out
      }

    case Throw =>
      ThrowV()
    // throw an error throw if a continuation
  }


  def run(str: String): String = {
    // TODO: check if this identity is ok
    def identity(x:KXCFAEValue): KXCFAEValue = x
    val out = interp(KXCFAE(str), Map(), identity)
    out match{
      case NumV(n) => n.toString
      case CloV(_,_,_) => "function"
      case ContV(_) => "continuation"
      case ListV(l) => s"ListV as an output: $l"
      case ThrowV() => error(s"no enclosing try-catch")
    }
  }

  def tests: Unit = {
    test(run("{{fun {x y} {- y x}} 10 12}"), "2")
    test(run("{fun {} 12}"), "function")
    testExc(run("{{fun {x y} 1} 2}"), "wrong arity")
    test(run("{withcc esc {{fun {x y} x} 1 {esc 3}}}"), "3")
    test(run("{{fun {x y} {- y x}} 10 12}"), "2")
    test(run("{fun {} 12}"), "function")
    testExc(run("{{fun {x y} 1} 2}"), "wrong arity")
    test(run("{withcc esc {{fun {x y} x} 1 {esc 3}}}"), "3")
    test(run("{try 1 catch 2}"), "1")
    test(run("{try {throw} catch 2}"), "2")
    test(run("{withcc esc {+ 1 {esc 3}}}"), "3")
    test(run("{try {+ 1 {throw}} catch 2}"), "2")
    test(run("{{fun {f} {try {f} catch 1}} {fun {} {throw}}}"), "1")
    testExc(run("{throw}"), "no enclosing try-catch")
    test(run("{withcc cont {try {cont {throw}} catch {cont cont}}}"), "continuation")
    testExc(run("{try {throw} catch {throw}}"), "no enclosing try-catch")
    testExc(run("{withcc esc {esc}}"), "wrong arity")
    test(run("{try {{fun {x y} x} 1 {throw}} catch 2}"), "2")
    test(run("{+ 1 {withcc k {{fun {x y} {+ x y}} {k 2} 4}}}"),"3")
    test(run("{withcc done {{withcc esc {done {+ 1 {withcc k {esc k}}}}} 3}}"),"4")
    //testExc(run("{{fun {x y} {+ x y}} {1 2}}"), "wrong arity")
    /* Write your own tests */
  }
}
