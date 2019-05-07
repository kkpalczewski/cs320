package cs320

package object hw03 extends Homework03 {
  // we declare type for environment - because why not
  // WE HAVE TO HAVE  HERE MAPPING:
  type Env = Map[String, MRFWAEValue]
  type RecEnv = Map[String, MRFWAE]

  // First of all I add this type because we have to distuinguish between Numbers and Functions which are values
  trait MRFWAEValue
  case class NumV(n: Int) extends MRFWAEValue
  case class CloV(param: List[String],
                  body: MRFWAE,
                  env: Env) extends MRFWAEValue
  case class Record(name: RecEnv) extends MRFWAEValue

  // Specify operations: addition and subtraction
  // numAdd: (FWAE, FWAE) => FWAE
  def numAdd(x: MRFWAEValue, y: MRFWAEValue): MRFWAEValue = (x, y) match {
    case (NumV(n), NumV(m)) => NumV(n + m)
    case _ => error(s"not both numbers: $x, $y")
  }
  // numSub: (FWAE, FWAE) => FWAE
  def numSub(x: MRFWAEValue, y: MRFWAEValue): MRFWAEValue = (x, y) match {
    case (NumV(n), NumV(m)) => NumV(n - m)
    case _ => error(s"not both numbers: $x, $y")
  }

  // We specify lookup
  // lookup : (String, Env) => Int
  def lookup(name: String, env: Env): MRFWAEValue = env.get(name) match {
    case Some(v) => v
    case None => error(s"free identifier: $name")
  }

  def lookupRecord(name: String, exp: MRFWAE, env: Env): MRFWAE = interpMRFWAE(exp, env) match {
    case Record(expr) => expr.get(name) match {
      case Some(v) => v
      case None => error(s"no such field")
    }
    case _ => error(s"Access to non-record")

  }

  def multipleMappingCloV(param: List[String], args: List[MRFWAE], env: Env, fenv: Env): Env = {
    if(param.length != args.length) {
      error(s"wrong arity")
    }
    else {
      param match {
        case List() => fenv
        case x => multipleMappingCloV(param.tail, args.tail, env, fenv) + (param.head -> interpMRFWAE(args.head, env))
      }
    }
  }

  def multipleMappingRecord(recenv: RecEnv, oldenv: Env): RecEnv = {
    val emptyMap: RecEnv = Map()
    if(recenv == emptyMap) {
      emptyMap
    }
    else{
      val (k: String, v: MRFWAE) = recenv.head
      val new_v = interpMRFWAE(v, oldenv) match {
        case NumV(n) => Num(n)
        case CloV(x, b, env) => Fun(x, b)
      }
        multipleMappingRecord(recenv.tail, oldenv) + (k -> v)
      }
    }


  // we need to introduce interpreter
  def interpMRFWAE(expr: MRFWAE, env: Env): MRFWAEValue = expr match {
    case Num(n) => NumV(n)
    case Add(l, r) => numAdd(interpMRFWAE(l, env), interpMRFWAE(r, env))
    case Sub(l, r) => numSub(interpMRFWAE(l, env), interpMRFWAE(r, env))
    case With(x, i, b) =>interpMRFWAE(b, env + (x -> interpMRFWAE(i, env)))
    case Id(x) => lookup(x, env)
    case Fun(x, b) => CloV(x, b, env)
    case App(f, a) => interpMRFWAE(f, env) match {
      case CloV(x, b, fenv) => interpMRFWAE(b, multipleMappingCloV(x ,a, env, fenv))
      case v =>error(s"not a closure: $v")
    }
    case Rec(rec) => Record(multipleMappingRecord(rec, env)) // does record have to be in the same environment as functions and values
    case Acc(exp, name) => interpMRFWAE(lookupRecord(name, exp, env), env)
  }


  def interpMRFWAEValue(exprVal: MRFWAEValue): String = exprVal match {
    case NumV(n) => n.toString
    case Record(expr) => "record"
    case CloV(_, _, _) => "function"
  }

  def run(str: String): String = {
    interpMRFWAEValue(interpMRFWAE(MRFWAE(str), Map()))
  }

  def tests1: Unit = {
    //3 tests with
    test(run("{with {f {fun {x} x}} {f 1}}"), "1")
    testExc(run("{with {y 1} {with {z {fun {x y} x}} {z 3}}}"), "wrong arity")
    test(run("{with {y 1} {with {t {fun {x} {+ x x}}} {+ {t 4} y}}}"), "9")
    //3 tests with fun in fun
    test(run("{{fun {x y} {+ x y}} 1 {{fun {z w} {+ z w}} 1 2}}"), "4")
    test(run("{{fun {x y} {+ x y}} 1 {{fun {x w} {+ x w}} 1 2}}"), "4")
    testExc(run("{{fun {x y} {+ x y}} 1 {{fun {x x} {+ x w}} 1 2}}"), "duplicate parameters")


    test(run("{{fun {x y} {+ x y}} 1 2}"), "3")
    test(run("{{fun {} {+ 3 4}}}"), "7")
    testExc(run("{{fun {x y} {+ x y}} 1}"), "wrong arity")
    test(run("{access {record {x 1} {y 2}} x}"), "1")
    testExc(run("{access {record {x 1} {y 2}} z}"), "no such field")
    test(run("42"), "42")
    test(run("{fun {x} x}"), "function")
    test(run("{record {x 1}}"), "record")

    test(run("{with {x1 3} {{fun {x2} {+ {access x2 x3} {access x2 z}}} {record {x3 4} {z x1}}}}"), "7")

    testExc(run("{record {x {access {record {y 1}} z}}}"), "no such field")
    testExc(run("{record {x {record {y {access {record {y 1}} z}}}}}"), "no such field")

    testExc(run("{+{{fun {x y} {+x y}}1}1}"),"wrong arity")
    test(run("{access {record {x {fun {z} z}} {y 2}} x}"), "function")
    /* Write your own tests */
  }

  def tests: Unit = {
    test(run("{record {a 10} {b {+ 1 2}}}"), "record")
    test(run("{access {record {a 10} {b {+ 1 2}}} b}"), "3")
    test(run("{with {g {fun {r} {access r c}}} {g {record {a 0} {c 12} {b 7}}}}"), "12")
    test(run("{access {record {r {record {z 0}}}} r}"), "record")
    test(run("{access {access {record {r {record {z 0}}}} r} z}"), "0")
    test(run("{with {f {fun {a b} {+ a b}}} {with {g {fun {x} {- x 5}}} {with {x {f 2 5}} {g x}}}}"), "2")
    test(run("{with {f {fun {x y} {+ x y}}} {f 1 2}}"), "3")
    test(run("{with {f {fun {} 5}} {+ {f} {f}}}"), "10")
    test(run("{with {h {fun {x y z w} {+ x w}}} {h 1 4 5 6}}"), "7")
    test(run("{with {f {fun {} 4}} {with {g {fun {x} {+ x x}}} {with {x 10} {- {+ x {f}} {g 4}}}}}"), "6")
    test(run("{record {a 10} {b {+ 1 2}}}"), "record")
    test(run("{access {record {r {record {z 0}}}} r}"), "record")
    test(run("{access {access {record {r {record {z 0}}}} r} z}"), "0")
    test(run("{with {x 3} {with {y 5} {access {record {a x} {b y}} a}}}"), "3")
    test(run("{with {f {fun {a b} {+ {access a a} b}}} {with {g {fun {x} {+ 5 x}}} {with {x {f {record {a 10} {b 5}} 2}} {g x}}}}"), "17")
    test(run("{with {f {fun {a b c d e} {record {a a} {b b} {c c} {d d} {e e}}}} {access {f 1 2 3 4 5} c}}"), "3")
    test(run("{with {f {fun {a b c} {record {a a} {b b} {c c}}}} {access {f 1 2 3} b}}"), "2")
    test(run("{with {f {fun {a b c} {record {x a} {y b} {z c} {d 2} {e 3}}}} {access {f 1 2 3} y}}"), "2")
    test(run("{with {f {fun {a b c} {record {x a} {y b} {z c} {d 2} {e 3}}}} {access {f 1 2 3} d}}"), "2")
    test(run("{with {f {fun {x} {+ 5 x}}} {f {access {access {record {a {record {a 10} {b {- 5 2}}}} {b {access {record {x 50}} x}}} a} b}}}"), "8")
    test(run("{access {record {a 10} {b {+ 1 2}}} b}"), "3")
    test(run("{access {record {r {record {z 0}}}} r}"), "record")
    test(run("{access {access {record {r {record {z 0}}}} r} z}"), "0")
    test(run("{access {record {a 10}} a}"), "10")
    test(run("{access {record {a {+ 1 2}}} a}"), "3")
    test(run("{fun {x} x}"), "function")
    test(run("{access {record {a {record {b 10}}}} a}"), "record")
    test(run("{access {access {record {a {record {a 10}}}} a} a}"), "10")
    test(run("{access {access {record {a {record {a 10} {b 20}}}} a} a}"), "10")
    test(run("{access {access {record {a {record {a 10} {b 20}}}} a} b}"), "20")
    test(run("{+ {access {record {a 10}} a} {access {record {a 20}} a}}"), "30")
    test(run("{record {a {- 2 1}}}"), "record")
    test(run("{access {record {a 10}} a}"), "10")
    test(run("{access {record {a {- 2 1}}} a}"), "1")
    test(run("{access {record {a {record {b 10}}}} a}"), "record")
    test(run("{access {access {record {a {record {a 10}}}} a} a}"), "10")
    test(run("{access {access {record {a {record {a 10} {b 20}}}} a} a}"), "10")
    test(run("{access {access {record {a {record {a 10} {b 20}}}} a} b}"), "20")
    test(run("{access {record {r {record {z 0}}}} r}"), "record")
    test(run("{access {access {record {r {record {z 0}}}} r} z}"), "0")
    test(run("{with {y {record {x 1} {y 2} {z 3}}} {access y y}}"), "2")
    test(run("{with {y {record {x 1} {y 2} {z 3}}} {access y z}}"), "3")
    test(run("{record {a 10} {b {+ 1 2}}}"), "record")
    test(run("{access {record {a 10} {b {+ 1 2}}} b}"), "3")
    test(run("{with {g {fun {r} {access r c}}} {g {record {a 0} {c 12} {b 7}}}}"), "12")
    test(run("{access {record {r {record {z 0}}}} r}"), "record")
    test(run("{access {access {record {r {record {z 0}}}} r} z}"), "0")
    testExc(run("{access {record {b 10} {b {+ 1 2}}} b}"), "duplicate fields")
    testExc(run("{access {record {a 10}} b}"), "no such field")
    testExc(run("{record {z {access {record {z 0}} y}}}"), "no such field")
  }
}
