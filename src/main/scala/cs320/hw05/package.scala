package cs320

//TODO:
// 1) interpret classical SRSRBFAE
// 2) generalize RBRFAE

package object hw05 extends Homework05 {
  // additional types
  type Addr = Int
  type Sto = Map[Addr, SRBFAEValue]
  type Env = Map[String, Addr]

  // trait for values
  trait SRBFAEValue
  case class NumV(n: Int) extends SRBFAEValue
  case class CloV(x: String, b: SRBFAE, env: Env) extends SRBFAEValue
  case class BoxV(addr: Addr) extends SRBFAEValue
  //TODO: ogarni czy nie lepiej użyć Store
  case class RecV(fields: Map[String, Addr]) extends SRBFAEValue

  // malloc : Sto => Addr
  def malloc(sto: Sto): Addr =
    maxAddress(sto) + 1

  // maxAddress: Sto => Addr
  def maxAddress(sto: Sto): Addr =
    sto.keySet.+(0).max

  // numAdd: (SRBFAE, SRBFWAE) => SRBFWAE
  def numVAdd(x: SRBFAEValue, y: SRBFAEValue): SRBFAEValue = (x, y) match {
    case (NumV(n), NumV(m)) => NumV(n + m)
    case _ => error(s"not both numbers: $x, $y")
  }
  // numSub: (FWAE, FWAE) => FWAE
  def numVSub(x: SRBFAEValue, y: SRBFAEValue): SRBFAEValue = (x, y) match {
    case (NumV(n), NumV(m)) => NumV(n - m)
    case _ => error(s"not both numbers: $x, $y")
  }
  //def seqMultiple()
  //lookup: (String, Env) => Addr
  def lookup(name: String, env: Env): Addr = env.get(name) match {
    case Some(v) => v
    case None => error(s"free identifier: $name")
  }

  //storeLookup: (Addr, Sto) => SRBValue
  //TODO: ogarnij tą funckję czy jest spoko
  def storeLookup(addr: Addr, sto: Sto): SRBFAEValue = sto.get(addr) match {
    case Some(v) => v
    case None => error(s"no value : $addr")
  }

  def matchRecAddr(fieldsToMap: Map[String, SRBFAE], mappedFields: Map[String, Addr], sto: Sto, env:Env): (Map[String, Addr], Sto) = {
    val strMap = fieldsToMap.toString
    if(strMap == "Map()") {
      (mappedFields, sto)
    }
    else{
      val (vname, vexpr) = fieldsToMap.head
      val (vval, vsto) = interp(vexpr, env, sto)
      val addr = malloc(vsto)
      matchRecAddr(fieldsToMap.tail, mappedFields + (vname -> addr),vsto + (addr -> vval),env)

    }

  }

  // interp : (srbfae, env, sto) => (bfaevalue, sto)
  def interp(srbfae: SRBFAE, env: Env, sto: Sto): (SRBFAEValue, Sto) =
    srbfae match {
      case Num(n) => (NumV(n), sto)
      case Add(l, r) =>
        val (lv, ls) = interp(l, env, sto)
        val (rv, rs) = interp(r, env, ls)
        (numVAdd(lv, rv), rs)
      case Sub(l, r) =>
        val (lv, ls) = interp(l, env, sto)
        val (rv, rs) = interp(r, env, ls)
        (numVSub(lv, rv), rs)
      case Id(name) =>
        (storeLookup(lookup(name, env), sto), sto)
      case Fun(param, body) => (CloV(param, body, env), sto)

        //TODO: ogarnij czy ten kawałek kodu jest spoko
        //TODO: ogarnij dlaczego potrzebujemy oddzielny przypadke dla ID

      case App(f, a) => a match {
        case Id(name) => // call-by-ref handling for Id arg:
          val (fv, fs) = interp(f, env, sto)
          fv match {
            case CloV(x, b, fenv) =>
              val addr = lookup(name, env)
              interp(b, fenv + (x -> addr), fs)
            case _ => error(s"not a closure: $fv")
          }
        case _ => // as before:
          val (fv, fs) = interp(f, env, sto) //najpierw evaluate function
          val (av, as) = interp(a, env, fs) //evaluate funkcję do parametru
          fv match {
            case CloV(x, b, fenv) =>
              val addr = malloc(as)
              interp(b, fenv + (x -> addr), as + (addr -> av))
            case _ => error(s"not a closure: $fv")
          }
      }

      case NewBox(e) =>
        val (v, s) = interp(e, env, sto)
        val addr = malloc(s)
        (BoxV(addr), s + (addr -> v))

      case SetBox(b, e) =>
        val (bv, bs) = interp(b, env, sto)
        bv match {
          case BoxV(addr) =>
            val (v, s) = interp(e, env, bs)
            (v, s + (addr -> v))
          case _ => error(s"not a box: $bv")
        }

      case OpenBox(b) =>
        val (bv, bs) = interp(b, env, sto)
        bv match {
          case BoxV(addr) =>
            (storeLookup(addr, bs), bs)
          case _ => error(s"not a box: $bv")
        }

      case Seqn(l, r) =>
        val (lv, ls) = interp(l, env, sto)
        //interp(r, env, ls)
        //TODO: here should be dostosowanie Seqn do nas
        r match {
          case List() => (lv, ls)
          case _ => interp (Seqn(r.head, r.tail), env, ls)
      }
      //Add rec and get forms for records, and also add set form that modifies the value of a record field:
      //rec - allocates a new record.
      //get - accesses from the record produced by the sub-expression the value of the field named by the identifier.
      //set - changes within the record produced by the first sub-expression the value of the field named by the identifer;
      //    - the value of the second sub-expression determines the field’s new value,
      //    - that value is also the result of the set expression.
      //
      //Note that changes within the record using the field name that does not exist in the record print error messages
      // containing "no such field". A record is a mutable data structure as same as a box

        // record is not stored anywhere it is just record !!!!!!!!!!!!!!!!!!!!!!!!!!!
      case Rec(e) =>
        val (rmap, rsto) = matchRecAddr(e, Map(), sto, env)
        (RecV(rmap), rsto)

      case Get(record, field) =>
        val (rec, recsto) = interp(record, env, sto)
        rec match {
          case RecV(e) =>
            e.get(field) match{
              case Some(addr) =>
                (storeLookup(addr, recsto), recsto)
              case None => error(s"no such field")
            }
        }

      case Set(record, field, expr) =>
        val (rec, recsto) = interp(record, env, sto)
        rec match {
          case RecV(e) =>
            e.get(field) match{
              case Some(addr) =>
                val (vale, vsto) = interp(expr, env, recsto)
                (vale, recsto + (addr -> vale))
              case None => error(s"no such field")
            }
        }


    }

  def run(str: String): String = {
    val (outVal, outStore) = interp(SRBFAE(str), Map(), Map())
    outVal match {
      case NumV(n) => n.toString
      case CloV(_,_,_) => "function"
      case BoxV(_) => "box"
      case RecV(_) => "record"
    }
  }

  def tests: Unit = {
    test(run("""{{fun {b} {seqn {setbox b {+ 2 {openbox b}}}
                          {setbox b {+ 3 {openbox b}}}
                          {setbox b {+ 4 {openbox b}}}
                          {openbox b}}}
                {newbox 1}}"""), "10")
    testExc(run("{get {rec {x 1}} y}"), "no such field")
    test(run("{{fun {r} {seqn {set r x 5} {get r x}}} {rec {x 1}}}"), "5")
    test(run("42"), "42")
    test(run("{fun {x} x}"), "function")
    test(run("{newbox 1}"), "box")
    test(run("{rec}"), "record")
    test(run("{{fun {x} {get {rec {x 1} {y 2} {z 3}} x}} {newbox 8}}"), "1")
    test(run("{{fun {r} {seqn {set r x {newbox 8}} {get r x}}} {rec {x {newbox 7}}}}"),"box")
    test(run("{openbox {get {rec {x {newbox 12}}} x}}"), "12")
    test(run(" { openbox {{  fun {b} { get { rec { x {setbox b 3} } {y b}  }  y } }  {newbox 1}} } " ),"3")
    testExc(run("{seqn {rec {a 1}} {get {rec {b 2}} a}}"),"no such field")
    testExc(run(" {{fun {x} {get {rec {a 1}} b}} {rec {b 2}}}"), "no such field")
    /* Write your own tests */
  }
}
