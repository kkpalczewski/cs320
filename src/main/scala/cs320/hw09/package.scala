package cs320

package object hw09 extends Homework09 {

  // Values
  trait CORELValue
  case class NumV(n: Int) extends CORELValue
  case class CloV(param: String, body: COREL, var env: Env) extends CORELValue
  case class BoolV(bl: Boolean) extends CORELValue
  // Możemy zaaplikować konstruktor np. apple do parametru i wtedy mamy variant
  // Mozemy rozwinąć Variant używając case i wtedy szukamy nazwy w środowisku
  // Ze środowiska bierzemy wyrażenie i aplikujemy parametr
  // VariantV jest do rozwinięcia tylko w Case !!!
  case class VariantV(name: String, value: CORELValue) extends CORELValue
  // ConstructorV is one of the field of the new type that we create
  case class ConstructorV(name: String) extends CORELValue



  // EnvironmentsApp
  type Env = Map[String, CORELValue]
  case class TypeEnv(vars  : Map[String, Type] = Map(),
                    //tbinds: Map[FromCOnstructorV, Map[FromVariantName, FromVariantValueType]]
                     tbinds: Map[String, Map[String, Type]] = Map()
                    ) {
    def addVar(x: String, t: Type): TypeEnv = copy(vars = vars + (x -> t))
    def addTBind(x: String, cs: Map[String, Type]): TypeEnv =
      copy(tbinds = tbinds + (x -> cs))
  }


  // Helper functions for type checker
  // [IMPORTANT]: when we use mustSame function we assume that the type IdT exists!
  // since we have no option to check wheather the type is in tyEnv or not
  def mustSame(left: Type, right: Type): Type =
    if (same(left, right)) left
    else notype(s"$left is not equal to $right")

  def same(left: Type, right: Type): Boolean = (left, right) match {
    case (NumT, NumT) => true
    case (BoolT, BoolT) => true
    case (ArrowT(p1, r1), ArrowT(p2, r2)) =>same(p1, p2) && same(r1, r2)
    case (IdT(name1), IdT(name2)) =>
      if(name1 == name2){true}
      else{false}
    case _ => false}

  def notype(msg: Any): Nothing = error(s"no type: $msg")

  def validType(ty: Type, tyEnv: TypeEnv): Type = ty match {
      //TODO: we we use this instead of typeCheckEval??
    case NumT => ty
    case BoolT => ty
    case ArrowT(p, r) =>ArrowT(validType(p, tyEnv), validType(r, tyEnv))
    case IdT(x) =>
      if (tyEnv.tbinds.contains(x)) ty
      else notype(s"$x is a free type")
  }

  // extend environment by constructor + variants and validate type of variants
  def WithTypeHelperTypeCheck(name: String, constructor: Map[String, Type], tyEnv: TypeEnv): TypeEnv = constructor.isEmpty match{
    case true => tyEnv
    case false =>
      //TODO: dlaczego to pierwsze leci>??
      val (vname, vtype) = constructor.head
      validType(vtype, tyEnv)
      val tyEnvV = tyEnv.addVar(vname, ArrowT(vtype, IdT(name)))
      WithTypeHelperTypeCheck(name, constructor.tail, tyEnvV)
  }

  def CasesHelperTypeCheck(name: String, variants: Map[String, Type], cases: Map[String, (String, COREL)], tyEnv: TypeEnv): Type = {
    // variants [nazwa_variantu, typ_variantu]
    //cases [nazwa_variantu, (parametr, wyrażenie_do_ewaluacji)]
    if(variants.isEmpty){
      if(cases.isEmpty){IdT(name)}
      else{notype(s"'not all cases for type $name")}
    }
    else{
      if(cases.isEmpty){notype(s"not all cases for type $name")}
      else{
        val (varName, varType) = variants.head
        val (caseParam, caseExpr) = cases.getOrElse(varName, notype(s" not all cases for Variant $varName"))
        val tyEnvV = tyEnv.addVar(caseParam, varType)
        val caseType = typeCheckEval(caseExpr, tyEnvV)
        //TODO: check dlaczego tak
        mustSame(caseType, varType)
        CasesHelperTypeCheck(name, variants.tail, cases - varName, tyEnvV)
      }
    }
  }


  // Type checking
  def typeCheckEval(corel: COREL, tyEnv: TypeEnv): Type = corel match{
    case Num(_) => NumT
    case Bool(_) => BoolT
    case Add(l, r) =>
      mustSame(typeCheckEval(l, tyEnv), NumT)
      mustSame(typeCheckEval(r, tyEnv), NumT)
      NumT
    case Sub(l, r) =>
      mustSame(typeCheckEval(l, tyEnv), NumT)
      mustSame(typeCheckEval(r, tyEnv), NumT)
      NumT
    case Equ(l, r) =>
      mustSame(typeCheckEval(l, tyEnv), NumT)
      mustSame(typeCheckEval(r, tyEnv), NumT)
      BoolT
    case With(name, ty, expr, body) =>
      //TODO: Przypadki testowe z nieistniejacymi typami albo złymi typami
      validType(ty, tyEnv)
      mustSame(typeCheckEval(expr, tyEnv), ty)
      //TODO: With could also take COnstructorV or VariantV???
      //TODO: czy tu potrzebny jest ten validType??
      validType(typeCheckEval(body, tyEnv.addVar(name, ty)), tyEnv)
      //TODO: Testy z Id !!!!!!?????
      //TODO: Czy tu trzeba dodać walidowanie typu?
      //Id could be constructor -> what then??
    case Id(name) => tyEnv.vars.getOrElse(name,notype(s"$name is a free identifier"))
    case Fun(p, t, b) =>
      validType(t, tyEnv)
      ArrowT(t, typeCheckEval(b, tyEnv.addVar(p, t)))
    case App(f, a) =>
      val funT = typeCheckEval(f, tyEnv)
      validType(funT, tyEnv)
      val argT = typeCheckEval(a, tyEnv)
      funT match {
        case ArrowT(param, result)
          if same(argT, param) => result
        case _ => notype(s"apply $argT to $funT")}
    case IfThenElse(testE, thenE, elseE) =>
      mustSame(typeCheckEval(testE, tyEnv),  BoolT)
      //TODO: przypadek testowy na złe typy
      mustSame(typeCheckEval(thenE, tyEnv), typeCheckEval(elseE, tyEnv))
    case Rec(f, ft, x, xt, b) => ft match {
      case ArrowT(pt, rt) =>
        validType(pt, tyEnv)
        validType(rt, tyEnv)
        mustSame(pt, xt)
        mustSame(rt, typeCheckEval(b, tyEnv.addVar(f,ft).addVar(x,xt)))
        ft
      case _ => notype(s"$ft is not an arrow type")
    }
    case WithType(name, constructor, body) =>
      val tyEnvT = tyEnv.addTBind(name, constructor)
      val tyEnvV = WithTypeHelperTypeCheck(name, constructor, tyEnvT)
      typeCheckEval(body, tyEnvV)
    case Cases(name:String, dispatchE: COREL, cases: Map[String, (String, COREL)]) =>
      //we get all the Variant of the IdT(name) type
      val variants = tyEnv.tbinds.getOrElse(name, notype(s"$name is a free type"))
      mustSame(typeCheckEval(dispatchE, tyEnv), IdT(name))
      // znajdz kazdego i porownaj potem
      //TODO: tutaj coś validujemy??
      CasesHelperTypeCheck(name, variants, cases, tyEnv)
  }

  def typeCheck(str: String): Type = {
    typeCheckEval(COREL(str), TypeEnv(Map(), Map()))
  }


  // Helpter functions for interpreter
  // numAdd: (CORELValue, CORELValue) => CORELValue
  def numVAdd(x: CORELValue, y: CORELValue): CORELValue = (x, y) match {
    case (NumV(n), NumV(m)) => NumV(n + m)
    case _ => error(s"not both numbers: $x, $y")
  }
  // numSub: (FWAE, FWAE) => FWAE
  def numVSub(x: CORELValue, y: CORELValue): CORELValue = (x, y) match {
    case (NumV(n), NumV(m)) => NumV(n - m)
    case _ => error(s"not both numbers: $x, $y")
  }
  def WithTypeHelperInterp(constructor: Map[String, Type], env: Env): Env = constructor.isEmpty match{
    case true => env
    case false =>
      val (vname, _) = constructor.head
      WithTypeHelperInterp(constructor.tail, env + (vname -> ConstructorV(vname)))
  }

  // Interp
  def interpEval(corel: COREL, env: Env): CORELValue = corel match{
    case Num(n) => NumV(n)
    case Bool(bl) => BoolV(bl)
    case Add(l, r) => numVAdd(interpEval(l, env), interpEval(r, env))
    case Sub(l, r) => numVSub(interpEval(l, env), interpEval(r, env))
    case Equ(l, r) =>
      val lv = interpEval(l, env)
      val rv = interpEval(r, env)
      if(lv == rv){
        BoolV(true)
      }
      else{
        BoolV(false)
      }
    case With(name, _, expr, body) =>
      val nameVal = interpEval(expr, env)
      interpEval(body, env + (name -> nameVal))
    case Id(x) => env.getOrElse(x, error(s"free identifier: $x"))
    case Fun(p, _, b) => CloV(p, b, env)
    case App(f, a) => interpEval(f, env) match {
      case CloV(param, b, fenv) => interpEval(b, fenv + (param -> interpEval(a, env)))
      //TODO: przypadek z VariantV jako output
      case ConstructorV(name) => VariantV(name, interpEval(a, env))
      case fv => error(s"not a closure: $fv")}
    case IfThenElse(testE, thenE, elseE) => interpEval(testE, env) match{
      case BoolV(true) => interpEval(thenE, env)
      case BoolV(false) => interpEval(elseE, env)
    }
    case Rec(name, _, param, _, body) =>
      val cloV = CloV(param, body, env)
      cloV.env = env + (name -> cloV)
      cloV
    case WithType(name, constructors, body) =>
      //TODO: dlaczeog tutaj name nie używamy??
      interpEval(body, WithTypeHelperInterp(constructors, env))
    case Cases(_, dispatchE,cases) =>
      interpEval(dispatchE, env) match{
        case VariantV(name, value) =>
          val (caseParam, caseExpr) = cases.getOrElse(name, notype(s"$name is a free constructor"))
          interpEval(caseExpr, env + (caseParam -> value))

        case v => error(s"not a variant: $v")
      }
  }

  def interp(str: String): String = {
    val out = interpEval(COREL(str), Map())
    out match{
      case NumV(n) => n.toString
      case CloV(_,_,_) => "function"
      case BoolV(bl) => bl.toString
      case VariantV(_, _) => "variant"
    }
  }


  // Tests
  def tests: Unit = {
    test(run("42"), "42")
    test(run("true"), "true")
    test(run("{+ 1 2}"), "3")
    test(run("{- 2 1}"), "1")
    test(run("{= 1 0}"), "false")
    testExc(run("{= true true}"), "no type")
    testExc(run("{= true 0}"), "no type")
    testExc(run("{+ true 0}"), "no type")
    testExc(run("{- true 0}"), "no type")

    test(run("{with {x : num 1} x}"), "1")
    testExc(run("{with {x : boooool 1} x}"), "no type")
    testExc(run("{with {x : bool 1} x}"), "no type")
    testExc(run("{with {x : bool true} {= x false}}"), "no type")

    test(run("{{fun {x : num} {+ x 1}} 2}"), "3")
    testExc(run("{{fun {y : bool} {+ y 1}} 2}"), "no type")

    test(run("""
      {{recfun {f: {num -> num} x: num}
               {if {= x 0} 0 {+ {f {- x 1}} x}}}
       10}"""), "55")
    test(run("""
      {{recfun {f: {num -> num} x: num}
               {if {= x 0} 0 {+ {f {- x 1}} x}}}
       10}"""), "55")


    test(run("""
        {if true
          {{recfun {f: {num -> num} x: num}
            {if {= x 0}
              0
              {+ {f {- x 1}} x}}}
            10}
          3}"""), "55")
    testExc(run("{if 1 2 3}"), "no type")
    testExc(run("{if true true 3}"), "no type")
    test(run("""
      {withtype
        {fruit {apple num}
               {banana num}}
        1}"""), "1")
    test(run("""
      {withtype
        {fruit {apple num}
               {banana num}}
        {cases fruit {apple 1}
               {apple {x} x}
               {banana {y} y}}}"""), "1")
    testExc(run("""
      {withtype
        {fruit {apple num}
               {banana num}}
        {cases fruit {apple 1}
               {apple {x} x}}}"""), "not all cases")
    testExc(run("""
      {withtype
        {fruit {apple num}}
        {cases fruit {apple 1}
               {apple {x} x}
               {banana {y} y}}}"""), "not all cases")
    /* Write your own tests */
  }
}
