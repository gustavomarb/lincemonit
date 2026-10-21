package lince.syntax

import cats.parse.Numbers.digits
import cats.parse.Parser.*
import cats.parse.{LocationMap, Numbers, Parser as P, Parser0 as P0}
import lince.syntax.Lince.{Cond, Expr, Inter, PlotInfo, Program, Simulation, Stl, eventually}
import Program.*
import caos.frontend.widgets.WidgetInfo.Simulate

import scala.sys.{env, error}
import scala.util.Random

object Parser :

  /** Parse a command  */
  def parseProgram(str:String):Program =
    pp(program,str) match {
      case Left(e) => error(e)
      case Right(c) => c
    }

  def parseSimulation(str: String): Simulation =
    pp(simulation, str) match {
      case Left(e) => error(e)
      case Right(c) => c
    }


  /** Applies a parser to a string, and prettifies the error message */
  private def pp[A](parser:P[A], str:String): Either[String,A] =
    parser.parseAll(str) match
      case Left(e) => Left(prettyError(str,e))
      case Right(x) => Right(x)

  /** Prettifies an error message */
  private def prettyError(str:String, err:Error): String =
    val loc = LocationMap(str)
    val pos = loc.toLineCol(err.failedAtOffset) match
      case Some((x,y)) =>
        s"""at (${x+1},$y):
           |<pre>${loc.getLine(x).getOrElse("-")}</br>${("-" * y)+"^\n"}</pre>""".stripMargin
      case _ => ""
    s"${pos}expected: ${err.expected.toList.mkString(", ")}\noffsets: ${
      err.failedAtOffset};${err.offsets.toList.mkString(",")}"

  // Simple parsers for spaces and comments
  /** Parser for a sequence of spaces or comments */
  private val whitespace: P[Unit] = P.charIn(" \t\r\n").void
  private val comment: P[Unit] = string("//") *> P.charWhere(_!='\n').rep0.void
  private val sps: P0[Unit] = (whitespace | comment).rep0.void

  // Parsing smaller tokens
  private def alphaDigit: P[Char] =
    P.charIn('A' to 'Z') | P.charIn('a' to 'z') | P.charIn('0' to '9') | P.charIn('_')
  private def varName: P[String] =
    (charIn('a' to 'z') ~ alphaDigit.rep0).string
  private def procName: P[String] =
    (charIn('A' to 'Z') ~ alphaDigit.rep0).string
  private def symbols: P[String] =
    // symbols starting with "--" are meant for syntactic sugar of arrows, and ignored as symbols of terms
    P.not(string("--")).with1 *>
    oneOf("+-><!%/*=|&".toList.map(char)).rep.string
  private def regExp: P[List[String]] =
    (alphaDigit | charIn("*. ()|+[]^!")).rep.string.map(_.trim).repSep(char(',')*>sps).map(_.toList)
//      .map(x => List(x))
  /** real number, e.g., 12 or 34.2 */
  def realP: P[Double] =
    (digits ~ (charIn('.')*>digits.map("."+_)).?)
      .map(x=>(x._1+x._2.getOrElse("")).toDouble)
  /** Positive integer */
  def intP: P[Int] = digits.map(_.toInt)

  //import scala.language.postfixOps

  private def simulation: P[Simulation] =
//    (sps.with1 *> program ~ plotInfo.? <* sps).map{
    (program ~ plotInfo.? <* sps).map {
        case (p,Some(pi)) => Simulation(p,pi)
        case (p,None) => Simulation(p, PlotInfo.default)
    }

  /** A program is a command with possible spaces or comments around. */
  private def program: P[Program] =
    statement.surroundedBy(sps).rep.map(l => l.tail.fold(l.head)(Program.Seq.apply))
//    (statement <* sps).rep.map(l => l.tail.fold(l.head)(Program.Seq.apply))

  private def block(rec: P[Program]): P[Program] =
    (char('{') *> sps *> (rec <* sps).rep0 <* char('}')).map(l =>
      if l.isEmpty then Skip else l.tail.fold(l.head)(Program.Seq.apply))

  private def statement: P[Program] = P.recursive((recSt: P[Program]) => {
    skip |
    ite(recSt) |
    whileP(recSt) |
    bern(recSt) |
    block(recSt) |
    waitP |
    ((varName <* sps) ~ (assign | diffEq | suffix) ).map (x => x._2 (x._1) )
  })

  def skip: P[Program] =
    (string ("skip") *> sps *> char (';') ).as(Skip)

  def ite(rec:P[Program]): P[Program] =
    ((string("if") *> sps *> cond ~
      (sps *> (string("then") *> sps).? *> // optional "then"
      rec <* sps)) ~
    (string("else") *> sps *> rec).?)
      .map(x => ITE(x._1._1,x._1._2,x._2.getOrElse(Skip)))

  def bern(rec: P[Program]): P[Program] =
    ((string("bernoulli") *> sps *> expr) ~
      (sps *> rec <* sps) ~
      rec)
      .map(x => ITE(Cond.Comp("<",Expr.Func("unif",Nil),x._1._1),
                    x._1._2, x._2))

  def whileP(rec:P[Program]): P[Program] =
    (string("while") *> sps *> cond ~
      (sps *> (string("do") *> sps).? *> // optional do
        rec <* sps))
      .map(x => While(x._1, x._2)) |
    (string("repeat") *> sps *> intP ~ (sps *> rec <* sps))
      .map(x => Seq(
        Assign("§c",Expr.Num(0)),
        While(Cond.Comp("<",Expr.Var("§c"),Expr.Num(x._1)),
          Seq(x._2,Assign("§c",Expr.Func("+",List(Expr.Var("§c"),Expr.Num(1))))))))

  def waitP: P[Program] =
    string("wait") *> sps *> expr.map(e => EqDiff(Map(),e)) <* sps <* char(';')

  def assign: P[String => Program] =
    (string(":=") *> sps *> expr <* sps <* char(';')).map(e => v => Assign(v,e))

  def diffEq: P[String => Program] =
    ((char('\'') *> sps *> char('=') *> sps *> expr <* sps) ~ // 1st expr
      ((char(',')*>sps*>varName) ~ (char('\'') *> sps *> char('=') *> sps *> expr <* sps)).rep0 ~ // (x2'=e2)*
      duration)//(string("for") *> sps *> expr <* (sps <* char(';')))) // for dur;
      .map{
        case ((e1,x2e2s),appDur) => x1 => appDur(Map(x1->e1)++x2e2s.toMap)
      }
  // "for" or "until" (syntactic sugar)
  def duration: P[Map[String,Expr] => Program] =
    string("for") *> sps *>
      expr.map(dur => eqs => EqDiff(eqs,dur)) <*
      (sps <* char(';')) |
    ((string("until_") *> expr) ~ (sps *> cond <* (sps <* char(';'))))
      .map((dur,c) => (eqs:Map[String,Expr]) => While(Cond.Not(c), EqDiff(eqs, dur)))


  def suffix: P[String => Program] =
    string("++") *> sps *> char(';')
      .as(v => Assign(v,Expr.Func("+",List(Expr.Var(v),Expr.Num(1))))) |
    string("--") *> sps *> char(';')
      .as(v => Assign(v, Expr.Func("-", List(Expr.Var(v), Expr.Num(1)))))

  def expr: P[Expr] = P.recursive((recExpr: P[Expr]) => {
    def literal: P[Expr] = P.recursive((recLit: P[Expr]) =>
      (char('(') *> recExpr.surroundedBy(sps) <* char(')')) |
      (char('-') *> recLit).map(x => Expr.Func("*",List(Expr.Num(-1),x))) |
      realP.map(x => Expr.Num(x)) |
//      (string("expn") *> sps *> char('(') *> sps *> recExpr <* (sps <* char(')')))
//        .map(lamb => Expr.Func("/",List(Expr.Func("*",List(Expr.Num(-1),
//                       Expr.Func("ln",List(Expr.Func("unif",Nil))))),lamb))) |
          // - ln ( unif ) / lambda
      (varName~(sps *> (char('(') *> sps *> recExpr.repSep0(sps~char(',')~sps) <* (sps <* char(')'))).?))
        .map {
          case (v, None) => Expr.Var(v)
          case (v, Some(args)) => preProcess(Expr.Func(v, args))
      })

    def pow: P[(Expr, Expr) => Expr] =
      string("^").as((x: Expr, y: Expr) => Expr.Func("^",List(x,y)))

    def mult: P[(Expr, Expr) => Expr] =
      string("*").as((x: Expr, y: Expr) => Expr.Func("*",List(x,y))) |
      string("/").as((x: Expr, y: Expr) => Expr.Func("/",List(x,y)))

    def sum: P[(Expr, Expr) => Expr] =
      string("+").as((x: Expr, y: Expr) => Expr.Func("+",List(x,y))) |
      string("-").as((x: Expr, y: Expr) => Expr.Func("-",List(x,y)))

    listSep(listSep(listSep(literal, pow), mult), sum)
  })

  /** Replaces some functions with its pre-processed equivalent. */
  def preProcess(f:Expr.Func): Expr = f match {
    case Expr.Func("expn",List(lamb)) => // - ln ( unif ) / lambda
      Expr.Func("/",List(Expr.Func("*",List(Expr.Num(-1),
        Expr.Func("ln",List(Expr.Func("unif",Nil))))),lamb))
    /* x1 := unif (0 ,1) ;
       x2 := unif (0 ,1) ;
       x := sqrt ( -2 * ( ln x1 ) ) * cos (2 * pi * x2 )
       x := m + s * x
     */
    case Expr.Func("normal",List(m,s)) => // - ln ( unif ) / lambda
      Expr.Func("+", List(
        m,
        Expr.Func("*",List(
          s,
          Expr.Func("*",List(
            Expr.Func("sqrt",List(
              Expr.Func("*",List(
                Expr.Num(-2.0),
                Expr.Func("ln",List(
                  Expr.Func("unif",Nil)
                ))
              ))
            )),
            Expr.Func("cos",List(
              Expr.Func("*",List(
                Expr.Func("*",List(
                  Expr.Num(2.0),
                  Expr.Func("pi",Nil)
                )),
                Expr.Func("unif",Nil)
              ))
            ))
          ))
        ))
      ))
           // xmin ⋅ (1−unif)^{−1/(alpha−1)}, where alpha>1 and xmin>0
     // (here using unif instead of 1-unif)
     case Expr.Func("powerlaw",List(alpha,xmin)) =>
      Expr.Func("*",List(
        xmin,
        Expr.Func("pow",List(
          Expr.Func("unif",Nil),
          Expr.Func("/",List(
            Expr.Num(-1),
            Expr.Func("-",List(
              alpha,
              Expr.Num(1)
            ))
          ))
        ))
      ))
    case _ => f
  }


  /** Parse a boolean condition */
  def cond: P[Cond] = P.recursive((recCond:P[Cond]) => {
    def lit: P[Cond] = P.recursive( (recLit:P[Cond]) =>
      string("true").as(Cond.True) |
      string("false").as(Cond.False) |
      (char('!') *> recLit).map(Cond.Not.apply) |
      ineq.backtrack |
      char('(') *> recCond.surroundedBy(sps) <* char(')')
    )

    def or: P[(Cond, Cond) => Cond] =
      (string("||")|string("\\/")).as(Cond.Or.apply)

    def and: P[(Cond, Cond) => Cond] =
      (string("&&")|string("/\\")).as(Cond.And.apply)

    listSep(listSep(lit, and), or)
  })

  def ineq =
    (expr ~ op.surroundedBy(sps) ~ expr).map(x => x._1._2(x._1._1, x._2))

  def op: P[(Expr, Expr) => Cond] = {
    (string("<=") | string(">=") | char('<') | char('>') | string("==") | string("!="))
      .string
      .map(op => ((e1, e2) => Cond.Comp(op, e1, e2)))
  }

/*
  def evenq =
    (eventl.surroundedBy(sps) ~ inter ~ stl).map(x => x._1._1(x._2, x._1._2))

  def eventl: P[(Stl, Inter) => Stl] = {
    (string("eventually"))
      .map(op => ((s, i) => eventually(s, i)))
  }
  /*def opuntils =
    (stl ~ opuntil.surroundedBy(sps) ~ inter ~ stl).map(x => x._1._1._2(x._1._1._1, x._2, x._1._2))*/

  def optemp: P[(Stl, Lince.Inter) => Stl] = {
    (string("eventually").as((x: Stl, y: Lince.Inter) => Lince.eventually(x, y)))
    (string("globally").as((x: Stl, y: Lince.Inter) => Lince.globally(x, y)))
  }
*/
  
  def inter: P[Lince.Inter] = {
    val double = Numbers.jsonNumber.map(_.toDouble)

    (P.char('[') *> double ~ (P.char(',').surroundedBy(sps) *> double).? <* P.char(']')).map {
      case (t1, t2) => Lince.Inter(t1, t2)
    }
  }
  
  
  //Stl parser
  def stl: P[Stl] = P.recursive((recStl: P[Stl]) => {

    def optem: P[(Stl) => Stl] = {
      ((string("eventually") *> sps *> inter.?)
        .map(i => (x: Stl) => Lince.eventually(x, i.getOrElse(Inter(0, None))))) |
      (((string("globally") | string("always")) *> sps *> inter.?)
        .map(i => (x: Stl) => Lince.globally(x, i.getOrElse(Inter(0, None)))))
    }
    
    def temp: P[Stl] = {
      (optem ~ recStl.surroundedBy(sps)).map((f,x) => f(x))
    }

    def lit: P[Stl] = P.recursive( (recLit:P[Stl]) =>
      string("true").as(Stl.Con(Cond.True)) |
        string("false").as(Stl.Con(Cond.False)) |
        ((char('!') | string("not") *> sps) *> recLit).map(Stl.Not.apply) |
        (string("next ") *> recLit).map(Stl.Next.apply) |
        temp |
        ineq.backtrack.map(Stl.Con.apply) |
        char('(') *> recStl.surroundedBy(sps) <* char(')'))
    
    def and: P[(Stl, Stl) => Stl] =
      ((string("&&") | string("and")).as((x: Stl, y: Stl) => Stl.And(x, y)))

    def or: P[(Stl, Stl) => Stl] =
      ((string("||") | string("or")).as((x: Stl, y: Stl) => Stl.Or(x, y)))

    def implies: P[(Stl, Stl) => Stl] =
      ((string("implies") | string("->")).as((x: Stl, y: Stl) => Lince.implies(x, y)))

    def untilOp2: P[(Stl, Stl) => Stl] =
      (string("until") *> sps *> inter.?)
        .map(i => (x: Stl, y: Stl) => Stl.U(x, y, i.getOrElse(Inter(0, None))))

    listSep(listSep(listSep(listSep(lit, and), or), implies), untilOp2)

  })

  /*def nextp: P[(Stl) => Stl] =
        //(string("next").as((x: Stl) => Stl.nextp(x)))
  
      def eventually: P[(Stl, Lince.Inter) => Stl] =
        (string("eventually").as((x: Stl, y: Lince.Inter) => Lince.eventually(x,y)))
  
      def globally: P[(Stl, Lince.Inter) => Stl] =
        (string("globally").as((x: Stl, y: Lince.Inter) => Lince.globally(x,y)))*/

  //listSep(listSep(lit, globally), eventually)
  
  /*def stl: P[Stl] =
    (string("monitor") *> sps *> stlExpr).surroundedBy(sps)
  
  private def stlExpr: P[Stl] = P.recursive((rec: P[Stl]) => {
    def literal: P[Stl] = P.recursive((recLit: P[Stl]) =>
        cond.map(c => Stl.Con(c))
    )
    
    listSep(listSep(listSep(literal, andP), orP), impliesP)*/


  def plotInfo: P[PlotInfo] =
    (char('-').rep *> sps *> (plotMod<*sps).rep).map(lst =>
      lst.foldLeft(PlotInfo.default)((p,f) => f(p))
    )

  def plotMod: P[PlotInfo => PlotInfo] =
    (string("monitor") *> sps *> stl).map(r =>
      (pi: PlotInfo) => pi.copy(monitor = r)) |
    (string("until") *> sps *> realP).map(r =>
      (pi:PlotInfo) => pi.copy(maxTime = r)) |
    (string("from") *> sps *> realP).map(r =>
      (pi:PlotInfo) => pi.copy(minTime = r)) |
    (string("iterations") *> sps *> intP).map(r =>
      (pi:PlotInfo) => pi.copy(maxLoops = r)) |
    (string("samples") *> sps *> intP).map(r =>
      (pi: PlotInfo) => pi.copy(samples = r)) |
    (string("samples") *> sps *> intP).map(r =>
      (pi: PlotInfo) => pi.copy(samples = r)) |
    (string("rk-samples") *> sps *> intP).map(r =>
      (pi: PlotInfo) => pi.copy(rkSamples = r)) |
    (string("seed") *> sps *> intP).map(r =>
      (pi: PlotInfo) => pi.copy(seed = r)) |
    (string("vars") *> sps *> regExp).map(r =>
      (pi: PlotInfo) => pi.copy(showVar = str => r.exists(re => re.r.matches(str)))) |
    (string("height") *> sps *> intP).map(r =>
      (pi: PlotInfo) => pi.copy(height = r)) |
    (string("runs") *> sps *> intP).map(r =>
        (pi: PlotInfo) => pi.copy(runs = r)) |
    (string("portrait") *> sps *>
      (varName ~ (sps *> char(',') *> sps *>(varName <* sps)))
        .repSep(char(';')*>sps)).map(lst =>
        (pi: PlotInfo) => pi.copy(portrait = lst.toList ::: pi.portrait)) |
    string("verbose").map(r =>
      (pi: PlotInfo) => pi.copy(showAll = true))

  //// Auxiliary functions

  def listSep[A](elem: P[A], op: P[(A, A) => A]): P[A] =
    (elem ~ (op.surroundedBy(sps).backtrack ~ elem).rep0)
      .map(x => {
        val pairlist = x._2
        val first = x._1;
        pairlist.foldLeft(first)((rest, pair) => pair._1(rest, pair._2))
      })
