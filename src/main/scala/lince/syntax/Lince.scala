package lince.syntax

import lince.backend.SmallStep

import scala.util.Random

/**
 * Internal structure to represent terms in Lince 2.0.
 */

object Lince:

  ///// Program ////

  enum Program:
    case Skip
    case Assign(v:String, e:Expr)
    case EqDiff(eqs:Map[String,Expr], dur:Expr)
    case Seq(p:Program, q:Program)
    case ITE(b:Cond, pt:Program, pf:Program)
    case While(b:Cond, p:Program)



  enum Ltl: // discrete time and boolean predicates
    case Variab(n:String)
    case Negati(e:Ltl)
    case AND(e1: Ltl, e2: Ltl)
    case OR(e1: Ltl, e2: Ltl)
    case X(e:Ltl) // at next point in time
    case F(e:Ltl) // at some point in future
    case G(e:Ltl) // at all future points
    case U(e1:Ltl, e2:Ltl) // x u y -> x is true until y is true and then only y is true

  def getexample: Ltl = Ltl.Negati(Ltl.Variab("x"))

  //intervalo tempo
  case class Inter(t1: Double, t2: Option[Double])
  //case class Inter(t1: Expr, t2: Option[Expr])

  enum Stl: // real-valued predicates and real-valued time
    case Not(e: Stl)
    case And(e1: Stl, e2: Stl)
    case Or(e1: Stl, e2: Stl)
    case U(e1: Stl, e2: Stl, ti: Inter) // x u y -> x is true until y is true and then only y is true
    case Con(e: Lince.Cond)
    //case nextp(e:Stl): Stl = ??? -> X
    //case X(e:STL) // at next point in time
    //case F(e:STL) // at some point in future
    //case G(e:STL) // at all future points
    //case I(e1: STL, e2:STL) // if first predicate true implies that second predicate is true "->"
  def implies(e1: Stl, e2: Stl): Stl = Stl.Or(Stl.Not(e1), e2)
  def eventually(e:Stl, ti: Inter): Stl = Stl.U(Stl.Con(Cond.True), e, ti)
  def globally(e:Stl, ti: Inter): Stl = Stl.Not(eventually(Stl.Not(e), ti))

  //criar exemplos e alterar parser. Depois funções 
  
  val ex1: Stl = Stl.Con(Cond.True)
  val ex2: Stl = Stl.Not(Stl.Con(Cond.Comp(">", Expr.Var("x"), Expr.Num(5))))
  val ex3: Stl = Stl.And(Stl.Con(Cond.Comp(">", Expr.Var("x"), Expr.Num(5))), Stl.Con(Cond.Comp("<", Expr.Var("x"), Expr.Num(10))))
  val ex4: Stl = Stl.U(Stl.Con(Cond.Comp(">", Expr.Var("x"), Expr.Num(5))), Stl.Con(Cond.Comp("<", Expr.Var("x"), Expr.Num(10))), Inter(1.3, Some(3.3)))
  val ex5: Stl = globally(Stl.Con(Cond.Comp(">", Expr.Var("x"), Expr.Num(5))), Inter(1.3, Some(3.33)))

    

  enum Cond:
    case True
    case False
    case Comp(op:String, e1:Expr, e2:Expr)
    case And(c1: Cond, c2: Cond)
    case Or(c1: Cond, c2: Cond)
    case Not(c: Cond)

  enum Expr:
    case Num(n:Double)
    case Var(x:String)
    case Func(op:String, es:List[Expr])

  ///// Actions ////

  enum Action:
    case Assign(v: String, n:Double)
    case DiffStop(eqs: Map[String, Expr], time: Double)
    case DiffSkip(eqs: Map[String, Expr], time: Double)
    case CheckIf(b: Cond, res:Boolean)
    case CheckWhile(b: Cond, res:Boolean)
    override def toString: String = Show(this)

  ///// Plot configuration ////

  case class Simulation(prog:Program, pi:PlotInfo):
    def state = SmallStep.St(prog,Map(),pi.seed+(pi.runs-1), pi.maxTime,pi.maxLoops)

  case class PlotInfo( minTime:Double,
                       maxTime:Double,
                       maxLoops: Int,
                       samples: Int,
                       seed: Long,
                       showAll: Boolean,
                       showVar: String=>Boolean,
                       height: Int, // height in px
                       runs: Int, // number of times to repeat the run
                       portrait: List[(String,String)], // to change the variables in the x and y axis
                       monitor: Stl
  )
  object PlotInfo:
    def default = PlotInfo(0,10,500,40,SmallStep.rand.nextLong(),false,_=>true,450,1,Nil, Stl.Con(Cond.True))


