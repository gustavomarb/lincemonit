package lince.syntax

import lince.backend.SmallStep
import lince.syntax.Lince.Expr.Var

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

  enum Stl: // real-valued predicates and real-valued time
    //case VAR(n:String)
    case Not(e:Stl)
    case And(e1: Stl, e2:Stl)
    case Or(e1: Stl, e2:Stl)
    case U(e1: Stl, e2: Stl, ti: Inter) // x u y -> x is true until y is true and then only y is true
    case Cond(e:Lince.Cond)
    //case X(e:STL) // at next point in time
    //case F(e:STL) // at some point in future
    //case G(e:STL) // at all future points
    //case I(e1: STL, e2:STL) // if first predicate true implies that second predicate is true "->"
    def implies(e1:Stl, e2:Stl): Stl = Or(Not(e1), e2)
  
  //criar exemplos e alterar parser. Depois funções
  //
  val ex1: Stl = Stl.Cond(Cond.True)
  val ex2: Stl = Stl.Not(Stl.Cond(Cond.Comp(">", Expr.Var("x"), Expr.Num(5))))
  val ex3: Stl = Stl.And(Stl.Cond(Cond.Comp(">", Expr.Var("x"), Expr.Num(5))), Stl.Cond(Cond.Comp("<", Expr.Var("x"), Expr.Num(10))))
  val ex4: Stl = Stl.U(Stl.Cond(Cond.Comp(">", Expr.Var("x"), Expr.Num(5))), Stl.Cond(Cond.Comp("<", Expr.Var("x"), Expr.Num(10))), Inter(1.3, Some(3.3)))

  //intervalo tempo
  case class Inter(t1:Double,t2:Option[Double])


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
                       monitor: Option[Stl]
  )
  object PlotInfo:
    def default = PlotInfo(0,10,50,40,SmallStep.rand.nextLong(),false,_=>true,450,1, None)

