package lince.syntax

import lince.backend.SmallStep

import scala.util.Random

/**
 * Internal structure to represent terms in Lince 2.0.
 */

object Lince:

  case class Simulation(prog:Program, pi:PlotInfo)

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

  enum STL: // real-valued predicates and real-valued time
    case NOT
    case AND
    case OR
    case X // at next point in time
    case F // at some point in future
    case G // at all future points
    case U // x u y -> x is true until y is true and then only y is true
    case I // if first predicate true implies that second predicate is true

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

  case class PlotInfo( minTime:Double,
                       maxTime:Double,
                       maxLoops: Int,
                       samples: Int,
                       seed: Long,
                       showAll: Boolean,
                       showVar: String=>Boolean
  )
  object PlotInfo:
    def default = PlotInfo(0,10,50,20,SmallStep.rand.nextLong(),false,_=>true)


