package lince.backend

import lince.syntax.Lince.{Cond, Expr, Stl, Inter}
import lince.syntax.{Lince, Show}
import lince.backend.Eval.Valuation

import scala.util.Random
import lince.{backend => ArrayStream}
import lince.{backend => valuation}

object CheckStream:

//complexidade linear em "list" a procurar por indice. complexidade constante em array
//usar index em vez de t e valuation
  type Stream = List[(Double,Valuation)]
  type ArrayStream = Array[(Double,Valuation)] //performance
  type StlExp = Stl

  def streamToArray(stream: Stream): Array[(Double, Valuation)] =
    val array = stream.toArray
    array.zipWithIndex.foreach { case (value, index) =>
      println(s"array[$index] = $value")
    }
    array

/* 
    1º Encontrar (t) atual (primeiro instant do intervalo) !!
    2º Verificar se psi acontece na valoração atual (se sim devolve true)
    3º Se psi nao acontece, verificar se phi acontece na valoração atual (se nao devolve falso)
    4º Encontrar próximo t2 (se nao existir devolve falso) !! 
    5º verificar se phi until psi acontece em t2 !! ajustar intervalo 
    pode nao haver instante seguinte no caso de fim da stream ou intervalo
    funções auxiliares (encontrar instante atual, ...)
    ver qual a valoração (valuation) no instante "t"
    ver qual o próximo instante depois de "t*/



  /*
//Option Double, adicionar intervalo como argumento, devolver valoração aqui
  def nextTimeAfter(stream: Stream, t: Double): Double = {
    val nextTime = stream.collectFirst { case (time, _) if time > t => time }.getOrElse(Double.PositiveInfinity)
    println(s"nextTime = $nextTime")
    if (nextTime == Double.PositiveInfinity) -1 else nextTime
  }*/

/*
//pode haver 2 pontos com o mesmo t, queremos o ultimo, ou percorrer todas as valorações para o mesmo t
  def findvalAtTime(stream: Stream, t: Double): Valuation = {
    stream.find(_._1 >= t) match
      case Some((time, valuation)) => valuation
      case None => Map.empty
  }


  def findnextval(stream: Stream, t: Double, upbound: Double, valt: Valuation): Option[(Double, Valuation)] = {
    stream.find(x => x._1 > t && x._1 <= upbound)
  }*/

  
/*
  private def checkUntil(stream: Stream, valuation: Valuation, phi: Stl, psi: Stl, inter: Inter, t: Double): Boolean = {
    // Compute absolute bounds on first call that stay fixed for all recursive calls
    val absoluteLowerBound = t + inter.t1
    val absoluteUpperBound = t + inter.t2.getOrElse(Double.PositiveInfinity)
    checkUntilHelper(stream, valuation, phi, psi, inter, t, absoluteLowerBound, absoluteUpperBound)
  }

  private def checkUntilHelper(stream: Stream, valuation: Valuation, phi: Stl, psi: Stl, inter: Inter, t: Double, absoluteLowerBound: Double, absoluteUpperBound: Double): Boolean = {
    // Use fixed absolute bounds from initial call - don't recompute them
    val a = absoluteLowerBound
    val b = absoluteUpperBound
    val valt = findvalAtTime(stream, t)
    println(s"=== checkUntil: t=$t, interval=[${inter.t1}, ${inter.t2}] -> absolute=[$a, $b], currentValt=$valt")
    println(s"    phi=$phi")
    println(s"    psi=$psi")
    
    if (valt.isEmpty || t > b) {
      println(s"    -> EARLY EXIT: valt.isEmpty=${valt.isEmpty}, t > b=${t > b}")
      return false
    }
    
    val nexttime = findnextval(stream, t, b, valt).map(_._1).getOrElse(Double.PositiveInfinity)
    val nextvalt = findnextval(stream, t, b, valt).map(_._2).getOrElse(Map.empty)
    println(s"    nexttime=$nexttime")
    
    if (t < a) {
      println(s"    -> t < a, skipping to nexttime")
      val newValt = findvalAtTime(stream, nexttime)
      checkUntilHelper(stream, newValt, phi, psi, inter, nexttime, absoluteLowerBound, absoluteUpperBound)
    }
    else {
      println(s"    -> checking at t=$t")
      val psiResult = apply(stream, valt, psi, t)
      println(s"       psi result: $psiResult")
      if (psiResult) {
        println(s"    -> psi is TRUE, returning true")
        true
      }
      else {
        val phiResult = apply(stream, valt, phi, t)
        println(s"       phi result: $phiResult")
        if (!phiResult) {
          println(s"    -> phi is FALSE, returning false")
          false
        }
        else {
          if (nexttime == Double.PositiveInfinity) {
            println(s"    -> no more times, returning false")
            false
          }
          else {
            println(s"    -> phi is TRUE, continuing to nexttime=$nexttime")
            val newValt = findvalAtTime(stream, nexttime)
            checkUntilHelper(stream, newValt, phi, psi, inter, nexttime, absoluteLowerBound, absoluteUpperBound)
          }
        }
      } 
    }
  }*/

  

  def findvalAtTime(stream: ArrayStream, t: Double): Valuation = {
    val index = stream.indexWhere(_._1 >= t)
    if (index >= 0) stream(index)._2 else Map.empty
  }

  def findnextval(stream: ArrayStream, t: Double, upbound: Double, valt: Valuation): Option[(Double, Valuation)] = {
    val startIndex = stream.indexWhere(_._1 > t)
    if (startIndex < 0) None
    else {
      val nextIndex = (startIndex until stream.length).find(index => stream(index)._1 <= upbound)
      nextIndex.map(stream(_)) //index => stream(index). returns Some((time, valuation)) or None
    }
  }


  private def checkUntil(stream: ArrayStream, startIndex: Int, phi: Stl, psi: Stl, inter: Inter, t: Double): Boolean = {
    val absoluteLowerBound = t + inter.t1
    val absoluteUpperBound = t + inter.t2.getOrElse(Double.PositiveInfinity)
    val index = if (startIndex == 0) stream.indexWhere(_._1 >= t) else startIndex
    if (index < 0 || index >= stream.length) false
    else checkUntilHelper(stream, index, phi, psi, absoluteLowerBound, absoluteUpperBound)
  }

  private def checkUntilHelper(stream: ArrayStream, index: Int, phi: Stl, psi: Stl, absoluteLowerBound: Double, absoluteUpperBound: Double): Boolean = {
    if (index >= stream.length) false
    else {
      val (t, valuation) = stream(index)

      if (t > absoluteUpperBound) false
      else if (t < absoluteLowerBound) {
        checkUntilHelper(stream, index + 1, phi, psi, absoluteLowerBound, absoluteUpperBound)
      }
      else if (applyAtIndex(stream, index, psi, t, valuation)) true
      else if (!applyAtIndex(stream, index, phi, t, valuation)) false
      else checkUntilHelper(stream, index + 1, phi, psi, absoluteLowerBound, absoluteUpperBound)
    }
  }

  private def applyAtIndex(stream: ArrayStream, index: Int, stlexp: StlExp, t: Double, valuation: Valuation): Boolean = {
    //println(s"analisando $stlexp")
    stlexp match
      case Stl.Con(e) =>
        Eval(e)(using valuation, new Random())
      case Stl.Not(s1) => !applyAtIndex(stream, index, s1, t, valuation)
      case Stl.And(s1, s2) => applyAtIndex(stream, index, s1, t, valuation) && applyAtIndex(stream, index, s2, t, valuation)
      case Stl.Or(s1, s2) => applyAtIndex(stream, index, s1, t, valuation) || applyAtIndex(stream, index, s2, t, valuation)
      case Stl.Next(s1) =>
        if index + 1 >= stream.length then false
        else {
          val (nextTime, nextValuation) = stream(index + 1)
          applyAtIndex(stream, index + 1, s1, nextTime, nextValuation)
        }
      case Stl.U(s1, s2, inter) => checkUntil(stream, index, s1, s2, inter, t)
  }

  def apply(stream: ArrayStream, valuation: Valuation, stlexp: StlExp, t: Double): Boolean = {
    val index = stream.indexWhere(_._1 >= t)
    if (index < 0) false
    else applyAtIndex(stream, index, stlexp, t, valuation)
  }





/*



def findvalAtTime(stream: ArrayStream, t: Double): Valuation = {
    val index = stream.indexWhere(_._1 >= t)
    if (index >= 0) stream(index)._2 else Map.empty
  }

  def findnextval(stream: ArrayStream, t: Double, upbound: Double, valt: Valuation): Option[(Double, Valuation)] = {
    val startIndex = stream.indexWhere(_._1 > t)
    if (startIndex < 0) None
    else {
      val nextIndex = (startIndex until stream.length).find(index => stream(index)._1 <= upbound)
      nextIndex.map(stream(_)) //index => stream(index). returns Some((time, valuation)) or None
    }
  }

  //usar index em vez de t e valuation

  private def checkUntil(stream: ArrayStream, valuation: Valuation, phi: Stl, psi: Stl, inter: Inter, t: Double): Boolean = {
    // Compute absolute bounds on first call that stay fixed for all recursive calls
    val absoluteLowerBound = t + inter.t1
    val absoluteUpperBound = t + inter.t2.getOrElse(Double.PositiveInfinity)
    checkUntilHelper(stream, valuation, phi, psi, inter, t, absoluteLowerBound, absoluteUpperBound)
  }

  private def checkUntilHelper(stream: ArrayStream, valuation: Valuation, phi: Stl, psi: Stl, inter: Inter, t: Double, absoluteLowerBound: Double, absoluteUpperBound: Double): Boolean = {
    // Use fixed absolute bounds from initial call
    val a = absoluteLowerBound
    val b = absoluteUpperBound
    val valt = findvalAtTime(stream, t)
    //println(s"=== checkUntil: t=$t, interval=[${inter.t1}, ${inter.t2}] -> absolute=[$a, $b], currentValt=$valt")
    //println(s"    phi=$phi")
    //println(s"    psi=$psi")

    if (valt.isEmpty || t > b) return false

    val nexttime = findnextval(stream, t, b, valt).map(_._1).getOrElse(Double.PositiveInfinity)
    //println(s"    nexttime=$nexttime")

    //skip to next time if t<a, because we are not in the interval yet
    if (t < a) {
      val newValt = findvalAtTime(stream, nexttime)
      checkUntilHelper(stream, newValt, phi, psi, inter, nexttime, absoluteLowerBound, absoluteUpperBound)
    }
    //if psi true return true, if phi false return false, if end of stream return false,else continue to next time
    else {
      val psiResult = apply(stream, valt, psi, t)
      if (psiResult) true
      
      else {
        val phiResult = apply(stream, valt, phi, t)
        if (!phiResult) false
        
        else if (nexttime == Double.PositiveInfinity) false

        else {
          val newValt = findvalAtTime(stream, nexttime)
          checkUntilHelper(stream, newValt, phi, psi, inter, nexttime, absoluteLowerBound, absoluteUpperBound)
        }
      }
    }
  }

  def apply(stream: ArrayStream, valuation: Valuation, stlexp: StlExp, t: Double): Boolean = {
    //println(s"analisando $stlexp")
    stlexp match
      case Stl.Con(e) =>
        if (stream.isEmpty) false
        else Eval(e)(using valuation, new Random())
      case Stl.Not(s1) => !apply(stream, valuation, s1, t)
      case Stl.And(s1, s2) => apply(stream, valuation, s1, t) && apply(stream, valuation, s2, t)
      case Stl.Or(s1, s2) => apply(stream, valuation, s1, t) || apply(stream, valuation, s2, t)
      case Stl.Next(s1) => if stream.isEmpty then false else apply(stream.drop(1), valuation, s1, t)
      case Stl.U(s1, s2, inter) => checkUntil(stream, valuation, s1, s2, inter, t)
  }





*/










/*
  def apply(stream: Stream, valuation: Valuation, stlexp: StlExp, t: Double): Boolean = { 
    println(s"analisando $stlexp")
    stlexp match 
      case Stl.Con(e) => 
        if (stream.isEmpty) false
        else Eval(e)(using valuation, new Random())
      case Stl.Not(s1) => !apply(stream, valuation, s1, t)
      case Stl.And(s1, s2) => apply(stream, valuation, s1, t) && apply(stream, valuation, s2, t)
      case Stl.Or(s1, s2) => apply(stream, valuation, s1, t) || apply(stream, valuation, s2, t)
      case Stl.Next(s1) => if stream.isEmpty then false else apply(stream.tail, valuation, s1, t)
      case Stl.U(s1, s2, inter) => checkUntil(stream, valuation, s1, s2, inter, t)
  }
*/
  

 /*def apply(stream: Stream, stlexp: StlExp, t: Double): Boolean = { 
    println(s"analisando $stlexp")
    stlexp match 
      case Stl.Con(e) => 
        if (stream.isEmpty) false
        else Eval(e)(using stream.head._2, new Random())
      case Stl.Not(s1) => !apply(stream, s1, t)
      case Stl.And(s1, s2) => apply(stream, s1, t) && apply(stream, s2, t)
      case Stl.Or(s1, s2) => apply(stream, s1, t) || apply(stream, s2, t)
      case Stl.Next(s1) => if stream.isEmpty then false else apply(stream.tail, s1, t)
      case Stl.U(s1, s2, inter) => checkUntil(stream, s1, s2, inter, t)
  }*/


  /*
  private def checkUntil(stream: Stream, valuation: Valuation, phi: Stl, psi: Stl, inter: Inter, t: Double, bound: Double, firstcheck: Double): Boolean = {
    val a = inter.t1
    val b = inter.t2.getOrElse(Double.PositiveInfinity)
    if (firstcheck == 0) { 
      bound =+ b
      firstcheck = 1
      }
    val valt = findvalAtTime(stream, t)
    println(s"a = $a e b = $b")
    println(s"stream $stream")
    println(s"bound = $bound")
    if (stream.isEmpty || t > bound) return false
    if (a > 0) {
      inter.t1 = a-1
      inter.t2 = b-1
      checkUntil(stream, valt, phi, psi, inter, nextTimeAfter(stream, t), bound)
    }
    else {
      if (apply(stream, valt, psi, t, bound)) == true then true
      else if (apply(stream, valt, phi, t, bound)) == false then false
      else {
        if (nextTimeAfter(stream, t) == -1) return false
        else checkUntil(stream, valt, phi, psi, inter, nextTimeAfter(stream, t), bound)
      } 
    }
  }



  private def checkUntil(stream: Stream, valuation: Valuation, phi: Stl, psi: Stl, inter: Inter, t: Double): Boolean = {
    val a = inter.t1
    val b = inter.t2.getOrElse(Double.PositiveInfinity)
    val valt = findvalAtTime(stream, t)
    val array = stream.toArray
    println(s"a = $a e b = $b")
    println(s"stream $stream")
    //println(s"Stream as array: ${array.mkString("Array(", ", ", ")")}")
    //array.zipWithIndex.foreach { case (value, index) =>
    //  println(s"array[$index] = $value")
    //}
    if (valt.isEmpty || t > b) return false
    val nexttime = findnextval(stream, t, b, valt).map(_._1).getOrElse(Double.PositiveInfinity)
    val nextvalt = findnextval(stream, t, b, valt).map(_._2).getOrElse(Map.empty)
    if (t < a) {
      val newValt = findvalAtTime(stream, nexttime)
      checkUntil(stream, newValt, phi, psi, inter, nexttime)
    }
    else {
      if (apply(stream, valt, psi, t)) == true then true
      else if (apply(stream, valt, phi, t)) == false then false
      else {
        if (nexttime == Double.PositiveInfinity) return false
        else {
          val newValt = findvalAtTime(stream, nexttime)
          checkUntil(stream, newValt, phi, psi, inter, nexttime)
        }
      } 
    }
  }

*/
 /* 
def apply(stream: Stream, valuation: Valuation, stlexp: StlExp, t: Double, bound: Double, firstcheck: Double): Boolean = { 
    println(s"analisando $stlexp")
    stlexp match 
      case Stl.Con(e) => 
        if (stream.isEmpty) false
        else Eval(e)(using valuation, new Random())
      case Stl.Not(s1) => !apply(stream, valuation, s1, t, bound, firstcheck)
      case Stl.And(s1, s2) => apply(stream, valuation, s1, t, bound, firstcheck) && apply(stream, valuation, s2, t, bound, firstcheck)
      case Stl.Or(s1, s2) => apply(stream, valuation, s1, t, bound, firstcheck) || apply(stream, valuation, s2, t, bound, firstcheck)
      case Stl.Next(s1) => if stream.isEmpty then false else apply(stream.tail, valuation, s1, t, bound, firstcheck)
      case Stl.U(s1, s2, inter) => checkUntil(stream, valuation, s1, s2, inter, t, bound, firstcheck)
                                   //bound = bound + inter.t2
  }*/

 /*
  private def checkUntil(stream: Stream, valuation: Valuation, phi: Stl, psi: Stl, inter: Inter, t: Double): Boolean = {
    val a = inter.t1
    val b = inter.t2.getOrElse(Double.PositiveInfinity)
    val valt = findvalAtTime(stream, t)
    println(s"a = $a e b = $b")
    println(s"stream $stream")
    if (valt.isEmpty || t > b) return false
    if (t < a) checkUntil(stream, valt, phi, psi, inter, nextTimeAfter(stream, t))
    else {
      if (apply(stream, valt, psi, t)) == true then true
      else if (apply(stream, valt, phi, t)) == false then false
      else {
        if (nextTimeAfter(stream, t) == -1) return false
        else checkUntil(stream, valt, phi, psi, inter, nextTimeAfter(stream, t))
      } 
    }
  }*/


/*
  private def checkUntil(stream: Stream, phi: Stl, psi: Stl, inter: Inter, t: Double): Boolean = {
    if (stream.isEmpty) return false
    //val startTime = stream.head._1
    val a = t + inter.t1
    val b = inter.t2.map(t + _).getOrElse(Double.PositiveInfinity)
    println(s"a = $a e b = $b")
    val candidates = stream.filter(_._1 >= a).takeWhile(_._1 <= b)
    println(s"candidatos $candidates")
    val psiPoints = candidates.filter(p => apply(List(p), psi, a))
    if (psiPoints.isEmpty) false
    else {
      val firstPsiTime = psiPoints.head._1
      //val before = stream.takeWhile(_._1 < firstPsiTime)
      val before = candidates.takeWhile(_._1 < firstPsiTime)
      before.forall(p => apply(List(p), phi, a))
    }*/

    /*
  private def checkUntil(stream: Stream, phi: Stl, psi: Stl, inter: Inter, t: Double): Boolean = {
    if (stream.isEmpty) return false

    val lowerBound = t + inter.t1
    val upperBound = inter.t2.map(t + _).getOrElse(Double.PositiveInfinity)
    val candidates = stream.filter { case (time, _) => time >= lowerBound && time <= upperBound }
    println(s"candidatos $candidates")

    candidates.exists { case (candidateTime, candidateValuation) =>
      val previousPoints = stream.takeWhile { case (time, _) => time < candidateTime }
      val psiHolds = apply(List((candidateTime, candidateValuation)), psi, candidateTime)
      val phiHoldsBefore = previousPoints.forall { case (pointTime, pointValuation) =>
        apply(List((pointTime, pointValuation)), phi, pointTime)
      }

      psiHolds && phiHoldsBefore
    }
  }*/
    

 /* private def checkUntil(stream: Stream, phi: Stl, psi: Stl, inter: Inter, t: Double): Boolean = {
    if (stream.isEmpty) return false
    //val startTime = stream.head._1
    //val first = stream.find(_._1 >= a)
    val a = t + inter.t1
    val b = inter.t2.getOrElse(Double.PositiveInfinity)
    //val b = inter.t2.map(t + _).getOrElse(Double.PositiveInfinity)
    println(s"a = $a e b = $b")
    println(s"stream $stream")
    if (apply(stream.dropWhile(_._1 < a), psi, a)) == true then true
    else if (apply(stream.dropWhile(_._1 < a), phi, a)) == false then false
    else {
      val nextPoint = stream.find(_._1 > a)
      nextPoint match {
        case Some((nextTime, _)) if nextTime <= b =>
          println(s"nextTime = $nextTime")
          checkUntil(stream.dropWhile(_._1 <= a), phi, psi, inter, nextTime-inter.t1)
        case _ => false
      }
    } 
  }*/

 /*
private def checkUntil(stream: Stream, phi: Stl, psi: Stl, inter: Inter, t: Double): Boolean = {
    if (stream.isEmpty) return false

    val start = t + inter.t1
    val end = inter.t2.map(t + _).getOrElse(Double.PositiveInfinity)

    stream
      .dropWhile(_._1 < start)
      .takeWhile(_._1 <= end)
      .exists { case (time, _) =>
        val suffix = stream.dropWhile(_._1 < time)
        if apply(suffix, psi, time) then
          stream
            .takeWhile { case (tt, _) => tt < time && tt >= start }
            .forall { case (tt, _) => apply(stream.dropWhile(_._1 < tt), phi, tt) }
        else false
      }
  }*/

/*
private def evaluateAtIndex(
    stream: Stream,
    stlexp: StlExp,
    index: Int,
    memo: scala.collection.mutable.Map[(Int, StlExp), Boolean]
  ): Boolean = {
    if (index < 0 || index >= stream.length) false
    else memo.getOrElseUpdate((index, stlexp), {
      stlexp match
        case Stl.Con(e) =>
          val (_, valuation) = stream(index)
          Eval(e)(using valuation, new Random())
        case Stl.Not(s1) => !evaluateAtIndex(stream, s1, index, memo)
        case Stl.And(s1, s2) => evaluateAtIndex(stream, s1, index, memo) && evaluateAtIndex(stream, s2, index, memo)
        case Stl.Or(s1, s2) => evaluateAtIndex(stream, s1, index, memo) || evaluateAtIndex(stream, s2, index, memo)
        case Stl.Next(s1) =>
          if (index + 1 < stream.length) evaluateAtIndex(stream, s1, index + 1, memo) else false
        case Stl.U(s1, s2, inter) =>
          val (currentTime, _) = stream(index)
          val lowerBound = currentTime + inter.t1
          val upperBound = inter.t2.map(currentTime + _).getOrElse(Double.PositiveInfinity)

          stream.drop(index).zipWithIndex.exists { case ((time, _), offset) =>
            val candidateIndex = index + offset
            time >= lowerBound && time <= upperBound &&
              evaluateAtIndex(stream, s2, candidateIndex, memo) &&
              (0 until offset).forall(nextOffset => evaluateAtIndex(stream, s1, index + nextOffset, memo))
          }
    })
  }

  def evaluateBottomUp(stream: Stream, stlexp: StlExp): List[(Double, Boolean)] = {
    val memo = scala.collection.mutable.Map.empty[(Int, StlExp), Boolean]
    stream.zipWithIndex.map { case ((time, _), index) =>
      (time, evaluateAtIndex(stream, stlexp, index, memo))
    }
  }

  def apply(stream: Stream, stlexp: StlExp, t: Double): Boolean = {
    val index = stream.indexWhere(_._1 >= t)
    if (index < 0) false
    else {
      val memo = scala.collection.mutable.Map.empty[(Int, StlExp), Boolean]
      evaluateAtIndex(stream, stlexp, index, memo)
    }
  }
*/

/* import java.util.ArrayDeque

type Valuation = Map[String, Double]
type Stream = List[(Double, Valuation)]

sealed trait StlExp
case class Predicate(varName: String, threshold: Double) extends StlExp
case class And(left: StlExp, right: StlExp) extends StlExp
case class Or(left: StlExp, right: StlExp) extends StlExp
case class Eventually(interval: (Double, Double), stlexp: StlExp) extends StlExp
case class Globally(interval: (Double, Double), stlexp: StlExp) extends StlExp

object OptimizedSTLEvaluator {

  def evaluate(stlexp: StlExp, stream: Stream): List[(Double, Double)] = {
    // Convert to Vector for O(1) index access
    val streamVec = stream.toVector
    evaluateToVector(stlexp, streamVec).toList
  }

  private def evaluateToVector(stlexp: StlExp, stream: Vector[(Double, Valuation)]): Vector[(Double, Double)] = stlexp match {
    
    case Predicate(varName, threshold) =>
      stream.map { case (t, valuation) => 
        t -> (valuation.getOrElse(varName, 0.0) - threshold) 
      }

    case And(left, right) =>
      val robLeft = evaluateToVector(left, stream)
      val robRight = evaluateToVector(right, stream)
      robLeft.zip(robRight).map { case ((t, rL), (_, rR)) => t -> math.min(rL, rR) }

    case Or(left, right) =>
      val robLeft = evaluateToVector(left, stream)
      val robRight = evaluateToVector(right, stream)
      robLeft.zip(robRight).map { case ((t, rL), (_, rR)) => t -> math.max(rL, rR) }

    case Eventually((a, b), innerstlexp) =>
      val robInner = evaluateToVector(innerstlexp, stream)
      computeTemporalWindow(robInner, a, b, isMax = true)

    case Globally((a, b), innerstlexp) =>
      val robInner = evaluateToVector(innerstlexp, stream)
      computeTemporalWindow(robInner, a, b, isMax = false)
  }

  // O(N) Monotonic Queue implementation for sliding windows over irregular time intervals
  private def computeTemporalWindow(
    robInner: Vector[(Double, Double)], 
    a: Double, 
    b: Double, 
    isMax: Boolean
  ): Vector[(Double, Double)] = {
    
    val n = robInner.length
    val result = new Array[(Double, Double)](n)
    
    // Deque stores INDICES of the robInner vector
    val deque = new ArrayDeque[Int]()
    
    // Two pointers tracking the valid future window boundaries [t + a, t + b]
    var winStart = n
    var winEnd = n

    // Loop backwards to compute future dependencies seamlessly
    for (i <- (n - 1) to 0By -1) {
      val (t, _) = robInner(i)
      val tStart = t + a
      val tEnd = t + b

      // 1. Advance winEnd pointer to include all points up to (t + b)
      while (winEnd > 0 && robInner(winEnd - 1)._1 >= tStart) {
        winEnd -= 1
        val idxToAdd = winEnd
        
        // Only consider elements strictly within the upper bound tEnd
        if (robInner(idxToAdd)._1 <= tEnd) {
          // Maintain monotonicity
          while (!deque.isEmpty && shouldEvict(robInner(deque.peekLast())._2, robInner(idxToAdd)._2, isMax)) {
            deque.pollLast()
          }
          deque.addLast(idxToAdd)
        }
      }

      // 2. Retract winStart pointer to track elements that dropped below the lower bound tStart
      while (winStart > 0 && robInner(winStart - 1)._1 > tEnd) {
        winStart -= 1
      }

      // 3. Evict elements from the front of the deque that outlived the upper time bound (tEnd)
      while (!deque.isEmpty && robInner(deque.peekFirst())._1 > tEnd) {
        deque.pollFirst()
      }
      
      // 4. Evict elements from the front of the deque that are before the lower time bound (tStart)
      while (!deque.isEmpty && robInner(deque.peekFirst())._1 < tStart) {
        deque.pollFirst()
      }

      // 5. Extract extreme value
      val robValue = if (deque.isEmpty) {
        if (isMax) Double.NegativeInfinity else Double.PositiveInfinity
      } else {
        robInner(deque.peekFirst())._2
      }

      result(i) = (t, robValue)
    }

    result.toVector
  }

  @inline
  private def shouldEvict(existing: Double, incoming: Double, isMax: Boolean): Boolean = {
    if (isMax) existing <= incoming else existing >= incoming
  }
} */





/*  // Computes robustness at every time step bottom-up
  def evaluate(stlexp: StlExp, stream: stream): List[(Double, Double)] = stlexp match {
    
    // Base Case: Extract value from Valuation and subtract threshold
    case Predicate(varName, threshold) =>
      stream.map { case (t, valuation) => 
        val value = valuation.getOrElse(varName, 0.0)
        t -> (value - threshold) 
      }

    // Pointwise Minimum
    case And(left, right) =>
      val robLeft = evaluate(left, stream)
      val robRight = evaluate(right, stream)
      robLeft.zip(robRight).map { case ((t, rL), (_, rR)) => 
        t -> math.min(rL, rR) 
      }

    // Pointwise Maximum
    case Or(left, right) =>
      val robLeft = evaluate(left, stream)
      val robRight = evaluate(right, stream)
      robLeft.zip(robRight).map { case ((t, rL), (_, rR)) => 
        t -> math.max(rL, rR) 
      }

    // F[a, b] phi: Maximum robustness in future window [t + a, t + b]
    case Eventually((a, b), innerstlexp) =>
      val robInner = evaluate(innerstlexp, stream)
      
      robInner.zipWithIndex.map { case ((t, _), idx) =>
        // Slice the future list to look inside the time window
        val windowValues = robInner.drop(idx).takeWhile { case (futureT, _) => 
          futureT <= t + b 
        }.collect { case (futureT, r) if futureT >= t + a => r }

        t -> (if (windowValues.isEmpty) Double.NegativeInfinity else windowValues.max)
      }

    // G[a, b] phi: Minimum robustness in future window [t + a, t + b]
    case Globally((a, b), innerstlexp) =>
      val robInner = evaluate(innerstlexp, stream)
      
      robInner.zipWithIndex.map { case ((t, _), idx) =>
        // Slice the future list to look inside the time window
        val windowValues = robInner.drop(idx).takeWhile { case (futureT, _) => 
          futureT <= t + b 
        }.collect { case (futureT, r) if futureT >= t + a => r }

        t -> (if (windowValues.isEmpty) Double.PositiveInfinity else windowValues.min)
      }
  }
*/


/*
 // Logical AND ( pointwise minimum )
    case And(left, right) =>
      val robLeft = evaluate(left, stream)
      val robRight = evaluate(right, stream)
      robLeft.keys.map(t => t -> math.min(robLeft(t), robRight.getOrElse(t, Double.NegativeInfinity))).toMap

    // Logical OR ( pointwise maximum )
    case Or(left, right) =>
      val robLeft = evaluate(left, stream)
      val robRight = evaluate(right, stream)
      robLeft.keys.map(t => t -> math.max(robLeft(t), robRight.getOrElse(t, Double.NegativeInfinity))).toMap

    // Eventually F[a, b] phi ( maximum in sliding future window )
    case Eventually((a, b), innerstlexp) =>
      val robInner = evaluate(innerstlexp, stream)
      robInner.keys.map { t =>
        val windowValues = (t + a to t + b).flatMap(robInner.get)
        t -> (if (windowValues.isEmpty) Double.NegativeInfinity else windowValues.max)
      }.toMap

    // Globally G[a, b] phi ( minimum in sliding future window )
    case Globally((a, b), innerstlexp) =>
      val robInner = evaluate(innerstlexp, stream)
      robInner.keys.map { t =>
        val windowValues = (t + a to t + b).flatMap(robInner.get)
        t -> (if (windowValues.isEmpty) Double.PositiveInfinity else windowValues.min)
      }.toMap
      */


  
  


/*package stl

import scala.math.{max, min}

// 1. Define the AST (Abstract Syntax Tree) using Scala 3 Enums
enum StlExp:
  case Predicate(eval: Double => Double)
  case Eventually(inner: StlExp, a: Double, b: Double)
  case Always(inner: StlExp, a: Double, b: Double)

object STLEvaluator {

  /**
   * Evaluates any nested STL stlexp over a discrete time stream.
   * Returns a Vector of robustness values corresponding to each time step.
   */
  def evaluate(stlexp: StlExp, stream: Vector[Double], dt: Double): Vector[Double] = {
    val N = stream.length
    
    stlexp match {
      // Base Case: Map the continuous predicate function over the raw stream
      case StlExp.Predicate(evalFunc) =>
        stream.map(evalFunc)

      // Nested Case: Recursively solve inner bounds first, then evaluate Eventually
      case StlExp.Eventually(inner, a, b) =>
        val innerRobustness = evaluate(inner, stream, dt)
        val Na = (a / dt).floor.toInt
        val Nb = (b / dt).floor.toInt
        
        Vector.tabulate(N) { t =>
          val start = t + Na
          val end = min(t + Nb + 1, N)
          if (start < N && start < end) then
            // Slice the inner robustness array and find the maximum scalar
            innerRobustness.slice(start, end).max
          else 
            Double.NegativeInfinity // Edge protection
        }

      // Nested Case: Recursively solve inner bounds first, then evaluate Always
      case StlExp.Always(inner, a, b) =>
        val innerRobustness = evaluate(inner, stream, dt)
        val Na = (a / dt).floor.toInt
        val Nb = (b / dt).floor.toInt
        
        Vector.tabulate(N) { t =>
          val start = t + Na
          val end = min(t + Nb + 1, N)
          if (start < N && start < end) then
            // Slice the inner robustness array and find the minimum scalar
            innerRobustness.slice(start, end).min
          else 
            Double.PositiveInfinity // Edge protection
        }
    }
  }
}

@main def runSTL(): Unit = {
  val dt = 0.1
  // Generate a mock stream (simulating a system fluctuating above and below 5)
  val timeSteps = Vector.tabulate(50)(i => i * dt)
  val xstream = timeSteps.map(t => 4.0 + 2.0 * math.sin(2 * math.Pi * 0.5 * t))

  // Define the nested stlexp using Scala AST
  // Step 1: Predicate (x - 5.0)
  val myPredicate = StlExp.Predicate(x => x - 5.0)
  // Step 2: Inner operator F_[0, 1.0] (x > 5)
  val innerstlexp = StlExp.Eventually(myPredicate, a = 0.0, b = 1.0)
  // Step 3: Outer operator G_[0, 2.0] F_[0, 1.0] (x > 5)
  val nestedstlexp = StlExp.Always(innerstlexp, a = 0.0, b = 2.0)

  // Compute robustness arrays from the inside out
  val robustnessArray = STLEvaluator.evaluate(nestedstlexp, xstream, dt)

  // Robustness at t=0 determines overall satisfaction
  val initialRobustness = robustnessArray.head
  val isSatisfied = initialRobustness > 0

  println(s"Is nested stlexp satisfied at t=0? $isSatisfied (Robustness: $initialRobustness)")
}


*/

