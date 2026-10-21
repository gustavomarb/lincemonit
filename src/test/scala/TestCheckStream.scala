import lince.backend.CheckStream
import lince.syntax.Lince
import lince.syntax.Lince.{Cond, Expr, Inter, Stl}

class TestCheckStream extends munit.FunSuite {

  test("until is true when psi becomes true within the interval and phi holds before it") {
    val stream = List(
      (0.0, Map("x" -> 0.0)),
      (0.5, Map("x" -> 0.0)),
      (1.0, Map("x" -> 1.0))
    )
    val phi = Stl.Con(Cond.Comp(">=", Expr.Var("x"), Expr.Num(0)))
    val psi = Stl.Con(Cond.Comp(">", Expr.Var("x"), Expr.Num(0)))

    val obtained = CheckStream(stream, Stl.U(phi, psi, Inter(0.0, Some(1.0))), 0.0)

    assert(obtained)
  }

  test("until is false when psi never occurs in the requested interval") {
    val stream = List(
      (0.0, Map("x" -> 0.0)),
      (0.5, Map("x" -> 0.0)),
      (1.0, Map("x" -> 0.0))
    )
    val phi = Stl.Con(Cond.Comp(">=", Expr.Var("x"), Expr.Num(0)))
    val psi = Stl.Con(Cond.Comp(">", Expr.Var("x"), Expr.Num(0)))

    val obtained = CheckStream(stream, Stl.U(phi, psi, Inter(0.0, Some(1.0))), 0.0)

    assert(!obtained)
  }

  test("until evaluates valuations with the same timestamp separately") {
    val stream = List(
      (0.0, Map("x" -> 0.0)),
      (0.5, Map("x" -> 0.0)),
      (0.5, Map("x" -> 1.0))
    )
    val phi = Stl.Con(Cond.Comp(">=", Expr.Var("x"), Expr.Num(0)))
    val psi = Stl.Con(Cond.Comp(">", Expr.Var("x"), Expr.Num(0)))

    val obtained = CheckStream(stream, Stl.U(phi, psi, Inter(0.0, Some(0.5))), 0.0)

    assert(obtained)
  }

  test("globally eventually uses nested time bounds correctly") {
    val stream = List(
      (0.0, Map("x" -> 0.0)),
      (0.3, Map("x" -> 1.0)),
      (0.8, Map("x" -> 0.0))
    )
    val psi = Stl.Con(Cond.Comp(">", Expr.Var("x"), Expr.Num(0)))
    val formula = Lince.globally(Lince.eventually(psi, Inter(0.0, Some(0.5))), Inter(0.0, Some(0.8)))

    assert(CheckStream(stream, formula, 0.0))
  }

  test("nextTimeAfter returns the next timestamp without modifying the stream") {
    val stream = List(
      (0.0, Map("x" -> 0.0)),
      (0.5, Map("x" -> 0.0)),
      (1.0, Map("x" -> 1.0))
    )

    val obtained = CheckStream.nextTimeAfter(stream, 0.5)

    assertEquals(obtained, 1.0)
  }

  test("evaluateBottomUp computes a formula pointwise from the stream") {
    val stream = List(
      (0.0, Map("x" -> 0.0)),
      (1.0, Map("x" -> 1.0)),
      (2.0, Map("x" -> 2.0))
    )
    val formula = Stl.And(
      Stl.Con(Cond.Comp(">=", Expr.Var("x"), Expr.Num(0))),
      Stl.Next(Stl.Con(Cond.Comp(">", Expr.Var("x"), Expr.Num(0))))
    )

    val obtained = CheckStream.evaluateBottomUp(stream, formula)

    assertEquals(obtained, List((0.0, true), (1.0, true), (2.0, false)))
  }
}
