trait K

trait AExp extends K
trait BExp extends K
case class Plus(a: AExp, b: AExp) extends AExp
case class <=(a: AExp, b: AExp) extends BExp
case class Id(s: Symbol) extends AExp
case class Int(v: scala.Int) extends AExp with KResult
case class Bool(v: scala.Boolean) extends BExp with KResult

trait KResult

trait Stmt extends K
case class Block(s: Stmt*) extends Stmt
case class Assign(id: Id, value: AExp) extends Stmt
case class IfThenElse(cond: BExp, t: Stmt, e: Stmt) extends Stmt
case class While(cond: BExp, body: Block) extends Stmt

case class KCell(ks: Seq[K]) extends K
case class StateCell(m: Map[Id, Int]) extends K
case class TopCell(kcell: KCell, state: StateCell) extends K

object ImpInScala extends App {


  def step(k: K) = k match {
    // eval plus
    case TopCell(KCell(Plus(Int(a), Int(b)) +: rest), state) =>
      //println("eval +")
      TopCell(KCell(Int(a + b) +: rest), state)

    // heat plus
    case TopCell(KCell(Plus(a, b) +: rest), state) if !a.isInstanceOf[KResult] =>
      //println("heat + 1")
      TopCell(KCell(a +: Plus(null, b) +: rest), state)
    case TopCell(KCell(Plus(a, b) +: rest), state) if !b.isInstanceOf[KResult] =>
      //println("heat + 2 ")
      TopCell(KCell(b +: Plus(a, null) +: rest), state)

    // cool plus
    case TopCell(KCell((a: Int) +: Plus(null, b) +: rest), state) =>
      //println("cool + 1")
      TopCell(KCell(Plus(a, b) +: rest), state)
    case TopCell(KCell((b: Int) +: Plus(a, null) +: rest), state) =>
      //println("cool + 2")
      TopCell(KCell(Plus(a, b) +: rest), state)

    // eval <=
    case TopCell(KCell(<=(Int(a), Int(b)) +: rest), state) =>
      TopCell(KCell(Bool(a <= b) +: rest), state)

    // heat <=
    case TopCell(KCell(<=(a, b) +: rest), state) if !a.isInstanceOf[KResult] =>
      //println("heat <= 1")
      TopCell(KCell(a +: <=(null, b) +: rest), state)
    case TopCell(KCell(<=(a, b) +: rest), state) if !b.isInstanceOf[KResult] =>
      //println("heat <= 2 ")
      TopCell(KCell(b +: <=(a, null) +: rest), state)

    // cool <=
    case TopCell(KCell((a: Int) +: <=(null, b) +: rest), state) =>
      //println("cool <= 1")
      TopCell(KCell(<=(a, b) +: rest), state)
    case TopCell(KCell((b: Int) +: <=(a, null) +: rest), state) =>
      //println("cool <= 2")
      TopCell(KCell(<=(a, b) +: rest), state)


    // while
    case TopCell(KCell(While(c, body) +: rest), state) =>
      TopCell(KCell(IfThenElse(c, Block(body, While(c, body)), Block()) +: rest), state)

    // heat/cool ifthenelse
    case TopCell(KCell(IfThenElse(c, t, e) +: rest), state) if !c.isInstanceOf[KResult] =>
      TopCell(KCell(c +: IfThenElse(null, t, e) +: rest), state)
    case TopCell(KCell((c: Bool) +: IfThenElse(null, t, e) +: rest), state) =>
      TopCell(KCell(IfThenElse(c, t, e) +: rest), state)

    // eval ifthenelse
    case TopCell(KCell(IfThenElse(Bool(true), t, e) +: rest), state) =>
      TopCell(KCell(t +: rest), state)
    case TopCell(KCell(IfThenElse(Bool(false), t, e) +: rest), state) =>
      TopCell(KCell(e +: rest), state)

    // template
    case TopCell(KCell(Block(s1: K, ss@_*) +: rest), state) =>
      //println("block to seq")
      TopCell(KCell(s1 +: Block(ss: _*) +: rest), state)

    // remove empty block
    case TopCell(KCell(Block() +: rest), state) =>
      TopCell(KCell(rest), state)

    // lookup
    case TopCell(KCell((id: Id) +: rest), StateCell(m)) if m.contains(id) =>
      TopCell(KCell(m(id) +: rest), StateCell(m))

    // assign
    case TopCell(KCell(Assign(id: Id, v: Int) +: rest), StateCell(m)) =>
      TopCell(KCell(rest), StateCell(m.updated(id, v)))

    // heat/cool assign
    case TopCell(KCell(Assign(id: Id, v) +: rest), StateCell(m)) if !v.isInstanceOf[KResult] =>
      TopCell(KCell(v +: Assign(id: Id, null) +: rest), StateCell(m))
    case TopCell(KCell((v: Int) +: Assign(id: Id, null) +: rest), StateCell(m)) =>
      TopCell(KCell(Assign(id, v) +: rest), StateCell(m))


  }

  def execute(k: K): K = {
    var currentK = k
    try {
      do {
        currentK = step(currentK)
      } while (true)
    } catch {
      case e: MatchError =>
    }
    currentK
  }

  val count = While(<=(Int(0), Id('n)),
    Block(Block(
      Assign(Id('s), Plus(Id('s), Id('n))),
      Assign(Id('n), Plus(Id('n), Int(-1)))
    )))

  val justAPlus = Plus(Int(1), Int(2))
  val justAPlus1 = Plus(Id('n), Int(2))
  val justLess = <=(Int(1), Int(2))

  def concretize(a: K) = TopCell(
    KCell(Seq(a)),
    StateCell(Map(Id('s) -> Int(0), Id('n) -> Int(args(0).toInt))))

  val startTime = System.nanoTime()
  val res = execute(concretize(count))
  println("Execution time: " + (System.nanoTime() - startTime) / 1000000 + "ms")
  println("Result: " + res)
}
