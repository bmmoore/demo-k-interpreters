trait K

import collection._

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

case class KCell(var ks: Seq[K]) extends K
case class StateCell(var m: Map[Id, Int]) extends K
case class TopCell(kcell: KCell, state: StateCell) extends K

object ImpInScala extends App {

  def step(t: TopCell) = {
    val kcell = t.kcell
    val head: K = kcell.ks.head

    if (head.isInstanceOf[Id] && t.state.m.contains(head.asInstanceOf[Id])) {
      kcell.ks = t.state.m(head.asInstanceOf[Id]) +: kcell.ks.tail;
      t
    } else if (head.isInstanceOf[Assign] && head.asInstanceOf[Assign].value.isInstanceOf[Int]) {
      kcell.ks = kcell.ks.tail;
      t.state.m = t.state.m.updated(head.asInstanceOf[Assign].id, head.asInstanceOf[Assign].value.asInstanceOf[Int]);
      t
    } else {
      kcell.ks =
        kcell.ks.head match {
          // eval plus
          case Plus(Int(a), Int(b)) =>
            //println("eval +")
            //      k.ks = Int(a + b) +: rest), state)
            Int(a + b) +: kcell.ks.tail

          // heat plus
          case Plus(a, b) if !a.isInstanceOf[KResult] =>
            //println("heat + 1")
            //      a +: Plus(null, b) +: rest), state)
            a +: Plus(null, b) +: kcell.ks.tail

          case Plus(a, b) if !b.isInstanceOf[KResult] =>
            //println("heat + 2 ")
            b +: Plus(a, null) +: kcell.ks.tail

          // eval <=
          case <=(Int(a), Int(b)) =>
            Bool(a <= b) +: kcell.ks.tail

          // heat <=
          case <=(a, b) if !a.isInstanceOf[KResult] =>
            //println("heat <= 1")
            a +: <=(null, b) +: kcell.ks.tail
          case <=(a, b) if !b.isInstanceOf[KResult] =>
            //println("heat <= 2 ")
            b +: <=(a, null) +: kcell.ks.tail

          // while
          case While(c, body) =>
            IfThenElse(c, Block(body, While(c, body)), Block()) +: kcell.ks.tail

          //  heat ifthenelse
          case IfThenElse(c, th, e) if !c.isInstanceOf[KResult] =>
            c +: IfThenElse(null, th, e) +: kcell.ks.tail

          // eval ifthenelse
          case IfThenElse(Bool(true), th, e) =>
            th +: kcell.ks.tail
          case IfThenElse(Bool(false), th, e) =>
            e +: kcell.ks.tail

          // kseq
          case Block(s1: K, ss@_*) =>
            //println("block to seq")
            s1 +: Block(ss: _*) +: kcell.ks.tail

          // remove empty block
          case Block() =>
            kcell.ks.tail

          case Assign(id: Id, v) if !v.isInstanceOf[KResult] =>
            v +: Assign(id: Id, null) +: kcell.ks.tail

          case _ => kcell.ks match {
            // cool plus
            case (a: Int) +: Plus(null, b) +: rest =>
              //println("cool + 1")
              Plus(a, b) +: rest
            case (b: Int) +: Plus(a, null) +: rest =>
              //println("cool + 2")
              Plus(a, b) +: rest

            // cool <=
            case (a: Int) +: <=(null, b) +: rest =>
              //println("cool <= 1")
              <=(a, b) +: rest
            case (b: Int) +: <=(a, null) +: rest =>
              //println("cool <= 2")
              <=(a, b) +: rest

            // cool ifthenelse
            case (c: Bool) +: IfThenElse(null, th, e) +: rest =>
              IfThenElse(c, th, e) +: rest

            // heat/cool assign
            case (v: Int) +: Assign(id: Id, null) +: rest =>
              Assign(id, v) +: rest
          }
        }

      t
    }
  }


  def execute(k: TopCell): K = {
    var currentK = k
    try {
      do {
        currentK = step(currentK)
      } while (true)
    } catch {
      case e: MatchError =>
      case e: NoSuchElementException =>
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
