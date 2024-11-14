package cats.effect

package std

import cats.effect.*
import cats.effect.kernel.Outcome
import cats.Eq
import cats.laws.discipline.InvariantTests
import cats.syntax.all.*
import org.scalacheck.{Arbitrary, Gen}
import org.typelevel.discipline.specs2.mutable.Discipline
import specs2.arguments.sequential

class DequeueLawsSpec extends BaseSpec with Discipline {

  sequential

  def toList[A](d: Dequeue[IO, A]) = {
    for {
      size <- d.size
      list <- d.tryTakeN(size.some)
    } yield list
    println(d)
    println(d.size)
  }

  def fromList[A: Arbitrary](as: List[A]): IO[Dequeue[IO, A]] = {
    for {
      dequeue <- Dequeue.bounded[IO, A](Int.MaxValue)
      _ <- as.traverse(a => dequeue.offer(a))
    } yield dequeue
  }

  def toListSource[A](q: QueueSource[IO, A]): IO[List[A]] =
    for {
      size <- q.size
      list <- q.tryTakeN(size.some)
    } yield list

  def genQueue[A: Arbitrary](implicit ticker: Ticker): Gen[Dequeue[IO, A]] =
    for {
      list <- Arbitrary.arbitrary[List[A]]
      dequeue = fromList(list)
      outcome = unsafeRun(dequeue) match {
        case Outcome.Succeeded(a) => a
        case _ => None
      }
    } yield outcome.get

  implicit def eqForDequeueSource[A: Eq](implicit ticker: Ticker): Eq[Dequeue[IO, A]] =
    Eq.by(toListSource)

  implicit def arbQueue[A: Arbitrary](implicit ticker: Ticker): Arbitrary[Dequeue[IO, A]] =
    Arbitrary(genQueue)

  {
    implicit val ticker = Ticker()
    checkAll("DequeInvariantLaws", InvariantTests[Dequeue[IO, *]].invariant[Int, Int, String])
  }
}
