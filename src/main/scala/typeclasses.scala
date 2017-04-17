package lambdaconf.typeclasses

import matryoshka._
import monocle._
import scalaz._

import Scalaz._

object exercise1 {
  sealed trait PathLike[A] {
    // ???
  }

  object PathLike {
    // If you don't have control over type, stick its typeclass in the companion object of that typeclass
    // This applies to the standard library or 3rd party libs
    def apply[A: PathLike]: PathLike[A] = implicitly[PathLike[A]]
  }

  trait Show[A] {
    def show(v: A): String
  }

  object Show {
    // a shothand for "summoning" matchig show
    def apply[A](implicit S: Show[A]): Show[A] = S

    implicit val showString: Show[String] = new Show[String] {
      override def show(v: String) = v
    }

    case class UserId(value: String) extends AnyVal

    implicit def showList[A](implicit S: Show[A]): Show[List[A]] = new Show[List[A]] {
      override def show(v: List[A]) = v.map(S.show(_)).mkString(",")
    }

    implicit class ShowSyntax[A: Show](value: A) {
      def show: String = Show[A].show(value)
    }
  }


  Show[List[String]].show("1" :: "2" :: Nil)

  ("1" :: "2" :: Nil).show

  // if it's my own typeclass, I'll put instances in the companion object of Box
  case class Box[A](value: A)

  object Box {
    def showBox[A](implicit S: Show[A]) = new Show[Box[A]] {
      override def show(v: Box[A]): String = "Box(" + S.show(v.value) + ")"
    }
  }



  trait Semigroup[A] {
    def append(a1: A, a2: A): A
  }
  
  object Semigroup {
    def apply[A](implicit S: Semigroup[A]): Semigroup[A] = S
    
    implicit val stringSemigroup: Semigroup[String] = new Semigroup[String] {
      override def append(a1: String, a2: String) = a1 + a2
    }

//    implicit def numericSemigroup[A: Numeric]: Semigroup[A] = new Semigroup[A] {
//      override def append(a1: A, a2: A) = implicitly[Numeric[A]].plus(a1, a2)
//    }

    implicit val numericSemigroup: Semigroup[Int] = new Semigroup[Int] {
      override def append(a1: Int, a2: Int) = a1 + a2
    }
  }

  implicit class SemigroupSyntax[A](a: A) {
    def <> (that: A)(implicit S: Semigroup[A]): A = S.append(a, that)
  }

  trait Monoid[A] extends Semigroup[A] {
    def empty: A
  }

  object Monoid {
    implicit val stringMonoid: Monoid[String] = new Monoid[String] {

      override def empty = ""

      override def append(a1: String, a2: String) = a1 + a2
    }
  }

  // The more laws are imposed on typclasses, the fewer instances will be

  "a" <> "b"
  1 <> 2
}

object exercise2 {
  import exercise2._
}

object exercise3 {
  import exercise1._
  import exercise2._

  sealed trait Node
  final case object Root extends Node
  final case class Child(parent: Node, name: String) extends Node

  implicit val MonoidNode: Monoid[Node] = new Monoid[Node] {
    def append(v1: Node, v2: Node): Node = v2 match {
      case Root => v1
      case Child(parent, name) => Child(append(v1, parent), name)
    }

    override def empty = Root
  }

  implicit val NodePathLike: PathLike[Node] = new PathLike[Node] {
    // ???
  }


  trait Partitionable[F[_]] {
    def partition[A, B, C](fa: F[A], p: A => Either[B, C]): (F[B], F[C])
  }

  object Partitionable {
    implicit val partitionableList: Partitionable[List] = new Partitionable[List] {
      override def partition[A, B, C](fa: List[A], p: (A) => Either[B, C]) = {
//        fa.foldLeft((List.empty[B], List.empty[C])) {
//          case ((leftPartition, rightPartition), element) =>
//            p(element) match {
//              case Left(b) => (b :: leftPartition, rightPartition)
//              case Right(c) => (leftPartition, c :: rightPartition)
//            }
//
//        }
        val ps = fa.map(p)
        (ps.collect { case Left(l) => l },
        ps.collect { case Right(r) => r })
      }
    }


  }

  implicit class PartitionableSyntax[F[_], A](fa: F[A]) {

    def partition2[B, C](f: A => Either[B, C])(implicit P: Partitionable[F]) = P.partition(fa, f)
  }

  List( 1 :: 2 :: 3 :: Nil).partition2(a => Right(a))
}

object exercise4 {
  import exercise1._
  import exercise2._
  import exercise3._

  implicit class PathLikeSyntax[A: PathLike](self: A) {
    // ???
  }

  trait Functor[F[_]] {
    def map[A, B](fa: F[A])(ab: A => B): F[B]
  }

  object Functor {
    implicit val optionFunctor: Functor[Option] = new Functor[Option] {
      override def map[A, B](opt: Option[A])(f: A => B): Option[B] = {
        opt match {
          case Some(a) => Some(f(a))
          case _ => None
        }
      }
    }
  }

  implicit class FunctorSyntax[F[_], A](fa: F[A]) {
    def doMap[B](f: A => B)(implicit FU: Functor[F]) = FU.map(fa)(f)
  }

  val x: Option[Int] = Some(5)
  x.doMap(_ * 3)

  case class State[S, A](run: S => (S, A))
  object State {
    implicit def stateMonad[S]: Monad[State[S, ?]] => new Monad[State[S, ?]] {
      def pure[A](a: A): State[S, A] => State(s => (s, a))

      def zip()
    }
  }
}
