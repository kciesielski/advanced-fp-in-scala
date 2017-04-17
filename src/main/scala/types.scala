package lambdaconf.types

import matryoshka._
import monocle._
import scalaz._

import Scalaz._

object exercise1 {
  type BoardRow[A] = (A, A, A, A, A, A, A, A)

  final case class Board8x8[A](matrix: BoardRow[BoardRow[A]])

  final case class CheckersBoard(/* ??? */)

  sealed trait CheckerPiece
  final case object BlackPiece
  final case object RedPiece
  // ???
}

object exercise2 {
  final case class Box[A](/* ??? */)
}

object exercise3 {

//  a different way of looking at polymorphic functions: functions that take types
//    identity:
//  A : Type => a : A => A
//  in some programming languages functions can return types
//
//  higher-order-kinds
//
  trait Traversable[F[_]]
//  ( * => *) => *
//
//  _[_]   * => *
//
  // trait Foo[T[_[_]]

  // 1. scala.collection.List * => *
  // 2. F[_, _] [*, *] => *
  // 3. Option * => *
  // 4. Int *
  // 5. T[_[_], _] [* => *, *] => *

  object Traversable {
    implicit val TraversableList: Traversable[List] = ???

    // this defines a family of types
    //
    implicit def TraversableMap[A]: Traversable[Map[A, ?]] = ???
  }

//  existentials and universals are duals, on the opposite side
//    function on universals - it has to deal with all possible types
//  existential - we have to deal with all possible types
//
  trait ListMap[A] {
    type B
    val list : List[B]
    val mapf : B => A

    def run : List[A] = list.map(mapf)
  }

  case class StateMachine[S, A](initial: S, update: S => (A, S))

  // alternative way to define existentials:
  trait StateMachine2[A] { // we don't have to pass type parameter
    type State
    def initial: State
    def update(old: State): (State, A)
  }
  // we want to force whoever deals with the steal machine to handle all possible types

  // yet another way to encode existentials
  trait HandleStateMachine[A, Z] {
    def apply[S](sm: StateMachine[S, A]): Z
  }

  trait ExistentialStateMachine[A] {
    def apply[Z](h: HandleStateMachine[A, Z]): Z
  }

  val sm: ExistentialStateMachine[Int] = ???

//  sm.apply[List[Int]](new HandleStateMachine[Int, List[Int]] {
//    override def apply[S](sm: StateMachine[S, Int]) = {
//      val initial: S = sm.initial
//    }
//  })

//  def gimmeStateMachine: StateMachine[S] forSome { type S } = ???
}

object exercise4 {
  trait FileSystem {
    // ???
  }
}

object exercise5 {
  sealed trait Example[F[_]] {
    def value: F[String]
  }

  val ExampleOption: Example[Option] = new Example[Option] {
    def value: Option[String] = Some("xxxx")
  }

  type EitherAInt[A] = Either[A, Int]

  val ExampleEither: Example[Either[?, Int]] = new Example[EitherAInt] {
    override def value: Either[String, Int] = Right(2)
  }

  val MapExample: Example[Map[?, Int]] = new Example[Map[?, Int]] {
    def value: Map[String, Int] = Map.empty.withDefault(_.length)
  }

  type Map2[A] = Map[String, A]

  val MapExample2: Example[Map2] = new Example[Map2] {
    def value: Map[String, String] = ???
  }

}
