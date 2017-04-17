package lambdaconf.functions

import matryoshka._
import monocle._
import scalaz._

import Scalaz._

object exercise1 {
  // Domain: {Vegetables, Fruits, Meat, Dairy, Eggs}
  // Codomain: {Love, Like, Neutral, Dislike, Hate}
}

object exercise2 {
  val compareStrings: (Char => Char) => (String, String) => Boolean = ???
}

object exercise3 {
  type Error = String
  type Parser[A] = String => Either[Error, (String, A)]

  // a different way of looking at polymorphic functions: functions that take types
  // A : Type => a : A => A
  // in some programming languages functions can return types

  def or[A](left: Parser[A], right: Parser[A]): Parser[A] = (input: String) =>
    left(input) match {
    case Left(_) => right(input)
    case x => x
  }
}

object exercise4 {
  def snd[A, B](v: (A, B)): B = ???
}
