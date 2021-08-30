package fpinscala.errorhandling


import scala.{Option => _, Some => _, Either => _, _} // hide std library `Option`, `Some` and `Either`, since we are writing our own in this chapter
import scala.util.Try

sealed trait Option[+A] {
  // p.53, when we put methods in trait...
  // .. we get Object-oriented style of syntax
  // ---> `obj.fn(arg1)`, not fn(obj, arg1) like in datastructure chapter3, where methods are in companion object
  
  def map[B](f: A => B): Option[B] = this match { // why this?
    case None => None
    case Some(get) => Some(f(get))
    /* map is a:
      - a proceeding with a computation on assumption that an error has NOT occured
      - a way of deferring the error handling to later code
       */
  }
    
  def getOrElse[B>:A](default: => B): B = this match {
    case None => default
    case Some(get) => get
  }

  def flatMap[B](f: A => Option[B]): Option[B] = map(f).getOrElse(None)

  def orElse[B>:A](ob: => Option[B]): Option[B] = this.map(Some(_)).getOrElse(None)

  def orElse_1[B>:A](ob: => Option[B]): Option[B] = ob match {
    case None => ob
    case _ => this
  }

  def filter(f: A => Boolean): Option[A] = flatMap(value => if(f(value)) Some(value) else None)

  def filter_1(f: A => Boolean): Option[A] = this match {
    case Some(get) if f(get) => this
    case _ => None
  }
}

case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Option {
  def failingFn(i: Int): Int = {
    val y: Int = throw new Exception("fail!") // `val y: Int = ...` declares `y` as having type `Int`, and sets it equal to the right hand side of the `=`.
    try {
      val x = 42 + 5
      x + y
    }
    catch { case e: Exception => 43 } // A `catch` block is just a pattern matching block like the ones we've seen. `case e: Exception` is a pattern that matches any `Exception`, and it binds this value to the identifier `e`. The match returns the value 43.
  }

  def failingFn2(i: Int): Int = {
    try {
      val x = 42 + 5
      x + ((throw new Exception("fail!")): Int) // A thrown Exception can be given any type; here we're annotating it with the type `Int`
    }
    catch { case e: Exception => 43 }
  }

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)
  def variance(xs: Seq[Double]): Option[Double] = mean(xs).flatMap(m => mean(xs.map(x => math.pow(x - m, 2))))

  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = 
    a flatMap( // a, which is Option[A], flatmaps into aOrdinary, which is A
      aOrdinary => b.map(
        bOridynary => f(aOrdinary, bOridynary)
      )
    )

  def sequence[A](a: List[Option[A]]): Option[List[A]] = a match {
    case head :: next => head.flatMap( // head: A, next: List[Option[A]] , flatMap takes a function from A to Option[B]
      headOrdinary => sequence(next).map( // sequence takes a List[Option[A]], return Option[List[A]]
        headOrdinary :: _)) // here headOrdinary (A) is concatenated to the rest, making a List[A]
    //                                                  
    case Nil => None
  }// if a contains None even once, result is None; otherwise is a list of all the value

  /* Sometimes we want to map over a list using a function that might fail...
  ... returning None if applying it to any element of the list returning None
   */
  // def parseInts[A](a: List[String]): Option[List[A]] = sequence(a.map(i => Try(i.toInt)))
  // that is inefficient, because it traverese the list twice:
    // 1: convert each String to a Option[Int]
    // 2: combine these Option[Int] into Option[List[A]]
  

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = a match {
    case head :: next => map2(f(head), traverse(next)(f))(_ :: _)
    case Nil => None
  }

  def sequenceByTraverse[A](a: List[Option[A]]): Option[List[A]] = traverse(a)(x => x) // hay, dont forget f
}