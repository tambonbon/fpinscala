package fpinscala.datastructures

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List { // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x,xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))
    }

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x,y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar


  def tail[A](l: List[A]): List[A] = l match {
    case Cons(head, tail) => tail
    case Nil => Nil
  }

  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Cons(head, tail) => Cons(h, tail)
    case Nil => Nil
  }

  def drop[A](l: List[A], n: Int): List[A] = l match {
    case Cons(head, tail) => drop(tail, n-1) // ? why n-1 ?
    case Nil => Nil
  }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Cons(head, tail) if (f(head)) => dropWhile(tail, f) // if before =>
    case _ => l
  }

  def init[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(head, Nil) => Nil
    case Cons(head, tail) => Cons(head, init(tail))
  }

  def length[A](l: List[A]): Int = 
    foldRight(l, 0)((head, acc) => acc + 1) // head is A

  @annotation.tailrec  
  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Cons(head, tail) => foldLeft(tail, f(z, head))(f)
    case Nil => z
    // in foldRight:
      // case Nil => z
      // case Cons(x, xs) => f(x, foldRight(xs, z)(f))
  }
    

  def sixteen(list: List[Int]): List[Int] = {
    foldRight(list, Nil: List[Int])((head, tail) => Cons(head + 1, tail))
  }

  def seventeen(list: List[Double]): List[String] = {
    foldRight(list, Nil: List[String])((head, tail) => Cons(head.toString(), tail)) 
  } // need Nil: List[String], not List[Int]
  // head: List[Double], tail: List[String]
  
  def map[A,B](l: List[A])(f: A => B): List[B] = {
    foldRight(l, Nil: List[B])((head, tail) => Cons(f(head), tail)) 
  } // head: A, tail: List[B] why?

  def filter[A](l: List[A])(f: A => Boolean): List[A] = {
    foldRight(l, Nil: List[A])((head, tail) => if (f(head)) Cons(head, tail) else tail)
  } // head: A, tail: List[A]

  def flatMap[A, B](l: List[A])(f: A => List[B]): List[B] = {
    foldLeft(l, Nil: List[B])((head, tail) => append(head, f(tail)))
  }

}

object TestList {
  def main(args: Array[String]): Unit = {
    import List._
    assert(flatMap(List(1,2,3))(i => List(i,i)) == List(1,1,2,2,3,3))    
    assert(flatMap(List(1,2,3))(i => List(i,i)) != List(1,1,2,2,3))    
  }
}