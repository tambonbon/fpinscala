package fpinscala.laziness

import Stream._
import org.scalactic.Bool
trait Stream[+A] {

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean = 
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }

  // 5.1
  def toList: List[A] = this match {
    case Cons(h, t) => h() :: t().toList // t() is Stream[A]
    case Empty => List() // List must have () here, as it's empty
  }

  def toListTailRec: List[A] = {
    @annotation.tailrec
    def go(stream: Stream[A], acc: List[A]): List[A] = stream match {
      case Cons(h, t) => go(t(), h() :: acc)
      case Empty => List()
    }
    go(this, List())
  }

  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 1 => cons(h(), t().take(n - 1))
    case Cons(h, _) if n == 1 => cons(h(), empty)
    case Empty => empty
  }

  /*
Unlike `take`, `drop` is not incremental. That is, it doesn't generate the
answer lazily. It must traverse the first `n` elements of the stream eagerly.
*/
  def drop(n: Int): Stream[A] = this match {
    case Cons(h, t) => t().drop(n - 1)
    case Empty => this
  }

  /** 
   * Return all starting elements of a Stream matching the given predicate
  */
  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if p(h()) => cons(h(), t().takeWhile(p)) 
    case Empty => empty
  }

  def takeWhileFoldRight(p: A => Boolean): Stream[A] = ???

  /** 
   * Check all elements in Stream match a given predicate.
   * It should terminate the traversal as soon as it encounters a nonmatching value
  */
  def forAll(p: A => Boolean): Boolean = this match {
    case Cons(h, t) => p(h()) && t().forAll(p)
    case _ => false
  }


  def headOption: Option[A] = this match {
    case Cons(h, t) => Some(h())
    case Empty => None
  }

  def headOptionFoldRight: Option[A] = foldRight(None: Option[A])((h, t) => Some(h))

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.

  def startsWith[B](s: Stream[B]): Boolean = ???
}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty 
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = Stream.cons(1, ones)
  def from(n: Int): Stream[Int] = ???

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = ???
}