package fpinscala.state


trait RNG {
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.
}

object RNG {
  // NB - this was called SimpleRNG in the book text

  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
    }
  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (number, random) = rng.nextInt
    (if (number < 0) -(number+1) else number, random) // to avoid the corner case of number being MINValue
  }

  // generate a DOuble between 0 and 1 (exclusive)
  def double(rng: RNG): (Double, RNG) = {
    val (number, random) = nonNegativeInt(rng)
    (number/(Int.MaxValue.toDouble + 1), random)
  }

  def intDouble(rng: RNG): ((Int,Double), RNG) = {
    val (int, random1) = rng.nextInt
    val (doub, random2) = double(random1)
    ((int, doub), random2)
  }

  def doubleInt(rng: RNG): ((Double,Int), RNG) = {
    val ((int, doub), rand) = intDouble(rng)
    ((doub, int), rand)
  }

  def double3(rng: RNG): ((Double,Double,Double), RNG) = {
    val (double1, rand1) = double(rng)
    val (double2, rand2) = double(rand1)
    val (double3, rand3) = double(rand2)
    ((double1, double2, double3), rand3)
  }
     // too many duplications...

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    if (count < 0) (List(), rng)
    else {
      val (x, rand1) = rng.nextInt
      val (xs, rand2) = ints(count-1)(rng)
      ((x :: xs), rand2)
    }
  }


  // 6.5
  /* 
  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    } */
  def doubleByMap: Rand[Double] = { // Rand is RNG => (A, RNG)
    map(nonNegativeInt)(_ / Int.MaxValue.toDouble + 1)
    //  ^^^^^^^^^^^^^^  ^^ 
    //  Rand[Double]   (f: Int => Double)
  } 

  // map2 takes 2 actions ra and rb, and a function f for combining their results..
  // .. and returns a new action combining them
  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
    rng => {
      val (val1, rand1) = ra(rng)
      val (val2, rand2) = rb(rand1)
      (f(val1, val2), rand2)
    }
  }

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = ???

  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = ???
}

case class State[S,+A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] =
    ???
  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    ???
  def flatMap[B](f: A => State[S, B]): State[S, B] =
    ???
}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object State {
  type Rand[A] = State[RNG, A]
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = ???
}
