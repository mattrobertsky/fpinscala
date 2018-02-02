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
    foldRight(ns, 0)((x,_) => x + 1)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar


  def foldRight2[A,B](as: List[A], z: B, y: B)(f: (A, B) => B): B = // n.b. this breaks the ability to have the f return a different type
    as match {
      case Nil => z
      case Cons(x, _) if x == y => y
      case Cons(x, xs) => f(x, foldRight2(xs, z, y)(f))
    }

  // No you can't short the recursion using foldRight unless you rewrite it as above, because f has no control of the match
  def product3(ns: List[Double]) =
    foldRight2(ns, 1.0, 0.0)((a,b) => (a,b) match {
      case (x, y) => println("foo"); x * y
    })

  def tail[A](l: List[A]): List[A] = {
    l match {
      case Cons(_, xs) => xs
      case _ => Nil
    }
  }

  def setHead[A](l: List[A], h: A): List[A] = {
    l match {
      case Cons(x, xs) => Cons(h, xs)
      case _=> Cons(h, Nil)
    }
  }

  def drop[A](l: List[A], n: Int): List[A] = {
    l match {
      case Cons(x, Nil) if n > 0 => Nil//Cons(x, Nil)
      case Cons(x, xs) if n == 0 => Cons(x, xs)
      case Cons(x, xs) => drop(xs, n-1)
    }
  }

  def dropWhile[A](l: List[A])(f: A => Boolean): List[A] = {
    l match {
      case Cons(x, xs) if f(x) => dropWhile(xs)(f)
      case Cons(x, xs) => Cons(x, xs)
    }
  }

  def init[A](l: List[A]): List[A] = {
    def loop[A](l: List[A], m: List[A]): List[A] = {
      l match {
        case Nil => m
        case Cons(h, Nil) => m
        case Cons(h, t) => loop(t, append(m, List(h)))
      }
    }
    loop(l, Nil)
  }

  def length[A](l: List[A]): Int = {
    def loop[A](l: List[A], i: Int): Int = {
      l match {
        case Nil => i
        case Cons(h, t) => loop(t, i + 1)
      }
    }
    loop(l, 0)
  }

  def length2[A](l: List[A]):Int = {
    foldRight(l, 0)((_,b) => b + 1)
  }

  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = ???

  def map[A,B](l: List[A])(f: A => B): List[B] = ???
}
