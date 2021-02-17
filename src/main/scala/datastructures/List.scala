package datastructures

sealed trait List[+A]

case object Nil extends List[Nothing]

case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  @annotation.tailrec
  def startsWith[A](l: List[A], prefix: List[A]): Boolean = (l, prefix) match {
    case (_, Nil) => true
    case (Cons(h, t), Cons(h2, t2)) if h == h2 => startsWith(t, t2)
    case _ => false
  }

  @annotation.tailrec
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = sup match {
    case Nil => sub == Nil
    case _ if startsWith(sup, sub) => true
    case Cons(h, t) => hasSubsequence(t, sub)
  }

  def zipWith[A, B, C](l: List[A], r: List[B])(f: (A, B) => C): List[C] = {
    (l, r) match {
      case (Nil, _) => Nil
      case (_, Nil) => Nil
      case (Cons(lh, lt), Cons(rh, rt)) => Cons(f(lh, rh), zipWith(lt, rt)(f))
    }
  }

  def zipAdd(l: List[Int], r: List[Int]): List[Int] = {
    (l, r) match {
      case (Nil, _) => Nil
      case (_, Nil) => Nil
      case (Cons(lh, lt), Cons(rh, rt)) => Cons(lh + rh, zipAdd(lt, rt))
    }
  }

  def filterViaFlatMap[A](as: List[A])(f: A => Boolean): List[A] = {
    flatMap(as)(h => if (f(h)) Cons(h, Nil) else Nil)
  }

  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] = {
    concat(map(as)(f))
  }

  def filter[A](ls: List[A])(f: A => Boolean): List[A] = {
    foldRight(ls, Nil: List[A])((h, t) => if (f(h)) Cons(h, t) else t)
  }

  def mapViaFoldLeft[A, B](ls: List[A])(f: A => B): List[B] = {
    foldLeft(ls, Nil: List[B])((h, t) => Cons(f(t), h))
  }

  def map[A, B](ls: List[A])(f: A => B): List[B] = {
    foldRight(ls, Nil: List[B])((h, t) => Cons(f(h), t))
  }

  def toStr(ls: List[Double]): List[String] = {
    foldRight(ls: List[Double], Nil: List[String])((h, t) => Cons(h.toString, t))
  }

  def add1(ls: List[Int]): List[Int] = {
    foldRight(ls: List[Int], Nil: List[Int])((h, t) => Cons(h + 1, t))
  }

  def concat[A](ls: List[List[A]]): List[A] = {
    foldRight(ls, Nil: List[A])(append)
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h, t) => Cons(h, append(t, a2))
    }

  def appendViaFoldRight[A](as: List[A], bs: List[A]): List[A] = {
    foldRight(as, bs)(Cons(_, _))
  }

  def reverse[A](as: List[A]): List[A] = {
    foldLeft(as, List[A]())((h, t) => Cons(t, h))
  }

  def lengthLeft[A](as: List[A]): Int = {
    foldLeft(as, 0)((acc, _) => acc + 1)
  }

  def productLeft(ns: List[Double]): Double = {
    foldLeft(ns, 1.0)((x, y) => x * y)
  }

  def sumLeft(ns: List[Int]): Int = {
    foldLeft(ns, 0)((x, y) => x + y)
  }

  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = {
    as match {
      case Nil => z
      case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
    }
  }

  def length[A](as: List[A]): Int = {
    foldRight(as, 0)((_, y) => 1 + y)
  }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B =
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]): Int =
    foldRight(ns, 0)((x, y) => x + y)

  def product2(ns: List[Double]): Double =
    foldRight(ns, 1.0)(_ * _)

  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def tail[A](as: List[A]): List[A] = as match {
    case Nil => Nil
    case Cons(_, tail) => tail
  }

  def drop[A](l: List[A], n: Int): List[A] = {
    n match {
      case 0 => l
      case _ => l match {
        case Nil => Nil
        case Cons(_, t) => drop(t, n - 1)
      }
    }
  }

  def dropWhile[A](l: List[A])(f: A => Boolean): List[A] = {
    l match {
      case Nil => Nil
      case Cons(h, t) => if (f(h)) dropWhile(t)(f) else l
    }
  }

  def init[A](l: List[A]): List[A] = {
    l match {
      case Nil => Nil
      case Cons(_, Nil) => Nil
      case Cons(h, t) => Cons(h, init(t))
    }
  }

  def setHead[A](as: List[A], a: A): List[A] = as match {
    case Nil => List(a)
    case Cons(_, tail) => Cons(a, tail)
  }
}
