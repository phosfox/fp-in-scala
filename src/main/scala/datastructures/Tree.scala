package datastructures

sealed trait Tree[+A]

case class Leaf[A](value: A) extends Tree[A]

case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  def mapViaFold[A, B](t: Tree[A])(f: A => B): Tree[B] = {
    fold(t)(v => Leaf(f(v)): Tree[B])(Branch(_, _))
  }

  def sizeViaFold[A](t: Tree[A]): Int =
    fold(t)(a => 1)(1 + _ + _)

  def maximumViaFold(t: Tree[Int]): Int =
    fold(t)(a => a)(_ max _)

  def depthViaFold[A](t: Tree[A]): Int =
    fold(t)(a => 0)((d1, d2) => 1 + (d1 max d2))

  def fold[A, B](t: Tree[A])(f: A => B)(g: (B, B) => B): B = {
    t match {
      case Leaf(value) => f(value)
      case Branch(left, right) => g(fold(left)(f)(g), fold(right)(f)(g))
    }
  }

  def map[A, B](t: Tree[A])(f: A => B): Tree[B] = {
    t match {
      case Leaf(value) => Leaf(f(value))
      case Branch(left, right) => Branch(map(left)(f), map(right)(f))
    }
  }

  def depth[A](t: Tree[A]): Int = {
    var log2 = (x: Double) => Math.log10(x) / Math.log10(2.0)
    log2(size(t).toDouble).floor.toInt + 1
  }

  def depth2[A](t: Tree[A]): Int = {
    t match {
      case Leaf(_) => 1
      case Branch(left, right) => 1 + (depth2(left) max depth2(right))
    }
  }

  def size[A](t: Tree[A]): Int = {
    t match {
      case Leaf(_) => 1
      case Branch(left, right) => 1 + size(left) + size(right)
    }
  }

  def maximum(t: Tree[Int]): Int = {
    t match {
      case Leaf(value) => value
      case Branch(left, right) => maximum(left) max maximum(right)
    }
  }

  def apply[A](b: Branch[A]): Tree[A] = {
    b
  }

  def apply[A](l: Leaf[A], r: Leaf[A]): Tree[A] = {
    Branch(l, r)
  }
}

