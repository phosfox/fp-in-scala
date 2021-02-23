package errorhandling


sealed trait Option[+A] {
  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = {
    a match {
      case Nil => Some(Nil)
      case h :: t => map2(f(h), traverse(t)(f))(_ :: _)
    }
  }

  def sequence[A](a: List[Option[A]]): Option[List[A]] = {
    a match {
      case h :: t => h flatMap (hh => sequence(t) map (hh :: _))
      case Nil => Some(Nil)
    }
  }

  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = {
    a flatMap (aa => b map (bb => f(aa, bb)))
  }

  def lift[A, B](f: A => B): Option[A] => Option[B] = _ map f

  def variance(xs: Seq[Double]): Option[Double] = {
    mean(xs) flatMap (m => mean(xs.map(x => math.pow(x - m, 2))))
  }

  def mean(xs: Seq[Double]): Option[Double] = {
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)
  }

  def map[B](f: A => B): Option[B] = {
    this match {
      case None => None
      case Some(a) => Some(f(a))
    }
  }

  def flatMap[B](f: A => Option[B]): Option[B] = {
    this match {
      case None => None
      case Some(a) => f(a)
    }
  }

  def flatMap[B](f: A => Option[B]): Option[B] = {
    map(f) getOrElse None
  }

  def getOrElse[B >: A](default: => B): B = {
    this match {
      case None => default
      case Some(a) => a
    }
  }

  def orElse[B >: A](ob: => Option[B]): Option[B] = {
    this match {
      case None => ob
      case Some(a) => this
    }
  }

  def orElse_1[B >: A](ob: => Option[B]): Option[B] = {
    this map (Some(_)) getOrElse ob
  }

  def filter(f: A => Boolean): Option[A] = {
    this match {
      case Some(a) if f(a) => Some(a)
      case _ => None
    }
  }
}

case class Some[+A](get: A) extends Option[A]

case object None extends Option[Nothing]
