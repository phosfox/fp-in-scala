package errorhandling

sealed trait Either[+E, +A] {
  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] = {
    traverse(es)(identity)
  }

  def traverse[E, A, B](as: List[A])(
    f: A => Either[E, B]): Either[E, List[B]] = {
    as match {
      case h :: t => (f(h) map2 traverse(t)(f)) (_ :: _)
      case Nil => Right(Nil)
    }
  }

  def Try[A](a: => A): Either[Exception, A] =
    try Right(a)
    catch {
      case e: Exception => Left(e)
    }

  def map[B](f: A => B): Either[E, B] = {
    this match {
      case Left(value) => Left(value)
      case Right(value) => Right(f(value))
    }
  }

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = {
    this match {
      case Left(value) => Left(value)
      case Right(value) => f(value)
    }
  }

  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = {
    this match {
      case Left(value) => Left(value)
      case Right(_) => b
    }
  }

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C):
  Either[EE, C] = {
    for {
      a <- this;
      bb <- b
    } yield f(a, bb)
  }

}


case class Left[+E](value: E) extends Either[E, Nothing]

case class Right[+A](value: A) extends Either[Nothing, A]