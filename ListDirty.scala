

sealed trait List[+A]

case object Nil extends List[Nothing]

case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def tail[A](ds: List[A]): List[A] = ds match {
    case Nil => Nil
    case Cons(x, xs) => xs
  }

  def setHead[A](ds: List[A], elem: A): List[A] = ds match {
    case Nil => List(elem)
    case Cons(_, h) => Cons(elem, h)
  }

  def apply[A](as: A*): List[A] = {
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
  }

  def drop[A](ds: List[A], n: Int): List[A] = ds match {
    case Nil => Nil
    case Cons(x, xs) => {
      if (n == 0) ds
      else drop(xs, n - 1)
    }
  }

  def dropWhile[A](ds: List[A], f: A => Boolean): List[A] = ds match {
    case Nil => Nil
    case Cons(x, xs) => {
      if (f(x)) dropWhile(xs, f)
      else xs
    }
  }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = as match {
    case Nil => z
    case Cons(x, xs) => f(x, foldRight(xs, z)(f))
  }

  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = as match {
    case Nil => z
    case Cons(x, xs) => foldLeft(xs,f(z,x))(f)
  }


}

object MyModule {
  def abs(n: Int): Int = if (n < 0) -n else n

  private def formatAbs(x: Int) = {
    val msg = "The absolute value of %d is %d"
    msg.format(x, abs(x))

  }

  def callList(): Unit = {
    //println(List(1,2,3,4))
    println(List.foldRight(List(1, 2, 3, 4), 0)((a, b) => 1 + b))
    println(List.foldLeft(List(1, 2, 3, 4), 0)((b, a) => b + 1))



  }

  def main(args: Array[String]): Unit = {
    println(List(1, 2, 3, 4))
  }
}



