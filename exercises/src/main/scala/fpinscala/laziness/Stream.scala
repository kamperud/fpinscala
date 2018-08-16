package fpinscala.laziness

import Stream._

trait Stream[+A] {

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h, t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }

  def toList: List[A] =
    foldRight(Nil: List[A])((a, b) => a :: b)

  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 0 => Cons(h, () => t().take(n - 1))
    case Empty => Empty
    case Cons(h, t) if n <= 0 => Empty
  }

  def drop(n: Int): Stream[A] = this match {
    case Empty => Empty
    case Cons(h, t) if n > 1 => t().drop(n - 1)
    case Cons(h, t) if n <= 1 => t()
  }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if p(h()) => Cons(h, () => t().takeWhile(p))
    case _ => Empty
  }

  def takeWhile2(p: A => Boolean): Stream[A] =
    foldRight(empty[A])((h, t) => if (p(h)) cons(h, t) else empty)

  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((a, b) => p(a) && b)


  def headOption: Option[A] =
    foldRight(None: Option[A])((a, _) => Some(a))

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.

  def map[B](f: A => B): Stream[B] =
    foldRight(empty[B])((h, t) => cons(f(h), t))

  def filter(f: A => Boolean): Stream[A] =
    foldRight(empty[A])((h, t) =>
      if (f(h)) cons(h, t)
      else t)

  def append[B >: A](s: => Stream[B]): Stream[B] =
    foldRight(s)((h, t) => cons(h, t))

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(empty[B])((h, t) => f(h).append(t))

  def map2[B](f: A => B): Stream[B] = unfold(this)({
    case Empty => None
    case Cons(h, t) => Some((f(h()), t()))
  })

  def take2(n: Int): Stream[A] = unfold((this, n))({
    case (Cons(h,t), i) if i>=0 => Some((h(), (t(), i-1)))
    case (_, _) => None
  })

  def takeWhile3(p: A => Boolean): Stream[A] = unfold(this)({
    case Cons(h,t) if p(h()) => Some(h(), t())
    case _ => None
  })

  def zipWith[B,C](s2: Stream[B])(f: (A,B) => C):Stream[C] = unfold((this, s2))({
    case (Cons(h1,t1), Cons(h2,t2)) => Some(f(h1(),h2()), (t1(),t2()))
    case _ => None
  })

  def zipAll[B](s2: Stream[B]):Stream[(Option[A], Option[B])] = unfold((this,s2))({
    case (Cons(h1,t1), Cons(h2,t2)) => Some(((Some(h1()),Some(h2())), (t1(),t2())))
    case (Cons(h1,t1), Empty) => Some(((Some(h1()),None), (t1(),Empty)))
    case (Empty, Cons(h2,t2)) => Some(((None,Some(h2())), (Empty,t2())))
    case _ => None
  })

  def startsWith[B](s: Stream[B]): Boolean =
    this.zipAll(s).takeWhile(!_._2.isEmpty).forAll(e => e._1 == e._2)
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

  def constant[A](a: A): Stream[A] = Stream.cons(a, constant(a))

  def from(n: Int): Stream[Int] = Stream.cons(n, constant(n + 1))

  def fibs(): Stream[Int] = {
    def go(first: Int, second: Int): Stream[Int] =
      cons(first, go(second, first+second))

    go(0, 1)
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case None => Empty
    case Some((a, s)) => cons(a, unfold(s)(f))
  }

  val ones2: Stream[Int] = unfold(1)(_ => Some((1, 1)))

  def constant2[A](a: A): Stream[A] = unfold(1)(_ => Some((a, 1)))

  def from2(n: Int): Stream[Int] = unfold(n-1)(a => Some((a+1, a+1)))

  def fibs2(): Stream[Int] = unfold((0,1))(t => Some((t._1, (t._2, t._1+t._2))))
}

// Stream(1,2,3,4,5).takeWhile2(a =>{ println("hi");a<3}).toList
// Stream.from2(3).take(5).toList
