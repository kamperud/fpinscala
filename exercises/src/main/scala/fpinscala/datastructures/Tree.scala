package fpinscala.datastructures
//Branch(Leaf(3), Branch(Leaf(100),Leaf(2000)))


sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {

  def size[A](t: Tree[A]): Int =
    fold(t)(a=>1)(1+_+_)

  def maximum(t: Tree[Int]): Int =
    fold(t)(a=>a)(_.max(_))

  def depth[A](t: Tree[A]): Int =
    fold(t)(_=>1)(1 + _.max(_))

  def map[A,B](t: Tree[A])(f: A => B): Tree[B] =
    fold(t)(v => Leaf(f(v)):Tree[B])(Branch(_,_))

  def fold[A,B](t: Tree[A])(l: A => B)(b: (B,B) => B): B = t match {
    case Leaf(v) => l(v)
    case Branch(left, right) => b(fold(left)(l)(b), fold(right)(l)(b))
  }


}
