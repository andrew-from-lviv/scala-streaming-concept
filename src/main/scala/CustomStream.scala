object CustomStream {

  sealed trait Stream[+A] {
    def headOption: Option[A] = this match {
      case Empty => None
      case Cons(h, t) => Some(h())
    }

    def toListRecursive: List[A] = this match {
      case Cons(h, t) => h() :: t().toListRecursive
      case _ => List()
    }

    def toList: List[A] = {
      def move(s: Stream[A], acc: List[A]): List[A] = s match {
        case Cons(h, t) => move(t(), h() :: acc)
        case _ => acc
      }

      move(this, List()).reverse
    }

    def take(n: Int): Stream[A] = this match {
      case Cons(h, t) if n > 1 => Stream.cons(h(), t().take(n - 1))
      case Cons(h, _) if n == 1 => Stream.cons(h(), Stream.empty)
      case _ => Stream.empty
    }

    def existsSimple(p: A => Boolean): Boolean = this match {
      case Cons(h, t) => p(h()) || t().existsSimple(p)
      case _ => false
    }

    def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
      case Cons(h, t) => f(h(), t().foldRight(z)(f))
      case _ => z
    }

    def exists(p: A => Boolean): Boolean =
      foldRight(false)((a, b) => p(a) || b)

    def forAll(p: A => Boolean): Boolean =
      foldRight(true)((theHead, theLazyTail) => p(theHead) && theLazyTail)


    def drop(n: Int): Stream[A] = this match {
      case _ if n < 1 => this
      case Cons(_, t) if n == 1 => t()
      case Cons(_, t) if n > 1 => t().drop(n - 1)
      case _ => Stream.empty
    }

    def filter(p: A => Boolean): Stream[A] = this match{
      case Cons(h, t) if p(h()) => Stream.cons(h(), t().filter(p))
      case Cons(_, t) => t().filter(p)
      case _ => this
    }

    def map[B](f: A=>B):Stream[B] = this match{
      case Cons(h, t) => Stream.cons(f(h()), t().map(f))
      case _ => Stream.empty[B]
    }
  }


  case object Empty extends Stream[Nothing]

  case class Cons[+A](head: () => A, tail: () => Stream[A]) extends Stream[A]

  object Stream {
    def cons[A](h: => A, t: => Stream[A]): Stream[A] = {
      lazy val head = h
      lazy val tail = t
      Cons(() => head, () => tail)
    }

    def empty[A]: Stream[A] = Empty

    def apply[A](args: A*): Stream[A] =
      if (args.isEmpty) empty
      else cons(args.head, apply(args.tail: _*))

    def from(n: Int): Stream[Int] =
      cons(n, from(n + 1))

  }

  def main(args: Array[String]): Unit = {
    val stream = Stream(1, 2, 3) // Stream.apply(1,2,3) - the same
    println(stream.toList)
    println(stream.toListRecursive)

    val fibonacciStream = {
      def next(f0: Int, f1: Int): Stream[Int] = Stream.cons(f0, next(f1, f0 + f1))

      next(0, 1)
    }

    println(fibonacciStream.take(7).toList)


    //testing

    val primeStream = {
      def nextPrime(startingPoint: Int): Int ={
        def isPrime(i: Int): Boolean = {
          if(i <= 1) false
          else if( i == 2) true
          else !((2 to (i/2 + 1)) exists(x => i % x == 0))
        }

        if(isPrime(startingPoint + 1))
          startingPoint + 1
        else
          nextPrime(startingPoint + 1)
      }

      def next(p0: Int): Stream[Int] = Stream.cons(p0, next(nextPrime(p0)))

      next(2)
    }

    println(primeStream.take(10).toList)

    val inputStream = primeStream.take(10)

    println(inputStream.forAll(x => x > 10))
    println(inputStream.forAll(x => x < 40))

    println(Stream.empty[Int].forAll(x => x < 10))

    println(inputStream.drop(3).toList)

    println(inputStream.map(x => x * 10).toList)

    println(inputStream.filter(x => x < 20).toList)


  }
}
