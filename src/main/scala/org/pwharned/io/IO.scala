package org.pwharned.io
import scala.annotation.tailrec
import scala.concurrent.{ExecutionContext, Future, Await, Promise}
import scala.concurrent.duration.Duration
import scala.util.{Try, Success, Failure}

/** Drop-in improved IO preserving your original names and behavior */
sealed trait IO[+A]:
  def map[B](f: A => B): IO[B] = IO.FlatMapped(this, (a: A) => IO.Pure(f(a)))
  def flatMap[B](f: A => IO[B]): IO[B] = IO.FlatMapped(this, f)
  def unsafeRun(): A = IO.run(this)

  /** preserved API: optimized fast evaluation that falls back to full run */
  final inline def unsafeRunOptimized(): A = IO.unsafeRunOptimized(this)

  def *>[B](next: => IO[B]): IO[B] = this.flatMap(_ => next)

object IO:
  case class Pure[A](value: A) extends IO[A]
  case class Effect[A](thunk: () => A) extends IO[A]
  case class FlatMapped[A, B](source: IO[A], f: A => IO[B]) extends IO[B]

  def pure[A](a: A): IO[A] = Pure(a)
  def effect[A](a: => A): IO[A] = Effect(() => a)
  def println(s: String): IO[Unit] = Effect(() => scala.Predef.println(s))

  /** Blocking conversion: when IO is run it blocks waiting for the given Future
    * to complete
    */
  def fromFutureBlocking[A](fa: => Future[A], atMost: Duration = Duration.Inf)(
      using ec: ExecutionContext
  ): IO[A] =
    Effect(() => Await.result(fa, atMost))

  /** Non-blocking creation: registers callback on the Future now; when the IO
    * is run it blocks waiting on a small latch which the callback will fulfill.
    * This makes creation non-blocking while still providing simple semantics
    * when unsafeRun is called.
    */
  def fromFuture[A](fa: => Future[A])(using ec: ExecutionContext): IO[A] =
    // allocate Promise and attach callback immediately (non-blocking)
    val p = Promise[A]()
    try
      val f = fa
      f.onComplete {
        case Success(v) => p.trySuccess(v)
        case Failure(e) => p.tryFailure(e)
      }(ec)
    catch case t: Throwable => p.tryFailure(t)
    // when the IO is run we wait on the promise (blocking)
    Effect(() => Await.result(p.future, Duration.Inf))

  /** Trampolined, stack-safe interpreter (full) */
  def run[A](io0: IO[A]): A =
    var current: IO[Any] = io0.asInstanceOf[IO[Any]]
    var stack: List[Any => IO[Any]] = Nil

    @tailrec
    def loop(): Any =
      current match
        case Pure(v) =>
          stack match
            case h :: tail =>
              current =
                try h(v)
                catch case e: Throwable => throw e
              stack = tail
              loop()
            case Nil =>
              v
        case Effect(thunk) =>
          val v =
            try thunk()
            catch case e: Throwable => throw e
          current = Pure(v)
          loop()
        case FlatMapped(src, f) =>
          src match
            case Pure(v) =>
              current =
                try f(v)
                catch case e: Throwable => throw e
              loop()
            case Effect(th) =>
              val v =
                try th()
                catch case e: Throwable => throw e
              current =
                try f(v)
                catch case e: Throwable => throw e
              loop()
            case FlatMapped(innerSrc, g) =>
              // reassociate: (innerSrc.flatMap(g)).flatMap(f) => innerSrc.flatMap(x => g(x).flatMap(f))
              current = innerSrc.asInstanceOf[IO[Any]]
              val cont: Any => IO[Any] = (x: Any) =>
                g(x.asInstanceOf).flatMap(f.asInstanceOf).asInstanceOf[IO[Any]]
              stack = cont :: stack
              loop()
            case other =>
              val cont: Any => IO[Any] = (x: Any) =>
                f(x.asInstanceOf).asInstanceOf[IO[Any]]
              stack = cont :: stack
              current = src.asInstanceOf[IO[Any]]
              loop()

    loop().asInstanceOf[A]

  /** Optimized run used by unsafeRunOptimized: tries common fast shapes (Pure,
    * Effect, FlatMapped(Pure/Effect/...)) with a limited depth, falling back to
    * run() if the structure is more complex or we exceed depth.
    */
  def unsafeRunOptimized[A](io0: IO[A], maxDepth: Int = 1000): A =
    @tailrec
    def fastLoop(
        current: IO[Any],
        stack: List[Any => IO[Any]],
        depth: Int
    ): Any =
      if depth > maxDepth then
        // fallback to full interpreter to be correct for all shapes
        run(io0)
      else
        current match
          case Pure(v) =>
            stack match
              case h :: tail =>
                val next =
                  try h(v)
                  catch case e: Throwable => throw e
                fastLoop(next.asInstanceOf[IO[Any]], tail, depth + 1)
              case Nil => v
          case Effect(thunk) =>
            val v =
              try thunk()
              catch case e: Throwable => throw e
            fastLoop(Pure(v), stack, depth + 1)
          case FlatMapped(Pure(v), f) =>
            val next =
              try f(v)
              catch case e: Throwable => throw e
            fastLoop(next.asInstanceOf[IO[Any]], stack, depth + 1)
          case FlatMapped(Effect(thunk), f) =>
            val v =
              try thunk()
              catch case e: Throwable => throw e
            val next =
              try f(v)
              catch case e: Throwable => throw e
            fastLoop(next.asInstanceOf[IO[Any]], stack, depth + 1)
          case FlatMapped(FlatMapped(innerSrc, g), f) =>
            val cont: Any => IO[Any] = (x: Any) =>
              g(x.asInstanceOf).flatMap(f.asInstanceOf).asInstanceOf[IO[Any]]
            fastLoop(innerSrc.asInstanceOf[IO[Any]], cont :: stack, depth + 1)
          case FlatMapped(src, f) =>
            val cont: Any => IO[Any] = (x: Any) =>
              f(x.asInstanceOf).asInstanceOf[IO[Any]]
            fastLoop(src.asInstanceOf[IO[Any]], cont :: stack, depth + 1)

    fastLoop(io0.asInstanceOf[IO[Any]], Nil, 0).asInstanceOf[A]
