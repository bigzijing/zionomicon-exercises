package Chapter3

import zio.Cause.{Die, Fail}
import zio._

object Exercises extends App {
  def run(args: List[String]) = ???

  // 1. Using the appropriate effect constructor, fix the following function so that
  // it no longer fails with defects when executed. Make a note of how the
  // inferred return type for the function changes
  def failWithMessage(string: String): UIO[Nothing] =
    ZIO.succeed(throw new Error(string))

  def catchDefects(string: String): ZIO[Any, Nothing, Unit] =
    failWithMessage(string).catchAllDefect(_ => ZIO.succeed(()))

  // 2. Using the ZIO `foldCauseM` operator and the Cause `defects` method,
  // implement the following function. This function should take the effect, inspect
  // defects, and if a suitable defect is found, it should recover from the error
  // with the help of the specified function, which generates a new success value for such a defect
  def recoverFromSomeDefects[R, E, A](zio: ZIO[R, E, A])(
    f: Throwable => Option[A]
  ): ZIO[R, E, A] =
    zio.foldCauseM(
      failure = errors => {
        val tryRecover = errors.defects.flatMap(f)
        if (tryRecover.nonEmpty) ZIO.succeed(tryRecover.head) else zio
      },
      success = _ => zio
    )

  // 3. Using the ZIO `foldCauseM` operator and the Cause `prettyPrint` method,
  // implement an operator that takes an effect, and returns a new effect that
  // logs any failures of the original effect (including errors and defects), without
  // changing its failure or success value
  def logFailures[R, E, A](zio: ZIO[R, E, A]): ZIO[R, E, A] =
    zio.foldCauseM(
      failure = cause => {
        console.putStrLn(cause.prettyPrint)
        zio
      },
      success = a => ZIO.succeed(a)
    )

  // 4. Using the ZIO `foldCauseM` method, which “runs” an effect to an Exit
  // value, implement the following function, which will execute the specified
  // effect on any failure at all
  def onAnyFailure[R, E, A](
    zio: ZIO[R, E, A],
    handler: ZIO[R, E, Any]
  ): ZIO[R, E, A] =
    zio.foldCauseM(
      failure = _ => handler *> zio,
      success = a => ZIO.succeed(a)
    )

  // 5. Using the ZIO `refineOrDie` method, implement the ioException function,
  // which refines the error channel to only include the IOException error
  def ioException[R, A](
    zio: ZIO[R, Throwable, A]
  ): ZIO[R, java.io.IOException, A] =
    zio.refineOrDie {
      case e: java.io.IOException => e
    }

  // 6. Using the ZIO `refineToOrDie` method, narrow the error type of the following effect to just NumberFormatException
  val parseNumber: ZIO[Any, Throwable, Int] =
    ZIO.effect("foo".toInt)

  val parseNumberNarrowed: ZIO[Any, NumberFormatException, Int] =
    parseNumber.refineToOrDie[NumberFormatException]

  // 7. Using the ZIO `foldM` method, implement the following two functions,
  // which make working with Either values easier, by shifting the unexpected
  // case into the error channel (and reversing this shifting).
  def left[R, E, A, B](
    zio: ZIO[R, E, Either[A, B]]
  ): ZIO[R, Either[E, B], A] =
    zio.foldM(
      failure = e => ZIO.fail(Left(e)),
      success = {
        case Left(aValue)  => ZIO.succeed(aValue)
        case Right(bValue) => ZIO.fail(Right(bValue))
      }
    )

  def unleft[R, E, A, B](
    zio: ZIO[R, Either[E, B], A]
  ): ZIO[R, E, Either[A, B]] =
    zio.foldM(
      failure = {
        case Left(e)       => ZIO.fail(e)
        case Right(bValue) => ZIO.succeed(Right(bValue))
      },
      success = a => ZIO.succeed(Left(a))
    )

  // 8. Using the ZIO `foldM` method, implement the following two functions,
  // which make working with Either values easier, by shifting the unexpected
  // case into the error channel (and reversing this shifting).
  def right[R, E, A, B](
    zio: ZIO[R, E, Either[A, B]]
  ): ZIO[R, Either[E, A], B] =
    zio.foldM(
      failure = e => ZIO.fail(Left(e)),
      success = {
        case Left(aValue)  => ZIO.fail(Right(aValue))
        case Right(bValue) => ZIO.succeed(bValue)
      }
    )

  def unright[R, E, A, B](
    zio: ZIO[R, Either[E, A], B]
  ): ZIO[R, E, Either[A, B]] =
    zio.foldM(
      failure = {
        case Left(e)       => ZIO.fail(e)
        case Right(aValue) => ZIO.succeed(Left(aValue))
      },
      success = b => ZIO.succeed(Right(b))
    )

  // 9. Using the ZIO#sandbox method, implement the following function.
  def catchAllCause[R, E1, E2, A](
    zio: ZIO[R, E1, A],
    handler: Cause[E1] => ZIO[R, E2, A]
  ): ZIO[R, E2, A] =
    zio.sandbox.foldM(
      failure = handler,
      success = a => ZIO.succeed(a)
    )

  // 10. Using the ZIO `foldCauseM` method, implement the following function.
  def catchAllCause[R, E1, E2, A](
    zio: ZIO[R, E1, A],
    handler: Cause[E1] => ZIO[R, E2, A]
  ): ZIO[R, E2, A] =
    zio.foldCauseM(
      failure = { e =>
        handler(e)
      },
      success = a => ZIO.succeed(a)
    )
}
