package Chapter2

import Chapter2.Exercises.{printLine, readLine, readFileZio}
import zio.{App, Task, UIO, ZIO}
//import zio.console._

object Exercises extends App {

  def run(commandLineArguments: List[String]) =
    ???

  // 1. Implement a ZIO version of the function `readFile` using `ZIO.effect` constructor
  def readFile(file: String): String = {
    val source = scala.io.Source.fromFile(file)
    try source.getLines().mkString
    finally source.close()
  }

  def readFileZio(file: String): Task[String] =
    ZIO.effect(readFile(file))

  // 2. Implement a ZIO version of the function `writeFile` using `ZIO.effect` constructor
  def writeFile(file: String, text: String): Unit = {
    import java.io._
    val pw = new PrintWriter(new File(file))
    try pw.write(text)
    finally pw.close()
  }

  def writeFileZio(file: String, text: String): Task[Unit] = {
    ZIO.effect(writeFile(file, text))
  }

  // 3. Using `flatMap`, together with `readFileZio` and `writeFileZio`, implement a ZIO version of `copyFile`
  def copyFile(source: String, dest: String): Unit = {
    val contents = readFile(source)
    writeFile(dest, contents)
  }

  def copyFileZio(source: String, dest: String): Task[Unit] = {
    for {
      copy <- readFileZio(source)
      _    <- writeFileZio(dest, copy)
    } yield ()
  }

  // 4. Rewrite the following ZIO code that uses `flatMap` into a for comprehension
  def printLine(line: String) = ZIO.effect(println(line))
  val readLine                = ZIO.effect(scala.io.StdIn.readLine())

  val getNameInputTask =
    // printLine("What is your name?").flatMap(_ => readLine.flatMap(name => printLine(s"Hello, $name")))
    for {
      _    <- printLine("What is your name?")
      name <- readLine
      _    <- printLine(s"Hello, $name")
    } yield ()

  // 5. Rewrite the following ZIO code that uses `flatMap` into a for comprehension
  val random = ZIO.effect(scala.util.Random.nextInt(3) + 1)

  val numberGuesser = {
//    random.flatMap { int =>
//      printLine("Guess a number from 1 to 3:").flatMap { _ =>
//        readLine.flatMap { num =>
//          if (num == int.toString) printLine("You guessed right!")
//          else printLine(s"You guessed wrong, the number was $int!")
//        }
//      }
//    }
    for {
      int   <- random
      _     <- printLine("Guess a number from 1 to 3:")
      guess <- readLine
      _ <- printLine {
        if (guess == int.toString) "You guessed right!"
        else s"You guessed wrong, the number was $int!"
      }
    } yield ()
  }

  final case class ToyZIO[-R, +E, +A](run: R => Either[E, A])

  // 6. Implement the `zipWIth` function in terms of the toy model of a ZIO effect.
  // The function should return an effect that sequentially composes the specified effects,
  // merging the results with the specified user-defined function
  def zipWith[R, E, A, B, C](self: ToyZIO[R, E, A], that: ToyZIO[R, E, B])(f: (A, B) => C): ToyZIO[R, E, C] = {
    ToyZIO { r: R =>
      for {
        a <- self.run(r)
        b <- that.run(r)
      } yield f(a, b)
    }
  }

  // 7. Implement the `collectAll` function in terms of the toy model of a ZIO effect.
  // The function should return an effect that sequentially collects
  // the results of the specified collections of effects
  def collectAll[R, E, A](in: Iterable[ToyZIO[R, E, A]]): ToyZIO[R, E, List[A]] = {
    ToyZIO { r: R =>
      def collectRecursive(effects: List[ToyZIO[R, E, A]], acc: List[A]): Either[E, List[A]] =
        effects match {
          case head :: tail => head.run(r).flatMap(succeed => collectRecursive(tail, succeed :: acc))
          case Nil          => Right(acc)
        }

      collectRecursive(in.toList, List.empty[A])
    }
  }

  // 8. Implement the `foreach` function in terms of the toy model of a ZIO effect.
  // The function should return an effect that sequentially runs the specified function
  // on every element of the specified collection
  def foreach[R, E, A, B](in: Iterable[A])(f: A => ToyZIO[R, E, B]): ToyZIO[R, E, List[B]] = {
    collectAll(in.map(f))
  }

  // 9. Implement the `orElse` function in terms of the toy model of a ZIO effect.
  // The function should return an effect that tries the left hand side, but if
  // that effect fails, it will fallback to the effect on the right hand side
  def orElse[R, E1, E2, A](
    self: ToyZIO[R, E1, A],
    that: ToyZIO[R, E2, A]
  ): ToyZIO[R, E2, A] = {
    ToyZIO { r =>
      self.run(r) match {
        case Right(a) => Right(a)
        case Left(_)  => that.run(r)
      }
    }
  }

  // 11. Using ZIO.fail and ZIO.succeed, implement the following function,
  // which converts an Either into a ZIO effect
  def eitherToZIO[E, A](either: Either[E, A]): ZIO[Any, E, A] =
    either match {
      case Left(e)        => ZIO.fail(e)
      case Right(succeed) => ZIO.succeed(succeed)
    }

  // 12. Using ZIO.fail and ZIO.succeed, implement the following function,
  // which converts a List into a ZIO effect, by looking at the head element in
  // the list and ignoring the rest of the elements
  def listToZIO[A](list: List[A]): ZIO[Any, None.type, A] =
    list.headOption match {
      case Some(head) => ZIO.succeed(head)
      case None       => ZIO.fail(None)
    }

  // 13. Using ZIO.effectTotal, convert the following procedural function into a ZIO function:
  def currentTime(): Long                          = System.currentTimeMillis()
  lazy val currentTimeZIO: ZIO[Any, Nothing, Long] = ZIO.effectTotal(currentTime())
  lazy val currentTimeUIO: UIO[Long]               = ZIO.effectTotal(currentTime())

  // 14. Using ZIO.effectAsync, convert the following asynchronous, callback based function into a ZIO function:
  def getCacheValue(
    key: String,
    onSuccess: String => Unit,
    onFailure: Throwable => Unit
  ): Unit =
    ???

  def getCacheValueZio(key: String): ZIO[Any, Throwable, String] =
    ZIO.effectAsync { callback =>
      getCacheValue(
        key,
        string => callback(ZIO.succeed(string)),
        throwable => callback(ZIO.fail(throwable))
      )
    }

  // 15. Using ZIO.effectAsync, convert the following asynchronous, callback based function into a ZIO function
  trait User

  def saveUserRecord(
    user: User,
    onSuccess: () => Unit,
    onFailure: Throwable => Unit
  ): Unit =
    ???

  def saveUserRecordZio(user: User): ZIO[Any, Throwable, Unit] =
    ZIO.effectAsync { callback =>
      saveUserRecord(
        user,
        () => callback(ZIO.succeed(())),
        throwable => callback(ZIO.fail(throwable))
      )
    }

  // 16. Using ZIO.fromFuture, convert the following code to ZIO
  import scala.concurrent.{ExecutionContext, Future}
  trait Query
  trait Result

  def doQuery(query: Query)(implicit ec: ExecutionContext): Future[Result] =
    ???

  def doQueryZio(query: Query): ZIO[Any, Throwable, Result] =
    ZIO.fromFuture { implicit ec =>
      doQuery(query)
    }

}

// 10. Using the following code as a foundation, write a ZIO application that
// prints out the contents of whatever files are passed into the program as
// command-line arguments. You should use the functions readFileZio
// and writeFileZio that you developed in these exercises, as well as ZIO.foreach
object Cat extends App {

  def workFlow(fileNames: List[String]) =
    ZIO.foreach(fileNames)(
      fileName =>
        for {
          contents <- readFileZio(fileName)
          _        <- printLine(contents)
        } yield ()
    )
  def run(commandLineArguments: List[String]) = workFlow(commandLineArguments).exitCode
}

// 17. Using the Console, write a little program that asks the user what their
// name is, and then prints it out to them with a greeting
object HelloHuman extends App {

  def run(args: List[String]) = {
    (for {
      _    <- printLine("What is your name na?")
      name <- readLine
      _    <- printLine(s"Hello, $name! Welcome to 1Q84!")
    } yield ()).exitCode
  }
}

// 18. Using the Console and Random services in ZIO, write a little program that
// asks the user to guess a randomly chosen number between 1 and 3, and
// prints out if they were correct or not
object NumberGuessing extends App {
  import zio.console._
  import zio.random._

  val program: ZIO[Console with Random, Throwable, Unit] =
    for {
      int   <- nextIntBetween(1, 4)
      _     <- putStrLn("Guess a number from 1 to 3")
      guess <- getStrLn
      _ <- putStrLn(
        if (guess == int.toString) "You guessed correctly!" else s"You guess incorrectly, the answer is $int"
      )
    } yield ()

  def run(args: List[String]) =
    program.exitCode
}

// 19. Using the Console service and recursion, write a function that will repeatedly read input from the console until the specified user-defined function
// evaluates to true on the input
object InputTruther extends App {
  import java.io.IOException
  import zio.console._

  def readUntil(
    acceptInput: String => Boolean
  ): ZIO[Console, IOException, String] =
    for {
      _     <- putStrLn("Some words to trigger user input")
      input <- getStrLn
      _     <- if (acceptInput(input)) putStrLn("Okay can") else readUntil(acceptInput)
    } yield input

  def run(args: List[String]) = readUntil { _ == args.headOption.getOrElse("foobazbar") }.exitCode
}

// 20. Using recursion, write a function that will continue evaluating the specified
// effect, until the specified user-defined function evaluates to true on the output of the effect
object EffectEvaluator {

  def doWhile[R, E, A](
    body: ZIO[R, E, A]
  )(condition: A => Boolean): ZIO[R, E, A] =
    for {
      someCalc <- body
      someA    <- if (condition(someCalc)) ZIO.succeed(someCalc) else doWhile(body)(condition)
    } yield someA
}
