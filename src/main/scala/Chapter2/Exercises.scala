package Chapter2

import Chapter2.Exercises.{printLine, readFileZio}
import zio.{Task, ZIO, ZIOAppArgs, ZIOAppDefault}

object Exercises {

  /**
    * Exercise 1. Implement a ZIO version of the function readFile by using the ZIO.attempt constructor
    */
  def readFile(file: String): String = {
    val source = scala.io.Source.fromFile(file)

    try source.getLines().mkString
    finally source.close()
  }

  /**
    * Exercise 2. Implement a ZIO version of the function writeFile by using the ZIO.attempt constructor
    */
  def readFileZio(file: String): Task[String] =
    ZIO.attempt(readFile(file))

  def writeFile(file: String, text: String): Unit = {
    import java.io._

    val pw = new PrintWriter(new File((file)))
    try pw.write(text)
    finally pw.close
  }

  def writeFileZio(file: String, text: String): Task[Unit] =
    ZIO.attempt(writeFile(file, text))

  /**
    * Exercise 3. Using the flatMap method of ZIO effects, together with the readFileZio and writeFileZio functions that you wrote,
    * implement a ZIO version of the function copyFile
    */
  def copyFile(source: String, dest: String): Unit = {
    val contents = readFile(source)
    writeFile(dest, contents)
  }

  def copyFileZio(source: String, dest: String): ZIO[Any, Throwable, Unit] =
    readFileZio(source).flatMap { fileContents =>
      writeFileZio(dest, fileContents)
    }

  /**
    * Exercise 4. Rewrite the following ZIO code that uses flatMAp into a for comprehension
    */
  def printLine(line: String) = ZIO.attempt(println(line))
  val readLine                = ZIO.attempt(scala.io.StdIn.readLine())

  val exercise4: ZIO[Any, Throwable, Unit] =
    for {
      _    <- printLine("What is your name?")
      name <- readLine
      _    <- printLine(s"Hello, $name!")
    } yield ()

  /**
    * Exercise 5. Rewrite the following ZIO code that uses flatMap into a for comprehension
    */
  val random = ZIO.attempt(scala.util.Random.nextInt(3) + 1)

  val exercise5 =
    for {
      randomInt <- random
      _         <- printLine("Guess a number from 1 to 3:")
      number    <- readLine
      _ <- printLine("You guessed right!")
        .when(number == randomInt.toString)
        .someOrElseZIO(printLine(s"You guessed wrong, the number was $randomInt!"))
    } yield ()

  /**
    * Exercise 6. Implement the zipWith function in terms of the toy model of a ZIO effect
    * The function should return an effect that sequentially composes the specified effects,
    * merging their results with the specificed user-defined function
    */
  final case class ToyZIO[-R, +E, +A](run: R => Either[E, A])

  def zipWith[R, E, A, B, C](
    self: ToyZIO[R, E, A],
    that: ToyZIO[R, E, B]
  )(f: (A, B) => C): ToyZIO[R, E, C] =
    ToyZIO { r =>
      for {
        a <- self.run(r)
        b <- that.run(r)
      } yield f(a, b)
    }

  /**
    * Exercise 7. Implement the collectAll function in terms of the toy model of a ZIO effect
    * The function should return an effect that sequentially collects the results of the specified collection of effects
    */
  def collectAll[R, E, A](
    in: Iterable[ToyZIO[R, E, A]]
  ): ToyZIO[R, E, List[A]] =
    ToyZIO { r =>
      in.foldLeft(Right(List.empty[A]): Either[E, List[A]]) { (acc, next) =>
        acc match {
          case l @ Left(_) => l
          case Right(list) =>
            next.run(r) match {
              case Right(a) => Right(a :: list)
              case Left(e)  => Left(e)
            }
        }
      }
    }

  /**
    * Exercise 8. Implement the foreach function in terms of the toy model of a ZIO effect.
    * The function should return an effect that sequentially runs the specified function on every element of the specified collection
    */
  def foreach[R, E, A, B](
    in: Iterable[A]
  )(f: A => ToyZIO[R, E, B]): ToyZIO[R, E, List[B]] =
    collectAll(in.map(f))

  /**
    * Exercise 9. Implement the orElse function in terms of the toy model of a ZIO effect
    * The function should return an effect that tries the left hand side, but if that effect fails,
    * it will fallback to the effect on the right hand side
    */
  def orElse[R, E1, E2, A](
    self: ToyZIO[R, E1, A],
    that: ToyZIO[R, E2, A]
  ): ToyZIO[R, E2, A] =
    ToyZIO { r =>
      self.run(r) match {
        case Left(_)  => that.run(r)
        case Right(v) => Right(v)
      }
    }

  /**
    * Exercise 11. Using ZIO.fail and ZIO.succeed, implement the following function, which converts an Either into a ZIO effect
    */
  def eitherToZIO[E, A](either: Either[E, A]): ZIO[Any, E, A] =
    either match {
      case Right(v) => ZIO.succeed(v)
      case Left(e)  => ZIO.fail(e)
    }

  /**
    * Exercise 12. Using ZIO.fail and ZIO.succeed, implement that following function, which converts a List into a ZIO effect,
    * by looking at the head element in the list and ignoring the rest of the elements
    */
  def listToZIO[A](list: List[A]): ZIO[Any, None.type, A] =
    list.headOption match {
      case Some(a) => ZIO.succeed(a)
      case _       => ZIO.fail(None)
    }

  /**
    * Exercise 13. Using ZIO.succeed, convert the following procedural function into a ZIO function:
    */
  def currentTime(): Long = java.lang.System.currentTimeMillis()

  lazy val currentTimeZIO: ZIO[Any, Nothing, Long] =
    ZIO.succeed(currentTime())

  /**
    * Exercise 14. Using ZIO.async, convert the following asynchronous, callback-based function into a ZIO function
    */
  def getCacheValue(
    key: String,
    onSuccess: String => Unit,
    onFailure: Throwable => Unit
  ): Unit = ???

  def getCacheValueZio(key: String): ZIO[Any, Throwable, String] =
    ZIO.async { f =>
      getCacheValue(
        key,
        value => f(ZIO.succeed(value)),
        throwable => f(ZIO.fail(throwable))
      )
    }

  /**
    * Exercise 15. Using ZIO.async, convert the following asynchronous, callback-based function into a ZIO function
    */
  trait User

  def saveUserRecord(
    user: User,
    onSuccess: () => Unit,
    onFailure: Throwable => Unit
  ): Unit = ???

  def saveUserRecordZio(user: User): ZIO[Any, Throwable, Unit] =
    ZIO.async { f =>
      saveUserRecord(
        user,
        () => f(ZIO.unit),
        throwable => f(ZIO.fail(throwable))
      )
    }

  /**
    * Exercise 16. Using ZIO.fromFuture, convert the following code to ZIO
    */
  import scala.concurrent.{ExecutionContext, Future}
  trait Query
  trait Result

  def doQuery(query: Query)(implicit ec: ExecutionContext): Future[Result] = ???

  def doQueryZio(query: Query): ZIO[Any, Throwable, Result] =
    ZIO.fromFuture(ec => doQuery(query)(ec))

  /**
    * Exercise 19. Using the Console service and recursion, write a function that will repeatedly
    * read input from the console until the specified user-defined function evaluates to true on the input
    */
  import java.io.IOException

  def readUntil(
    acceptInput: String => Boolean
  ): ZIO[Any, IOException, String] =
    zio.Console.readLine.repeatUntil(acceptInput(_))

  /**
    * Exercise 20. Using recursion, write a function that will continue evaluating the specified effect,
    * until the specified user-defined function evaluates to true on the output of the effect
    */
  def doWhile[R, E, A](body: ZIO[R, E, A])(condition: A => Boolean): ZIO[R, E, A] =
    body.repeatUntil(condition(_))
}

/**
  * Exercise 10. Using the following code as a foundation, write a ZIO application that prints out the contents of whatever files
  * are passed into the program as command-line arguments
  * You should use the function readFileZio that you developed in these exercises, as well as ZIO.foreach
  */
// sbt command: runMain Chapter2.Cat args1 args2
// e.g. `runMain Chapter2.Cat chapter2_file1.txt chapter2.file2.txt`
object Cat extends ZIOAppDefault {

  def run =
    (for {
      args <- ZIOAppArgs.getArgs.map(_.toList)
      _    <- ZIO.foreach(args)(readFileZio(_).flatMap(printLine))
    } yield ()).exitCode
}

/**
  * Exercise 17. Using the Console, write a little program that asks the user what their name is,
  * and then prints it out to them with a greeting
  */
// sbt command: runMain Chapter2.HelloHuman
object HelloHuman extends ZIOAppDefault {

  def run =
    (for {
      name <- zio.Console.readLine("What is your name?")
      _    <- zio.Console.print(s"Hello, $name!")
    } yield ()).exitCode
}

/**
  * Exercise 18. Using the Console and Random services in ZIO, write a little program that asks the user
  * to guess a randomly chosen number between 1 and 3, and prints out if they were correct or not
  */
// runMain Chapter2.NumberGuessing
object NumberGuessing extends ZIOAppDefault {

  val isValidNumber = (n: String) =>
    n.toIntOption match {
      case Some(num) if 1 <= num && num <= 3 => true
      case _                                 => false
  }

  def run =
    (for {
      ran <- zio.Random.nextIntBetween(1, 4)
      num <- zio.Console.readLine(s"Guess a number between 1 and 3, inclusive: ")
      validated <- zio.Console
        .readLine(s"That was not a valid guess, please try again")
        .repeatUntil(isValidNumber(_))
        .when(!isValidNumber(num))
        .someOrElse(num)
      _ <- zio.Console
        .printLine(s"You were correct, the number was $ran!")
        .when(ran == validated.toInt)
        .someOrElseZIO(zio.Console.printLine(s"You're wrong, the number was $ran"))
    } yield ()).exitCode
}
