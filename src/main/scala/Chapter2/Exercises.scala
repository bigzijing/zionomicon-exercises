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
}

/**
  * Exercise 10. Using the following code as a foundation, write a ZIO application that prints out the contents of whatever files
  * are passed into the program as command-line arguments
  * You should use the function readFileZio that you developed in these exercises, as well as ZIO.foreach
  */
object Cat extends ZIOAppDefault {

  // sbt command: runMain Chapter2.Cat args1 args2
  // e.g. `runMain Chapter2.Cat chapter2_file1.txt chapter2.file2.txt`
  def run =
    (for {
      args <- ZIOAppArgs.getArgs.map(_.toList)
      _    <- ZIO.foreach(args)(readFileZio(_).flatMap(printLine))
    } yield ()).exitCode
}
