package example

import org.scalatest._
import cats.effect.std.Dispatcher
import cats.effect.IO
import cats.effect.std.Queue
import cats.effect.unsafe.implicits.global

class HelloSpec extends FlatSpec with Matchers {
  "The Hello object" should "say hello" in {
    Hello.greeting shouldEqual "hello"
  }

  abstract class ImpureInterface {
    def onMessage(msg: String): Unit

    def init(): Unit = {
      onMessage("init")
    }

  }

  "The dispatcher" should "not hang" in {
    Dispatcher[IO]
      .use { dispatcher =>
        for {
          queue <- Queue.unbounded[IO, String]
          impureInterface <-
            IO.delay {
              new ImpureInterface {
                override def onMessage(msg: String): Unit =
                  dispatcher.unsafeRunSync(queue.offer(msg))
              }
            }
          _ <- IO.delay(impureInterface.init())
          value <- queue.tryTake
        } yield value match {
          case Some(v) => println(s"Value found in queue! $v")
          case None    => println("Value not found in queue :(")
        }
      }
      .unsafeRunSync()
  }

  "Fibres" should "work" in {
    Dispatcher[IO]
      .use { dispatcher =>
        for {
          f <- IO.raiseError(new RuntimeException("x")).start
          o <- f.join
        } yield {
          o.isError shouldBe true
          o.isSuccess shouldBe false
          o.isCanceled shouldBe false
          o.fold(fail("c"), _ => succeed, fail("s"))
        }
      }
      .unsafeRunSync()
  }
}
