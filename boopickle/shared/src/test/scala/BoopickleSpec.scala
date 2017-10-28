package test.sloth.boopickle

import org.scalatest._
import scala.concurrent.Future

import sloth.core._
import sloth.boopickle._

import cats.implicits._
import boopickle.Default._, java.nio.ByteBuffer

//shared
trait Api {
  def fun(a: Int): Future[Int]
}

//server
object ApiImpl extends Api {
  def fun(a: Int): Future[Int] = Future.successful(a)
}

class BoopickleSpec extends AsyncFreeSpec with MustMatchers {

 "run" in {
    object Transport extends RequestTransport[ByteBuffer, Future] {
      override def apply(request: Request[ByteBuffer]): Future[ByteBuffer] = Backend.router(request).fold(Future.failed(_), identity)
    }

    object Backend {
      import sloth.server._

      val server = Server.boopickle[Future]
      val router = server.route[Api](ApiImpl)
    }

    object Frontend {
      import sloth.client._

      val client = Client.boopickle[Future](Transport)
      val api = client.wire[Api]
    }

    Frontend.api.fun(1).map(_ mustEqual 1)
  }
}
