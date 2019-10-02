package test.sloth

import org.scalatest._
import scala.concurrent.Future
import scala.util.control.NonFatal
import sloth._
import cats.implicits._
import cats.{Id, ~>}
import monix.eval.Task
import monix.reactive.Observable
import monix.execution.Scheduler

import chameleon.ext.boopickle._
import boopickle.Default._
import java.nio.ByteBuffer
// import chameleon.ext.circe._
// import io.circe._, io.circe.syntax._, io.circe.generic.auto._

object Pickling {
  type PickleType = ByteBuffer
  // type PickleType = String
}
import Pickling._

trait EmptyApi
object EmptyApi extends EmptyApi

//shared
trait ExtendedApi[Result[_]] {
  def simple: Result[Int]
}
trait Api[Result[_]] extends ExtendedApi[Result] {
  def fun(a: Int, b: String = "drei"): Result[Int]
  def fun2(a: Int, b: String): Result[Int]
  def multi(a: Int)(b: Int): Result[Int]
}

//server
object ApiImplFuture extends Api[Future] {
  def simple: Future[Int] = Future.successful(1)
  def fun(a: Int, b: String): Future[Int] = Future.successful(a)
  def fun2(a: Int, b: String): Future[Int] = Future.successful(a)
  def multi(a: Int)(b: Int): Future[Int] = Future.successful(a)
  def stream(a: Int): Observable[Int] = Observable(a)
}
//or
case class ApiResult[T](event: String, result: Future[T])
object ApiImplResponse extends Api[ApiResult] {
  def simple: ApiResult[Int] = ApiResult("peter", Future.successful(1))
  def fun(a: Int, b: String): ApiResult[Int] = ApiResult("hans", Future.successful(a))
  def fun2(a: Int, b: String): ApiResult[Int] = ApiResult("hans", Future.successful(a))
  def multi(a: Int)(b: Int): ApiResult[Int] = ApiResult("hans", Future.successful(a + b))
}
//or
object TypeHelper { type ApiResultFun[T] = Int => ApiResult[T] }
import TypeHelper._
object ApiImplFunResponse extends Api[ApiResultFun] {
  def simple: ApiResultFun[Int] = i => ApiResult("peter", Future.successful(i))
  def fun(a: Int, b: String): ApiResultFun[Int] = i => ApiResult("hans", Future.successful(a + i))
  def fun2(a: Int, b: String): ApiResultFun[Int] = i => ApiResult("hans", Future.successful(a + i))
  def multi(a: Int)(b: Int): ApiResultFun[Int] = i => ApiResult("hans", Future.successful(a + b + i))
}

// mixed result type
trait MixedApi {
  def fun: Future[Int]
  def stream: Observable[Int]
}
object MixedApiImpl extends MixedApi {
  def fun: Future[Int] = Future.successful(1)
  def stream: Observable[Int] = Observable(1, 2)
}


// multi arg result type
trait MultiTypeArgApi[Single[_], Stream[_]] {
  def fun: Single[Int]
  def stream: Stream[Int]
}
object MultiTypeArgApiImpl extends MultiTypeArgApi[Future, Observable] {
  def fun = Future.successful(1)
  def stream = Observable(1, 2)
}

// type fun
trait TypeFunArgApi[Fun[F[_], _]] {
  def fun: Task[Int]
  def stream: Observable[Int]
  def state: Fun[Task, String]
  def sync: Int
}
object TypeFunArgApi {
  type Client[F[_], T] = F[T]
  type Server[F[_], T] = String => F[T]
}
object TypeFunArgApiImpl extends TypeFunArgApi[TypeFunArgApi.Server] {
  def fun = Task.pure(0)
  def stream = Observable(1, 2)
  def state = state => Task.pure(state)
  def sync = 5
}

class SlothSpec extends AsyncFreeSpec with MustMatchers {
  override implicit def executionContext: Scheduler = Scheduler.global // bring in sync with observable executioncontext

  import cats.derived.auto.functor._

  "run simple" in {
    object Backend {
      val router = Router[PickleType, Future]
        .route(EmptyApi)
        .route[Api[Future]](ApiImplFuture)
    }

    object Frontend {
      object Transport extends RequestTransport[PickleType, Future] {
        override def apply(request: Request[PickleType]): Future[PickleType] =
          Backend.router(request).toEither match {
            case Right(result) => result
            case Left(err) => Future.failed(new Exception(err.toString))
          }
      }

      val client = Client[PickleType, Future](Transport)
      val api = client.wire[Api[Future]]
      val emptyApi = client.wire[EmptyApi]
    }

    Frontend.api.fun(1).map(_ mustEqual 1)
  }

 "run different result types" in {
    import cats.data.EitherT

    sealed trait ApiError
    case class SlothClientError(failure: ClientFailure) extends ApiError
    case class SlothServerError(failure: ServerFailure) extends ApiError
    case class UnexpectedError(msg: String) extends ApiError

    implicit def clientFailureConvert = new ClientFailureConvert[ApiError] {
      def convert(failure: ClientFailure) = SlothClientError(failure)
    }

    type ClientResult[T] = EitherT[Future, ApiError, T]

    object Backend {
      val router = Router[PickleType, ApiResult]
        .route[Api[ApiResult]](ApiImplResponse)
    }

    object Frontend {
      object Transport extends RequestTransport[PickleType, ClientResult] {
        override def apply(request: Request[PickleType]): ClientResult[PickleType] = EitherT(
          Backend.router(request).toEither match {
            case Right(ApiResult(event@_, result)) =>
              result.map(Right(_)).recover { case NonFatal(t) => Left(UnexpectedError(t.getMessage)) }
            case Left(err) => Future.successful(Left(SlothServerError(err)))
          })
      }

      val client = Client[PickleType, ClientResult](Transport)
      val api = client.wire[Api[ClientResult]]
    }

    Frontend.api.fun2(1, "AAAA")
    Frontend.api.multi(11)(3)
    Frontend.api.fun(1).value.map(_.right.get mustEqual 1)
  }

 "run different result types with fun" in {

    object Backend {
      val router = Router[PickleType, ApiResultFun]
        .route[Api[ApiResultFun]](ApiImplFunResponse)
    }

    object Frontend {
      object Transport extends RequestTransport[PickleType, Future] {
        override def apply(request: Request[PickleType]): Future[PickleType] =
          Backend.router(request).toEither.fold(err => Future.failed(new Exception(err.toString)), _(10).result)
      }

      val client = Client[PickleType, Future](Transport)
      val api = client.wire[Api[Future]]
    }

    Frontend.api.fun(1).map(_ mustEqual 11)
  }

  "run mixed result types" in {
    implicit val futureToObservable: ResultMapping[Future, Observable] = new ResultMapping[Future, Observable] {
      def apply[T](result: Future[T]): Observable[T] = Observable.fromFuture(result)
    }
    implicit val observableToFuture: ResultMapping[Observable, Future] = new ResultMapping[Observable, Future] {
      def apply[T](result: Observable[T]): Future[T] = result.lastL.runToFuture
    }

    object Backend {
      val router = Router[PickleType, Observable]
        .route[MixedApi](MixedApiImpl)
    }

    object Frontend {
      object Transport extends RequestTransport[PickleType, Observable] {
        override def apply(request: Request[PickleType]): Observable[PickleType] =
          Backend.router(request).toEither match {
            case Right(result) => result
            case Left(err) => Observable.raiseError(new Exception(err.toString))
          }
      }

      val client = Client[PickleType, Observable](Transport)
      val api = client.wire[MixedApi]
    }

    for {
      fun <- Frontend.api.fun
      stream <- Frontend.api.stream.foldLeftL[List[Int]](Nil)((l,i) => l :+ i).runToFuture
    } yield {
      fun mustEqual 1
      stream mustEqual List(1, 2)
    }
  }

  "run multi arg result type" in {
    implicit val singleToObservable: ResultMapping[Future, Observable] =
      ResultMapping(Lambda[Future ~> Observable](Observable.fromFuture(_)))
    implicit val observableToFuture: ResultMapping[Observable, Future] =
      ResultMapping(Lambda[Observable ~> Future](_.lastL.runToFuture))

    object Backend {
      val router = Router[PickleType, Observable]
        .route[MultiTypeArgApi[Future, Observable]](MultiTypeArgApiImpl)
    }

    object Frontend {
      object Transport extends RequestTransport[PickleType, Observable] {
        override def apply(request: Request[PickleType]): Observable[PickleType] =
          Backend.router(request).toEither match {
            case Right(result) => result
            case Left(err) => Observable.raiseError(new Exception(err.toString))
          }
      }

      val client = Client[PickleType, Observable](Transport)
      val api = client.wire[MultiTypeArgApi[Future, Observable]]
    }

    for {
      fun <- Frontend.api.fun
      stream <- Frontend.api.stream.foldLeftL[List[Int]](Nil)((l,i) => l :+ i).runToFuture
    } yield {
      fun mustEqual 1
      stream mustEqual List(1, 2)
    }
  }

  "run result fun type" in {
    import cats.derived.auto.functor._

    case class Transferable[T](f: String => Observable[T])

    implicit val singleToTransferable: ResultMapping[Task, Transferable] =
      ResultMapping(Lambda[Task ~> Transferable](f => Transferable(_ => Observable.fromTask(f))))
    implicit val streamToTransferable: ResultMapping[Observable, Transferable] =
      ResultMapping(Lambda[Observable ~> Transferable](o => Transferable(_ => o)))
    implicit val syncToTransferable: ResultMapping[Id, Transferable] =
      ResultMapping(Lambda[Id ~> Transferable](v => Transferable(_ => Observable.now(v))))
    implicit def stateFunToTransferable[F[_]](implicit mapping: ResultMapping[F, Transferable]): ResultMapping[TypeFunArgApi.Server[F, ?], Transferable] =
      new ResultMapping[TypeFunArgApi.Server[F, ?], Transferable] {
        override def apply[T](f: TypeFunArgApi.Server[F, T]): Transferable[T] = Transferable { state =>
          mapping(f(state)).f(state)
        }
      }
    implicit val observableToTask: ResultMapping[Observable, Task] =
      ResultMapping(Lambda[Observable ~> Task](_.lastL))
    implicit val observableToId: ResultMapping[Observable, Id] =
      ResultMapping(Lambda[Observable ~> Id](o => {
        // cannot use Await.result in js, just poll for the sake of this test
        val fut = o.headL.runToFuture
        while (!fut.isCompleted) {}
        fut.value.get.get
      }))

    object Backend {
      val router = Router[PickleType, Transferable]
        .route[TypeFunArgApi[TypeFunArgApi.Server]](TypeFunArgApiImpl)
    }

    object Frontend {
      object Transport extends RequestTransport[PickleType, Observable] {
        override def apply(request: Request[PickleType]): Observable[PickleType] =
          Backend.router(request).toEither match {
            case Right(result) => result.f("harals")
            case Left(err) => Observable.raiseError(new Exception(err.toString))
          }
      }

      val client = Client[PickleType, Observable](Transport)
      val api = client.wire[TypeFunArgApi[TypeFunArgApi.Client]]
    }

    for {
      fun <- Frontend.api.fun.runToFuture
      stream <- Frontend.api.stream.foldLeftL[List[Int]](Nil)((l,i) => l :+ i).runToFuture
      state <- Frontend.api.state.runToFuture
    } yield {
      fun mustEqual 0
      stream mustEqual List(1, 2)
      state mustEqual "harals"
      Frontend.api.sync mustEqual 5
    }
  }
}
