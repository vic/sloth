package sloth

import cats.MonadError
import cats.syntax.all._

//TODO: is there a typeclass in cats for this "simpler" version of ApplicativeError and mapMaybe?
trait ClientResultError[Result[_], ErrorType] {
  def mapMaybe[T, R](result: Result[T])(f: T => Either[ErrorType, R]): Result[R]
  def raiseError[T](failure: ErrorType): Result[T]
}
object ClientResultError {
  implicit def fromMonadError[Result[_], ErrorType](implicit monad: MonadError[Result, ErrorType]): ClientResultError[Result, ErrorType] = new ClientResultError[Result, ErrorType] {
    def mapMaybe[T, R](result: Result[T])(f: T => Either[ErrorType, R]): Result[R] = result.flatMap(f andThen {
      case Right(v) => monad.pure(v)
      case Left(err) => monad.raiseError(err)
    })
    def raiseError[T](failure: ErrorType): Result[T] = monad.raiseError[T](failure)
  }
}

sealed trait ClientResultErrorConverted[Result[_]] extends ClientResultError[Result, ClientFailure]
object ClientResultErrorConverted {
  implicit def fromClientResultErrorCustom[Result[_], ErrorType](implicit c: ClientResultError[Result, ErrorType], converter: ClientFailureConvert[ErrorType]): ClientResultErrorConverted[Result] = new ClientResultErrorConverted[Result] {
    def mapMaybe[T, R](result: Result[T])(f: T => Either[ClientFailure, R]): Result[R] = c.mapMaybe(result)(f andThen (_.left.map(converter.convert)))
    def raiseError[T](failure: ClientFailure): Result[T] = c.raiseError[T](converter.convert(failure))
  }
}
