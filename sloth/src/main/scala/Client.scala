package sloth

import cats.~>
import sloth.internal.TraitMacro

//TODO: move implicits to wire method
class Client[PickleType, Result[_]](
  private[sloth] val transport: RequestTransport[PickleType, Result],
  private[sloth] val logger: LogHandler[Result]
)(implicit
  private[sloth] val resultError: ClientResultErrorConverted[Result]
) {

  def wire[T]: T = macro TraitMacro.impl[T, PickleType, Result]
}

object Client {
  def apply[PickleType, Result[_]](transport: RequestTransport[PickleType, Result], logger: LogHandler[Result] = LogHandler.empty[Result])(implicit clientResult: ClientResultError[Result, Throwable]): Client[PickleType, Result] = withError[PickleType, Result, Throwable](transport, logger)
  def withError[PickleType, Result[_], ErrorType](transport: RequestTransport[PickleType, Result], logger: LogHandler[Result] = LogHandler.empty[Result])(implicit clientResult: ClientResultError[Result, ErrorType], convert: ClientFailureConvert[ErrorType]): Client[PickleType, Result] = new Client(transport, logger)
}

trait RequestTransport[PickleType, Result[_]] { transport =>
  def apply(request: Request[PickleType]): Result[PickleType]

  def mapK[R[_]](f: Result ~> R): RequestTransport[PickleType, R] = new RequestTransport[PickleType, R] {
    def apply(request: Request[PickleType]): R[PickleType] = f(transport(request))
  }
}
object RequestTransport {
  def apply[PickleType, Result[_]](f: Request[PickleType] => Result[PickleType]) = new RequestTransport[PickleType, Result] {
    def apply(request: Request[PickleType]): Result[PickleType] = f(request)
  }
}

trait LogHandler[Result[_]] {
  def logRequest[T](path: List[String], argumentObject: Product, result: Result[T]): Result[T]
}
object LogHandler {
  def empty[Result[_]]: LogHandler[Result] = new LogHandler[Result] {
    def logRequest[T](path: List[String], argumentObject: Product, result: Result[T]): Result[T] = result
  }
}
