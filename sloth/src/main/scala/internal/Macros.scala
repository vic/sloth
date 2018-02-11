package sloth.internal

import scala.reflect.macros.blackbox.Context
import cats.syntax.either._

class Translator[C <: Context](val c: C) {
  import c.universe._

  val corePkg = q"_root_.sloth.core"
  val serverPkg = q"_root_.sloth.server"
  val clientPkg = q"_root_.sloth.client"
  val internalPkg = q"_root_.sloth.internal"
  val macroThis = q"${c.prefix.tree}"

  private def abort(msg: String) = c.abort(c.enclosingPosition, msg)

  private def valid(check: => Boolean, errorMsg: => String): Either[String, Unit] = Either.cond(check, (), errorMsg)
  private def validateMethod(expectedReturnType: Type, symbol: MethodSymbol, methodType: Type): Either[String, (MethodSymbol, Type)] = for {
    _ <- methodType match {
      case _: MethodType | _: NullaryMethodType => Right(())
      case _: PolyType => Left(s"method ${symbol.name} has type parameters")
      case _ => Left(s"method ${symbol.name} has unsupported type")
    }
    methodResult = methodType.finalResultType.typeConstructor
    returnResult = expectedReturnType.finalResultType.typeConstructor
    _ <- valid(methodResult <:< returnResult, s"method ${symbol.name} has invalid return type, required: $methodResult <: $returnResult")
  } yield (symbol, methodType)

  //TODO rename overloaded methods to fun1, fun2, fun3 or append TypeSignature instead of number?
  private def validateAllMethods(tpe: Type, methods: List[(MethodSymbol, Type)]): List[Either[String, (MethodSymbol, Type)]] =
    methods.groupBy(m => methodPath(tpe, m._1)).map {
      case (_, x :: Nil) => Right(x)
      case (k, ms) => Left(s"""method $k is overloaded (rename the method or add a @PathName("other-name"))""")
    }.toList

  private def findPathName(annotations: Seq[Annotation]) = annotations.reverse.map(_.tree).collectFirst {
    case Apply(Select(New(annotation), _), Literal(Constant(name)) :: Nil) if annotation.tpe =:= typeOf[sloth.core.PathName] => name.toString
  }

  private def definedMethodsInType(tpe: Type): List[(MethodSymbol, Type)] = for {
    member <- tpe.decls.toList
    if member.isMethod
    if member.isPublic
    if !member.isConstructor
    symbol = member.asMethod
  } yield (symbol, symbol.typeSignatureIn(tpe))

  def supportedMethodsInType(tpe: Type, expectedReturnType: Type): List[(MethodSymbol, Type)] = {
    val methods = definedMethodsInType(tpe)
    val validatedMethods = methods.map { case (sym, tpe) => validateMethod(expectedReturnType, sym, tpe) }
    val validatedType = eitherSeq(validatedMethods)
      .flatMap(methods => eitherSeq(validateAllMethods(tpe, methods)))

    validatedType match {
      case Right(methods) => methods
      case Left(errors) => abort(s"type '$tpe' contains unsupported methods: ${errors.mkString(", ")}")
    }
  }

  def methodPath(tpe: Type, m: MethodSymbol): List[String] =
    findPathName(tpe.typeSymbol.annotations).getOrElse(tpe.typeSymbol.name.toString) ::
    findPathName(m.annotations).getOrElse(m.name.toString) ::
    Nil

  def paramsAsValDefs(m: Type): List[List[ValDef]] =
    m.paramLists.map(_.map(p => q"val ${p.name.toTermName}: ${p.typeSignature}"))

  def paramValuesAsHList(m: Type): Tree =
    valuesAsHList(m.paramLists.map(list => valuesAsHList(list.map(a => q"${a.name.toTermName}"))))

  def paramTypesAsHList(m: Type): Tree =
    typesAsHList(m.paramLists.map(list => typesAsHList(list.map(a => tq"${a.typeSignature}"))))

  def valuesAsHList[T](list: List[Tree]): Tree =
    list.foldRight[Tree](q"HNil")((b, a) => q"$b :: $a")

  def typesAsHList[T](list: List[Tree]): Tree =
    list.foldRight[Tree](tq"HNil")((b, a) => tq"$b :: $a")
}

object Translator {
  def apply[T](c: Context)(f: Translator[c.type] => c.Tree): c.Expr[T] = {
    val tree = f(new Translator(c))
    // println("XXX: " + tree)
    c.Expr(tree)
  }
}

object TraitMacro {
  def impl[Trait, PickleType, Result[_], ErrorType]
    (c: Context)
    (implicit traitTag: c.WeakTypeTag[Trait], resultTag: c.WeakTypeTag[Result[_]]): c.Expr[Trait] = Translator(c) { t =>
    import c.universe._

    val validMethods = t.supportedMethodsInType(traitTag.tpe, resultTag.tpe)

    val methodImplList = validMethods.collect { case (symbol, method) if symbol.isAbstract =>
      val path = t.methodPath(traitTag.tpe, symbol)
      val parameters =  t.paramsAsValDefs(method)
      val paramListType = t.paramTypesAsHList(method)
      val paramListValue = t.paramValuesAsHList(method)
      val innerReturnType = method.finalResultType.typeArgs.head

      q"""
        override def ${symbol.name}(...$parameters): ${method.finalResultType} = {
          impl.execute[$paramListType, $innerReturnType]($path, $paramListValue)
        }
      """
    }
    val methodImpls = if (methodImplList.isEmpty) List(EmptyTree) else methodImplList

    q"""
      import shapeless._

      val impl = new ${t.internalPkg}.ClientImpl(${t.macroThis})

      new ${traitTag.tpe.finalResultType} {
        ..$methodImpls
      }
    """
  }
}

object RouterMacro {
  import sloth.server.Router

  def impl[Trait, PickleType, Result[_]]
    (c: Context)
    (value: c.Expr[Trait])
    (functor: c.Expr[cats.Functor[Result]])
    (implicit traitTag: c.WeakTypeTag[Trait], pickleTypeTag: c.WeakTypeTag[PickleType], resultTag: c.WeakTypeTag[Result[_]]): c.Expr[Router[PickleType, Result]] = Translator(c) { t =>
    import c.universe._

    val validMethods = t.supportedMethodsInType(traitTag.tpe, resultTag.tpe)

    val methodCases = validMethods.map { case (symbol, method) =>
      val path = t.methodPath(traitTag.tpe, symbol)
      val paramListType = t.paramTypesAsHList(method)
      val argParams = method.paramLists.zipWithIndex.map { case (l, i) => List.tabulate(l.size)(j => q"{ val a = args($i); a($j) }") }
      val innerReturnType = method.finalResultType.typeArgs.head

      cq"""
        ${t.corePkg}.Request($path, payload) =>
          impl.execute[$paramListType, $innerReturnType]($path, payload) { args =>
            value.${symbol.name.toTermName}(...$argParams)
          }
      """
    }

    q"""
      import shapeless._

      val value = $value
      val impl = new ${t.internalPkg}.RouterImpl[${pickleTypeTag.tpe}, ${resultTag.tpe.typeConstructor}]()($functor)

      val current: ${t.serverPkg}.Router[${pickleTypeTag.tpe}, ${resultTag.tpe.typeConstructor}] = ${t.macroThis}
      current.orElse(new ${t.serverPkg}.Router[${pickleTypeTag.tpe}, ${resultTag.tpe.typeConstructor}] {
        override def apply(request: ${t.corePkg}.Request[${pickleTypeTag.tpe}]) = request match {
          case ..$methodCases
          case other => ${t.serverPkg}.RouterResult.Failure(Nil, ${t.corePkg}.ServerFailure.PathNotFound(other.path))
        }
      })

    """
  }
}