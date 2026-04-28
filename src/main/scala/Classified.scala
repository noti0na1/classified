package classified

import language.experimental.captureChecking
import language.experimental.modularity
import caps.*

import scala.util.control.NonFatal

@assumeSafe
trait Level

object Level:
  def baseContext: Context[Level] = null.asInstanceOf[Context[Level]]

@assumeSafe
trait ReadCap[+L <: Level] private extends SharedCapability

@assumeSafe
trait EnterCap[-L <: Level] private extends SharedCapability

@assumeSafe
trait Context[L <: Level] private extends SharedCapability

@assumeSafe
given [L <: Level](using ctx: Context[L]): (EnterCap[L]^{ctx}) =
  null.asInstanceOf[EnterCap[L]]

@assumeSafe
given [L <: Level](using ctx: Context[L]): (ReadCap[L]^{ctx}) =
  null.asInstanceOf[ReadCap[L]]

@assumeSafe
class Classified[-L <: Level, +T] private (private val v: Either[Throwable, T]):
  def value(using ReadCap[L]): T = v match
    case Right(t) => t
    case Left(e)  => throw e
  override def toString: String = "Classified(***)"
  override def hashCode: Int = 0

@assumeSafe
object Classified:
  def apply[L <: Level, T](
      op: Context[L] ?->{any.rd} T
    )(using EnterCap[L]): Classified[L, T] =
    val v =
      try Right(op(using null.asInstanceOf[Context[L]]))
      catch case NonFatal(e) => Left(e)
    new Classified(v)

  given [L <: Level, T](using rc: ReadCap[L]): (Conversion[Classified[L, T], T]^{rc}) =
    _.value

@assumeSafe
class ClassifiedRef[L <: Level, T] private (private var v: T):
  def value(using ReadCap[L]): T = v
  def update(newValue: T)(using Context[L]): Unit = v = newValue

@assumeSafe
object ClassifiedRef:
  def apply[L <: Level, T](init: T)(using EnterCap[L]): ClassifiedRef[L, T] =
    new ClassifiedRef(init)

extension [L <: Level, T](c: Classified[L, T])

  @assumeSafe
  def map[U](f: T ->{any.rd} Context[L] ?->{any.rd} U)(using EnterCap[L]): Classified[L, U] =
    Classified(f(c.value))

  @assumeSafe
  def flatMap[U](f: T ->{any.rd} Context[L] ?->{any.rd} Classified[L, U])(using EnterCap[L]): Classified[L, U] =
    Classified(f(c.value).value)

extension [L <: Level, T](c: Classified[L, Classified[L, T]])

  @assumeSafe
  def flatten(using EnterCap[L]): Classified[L, T] =
    Classified(c.value.value)
