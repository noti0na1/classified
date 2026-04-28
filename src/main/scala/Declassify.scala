package classified

import language.experimental.captureChecking
import language.experimental.modularity
import caps.*

@assumeSafe
class Policy[Lfloor <: Level, +Lsrc <: Lfloor, -T, +U] private (
    private val f: T ->{any.rd} U
  ):

  def apply(c: Classified[Lsrc, T])(using EnterCap[Lfloor]): Classified[Lfloor, U] =
    Classified[Lfloor, U] {
      given ReadCap[Lsrc] = null.asInstanceOf[ReadCap[Lsrc]]
      f(c.value)
    }

// Companion. Deliberately NOT `@assumeSafe`: policy authoring is
// outside safe mode by design. The trust model treats the policy
// author as honest, so they operate in a non-safe-mode module (a
// trusted bootstrap site) where they mint policies given a
// `DeclassifyCap[Lfloor]`. Safe-mode code can hold a `Policy` value
// and apply it via `c.declassify(p)`, but cannot construct one.
object Policy:
  def apply[Lfloor <: Level, Lsrc <: Lfloor, T, U](
      f: T ->{any.rd} U
    ): Policy[Lfloor, Lsrc, T, U]^{f} =
    new Policy[Lfloor, Lsrc, T, U](f)

extension [Lsrc <: Level, T](c: Classified[Lsrc, T])

  @assumeSafe
  def declassify[Lfloor >: Lsrc <: Level, U](
      p: Policy[Lfloor, Lsrc, T, U]
    )(using EnterCap[Lfloor]): Classified[Lfloor, U] =
    p(c)
