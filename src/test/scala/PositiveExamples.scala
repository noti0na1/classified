import language.experimental.captureChecking
import language.experimental.modularity
import language.experimental.safe
import caps.*

import classified.{*, given}

// Levels mirror the original region tree. The root (was BaseRegion) is
// the trait `Level` itself. Level traits live at top level so they can
// be referenced by both the safe-mode `Examples` body and by the
// non-safe test suites that forge contexts for them.
trait RegionLvl extends Level
trait OuterLvl  extends Level
trait InnerLvl  extends OuterLvl
trait ChildLvl  extends RegionLvl

// Diamond lattice: JoinL1 and JoinL2 are unrelated branches; JoinL3
// is a common subtype (more classified than both). Used to
// demonstrate that data at the JoinL3 join can read both branches
// (covariance of ReadCap) but cannot flow back down to either branch
// (contravariance of Classified blocks the descent).
trait JoinL1 extends Level
trait JoinL2 extends Level
trait JoinL3 extends JoinL1, JoinL2

// Top-level helper for the audit example. Lives outside `Examples` so
// that closures inside `auditRecord.map(...)` do not need to capture
// the `Examples.this` instance (which holds Refs and is therefore not
// an `{any.rd}` view).
private def maskEmail(s: String): String =
  val at = s.indexOf('@')
  if at <= 0 then "***"
  else s.take(1) + "***" + s.substring(at)

// Top-level Report so that constructing it inside a Classified.apply
// body does not capture `Examples.this`.
final case class Report(highRisk: Boolean, summary: String, totalSpent: Double)

// Positive examples that exercise the classified API. Compiled in safe
// mode (see `language.experimental.safe` above), so every method here
// typechecks under the full information-flow discipline.
//
// The privileged Context[Level] is taken as a using parameter at the
// class boundary. Test suites supply it outside safe mode (where the
// forging primitive is available); the safe-mode body itself only
// declares the dependency. From the supplied Context[Level] the
// library's givens derive `EnterCap[L]` for any descendant level (by
// contravariance) — substituting for the original
// `BaseRegion.ClassifiedCtx` plumbing.
class Examples(using Context[Level]):

  // ----------------------------------------------------------------
  // Section 1. Single region basics
  // ----------------------------------------------------------------

  def basicMap: Classified[RegionLvl, String] =
    Classified[RegionLvl, String]("hello").map(_.toUpperCase)

  def basicFlatMap: Classified[RegionLvl, Int] =
    val a = Classified[RegionLvl, Int](1)
    val b = Classified[RegionLvl, Int](2)
    a.flatMap { x => b.map { y => x + y } }

  def readOuterImmutable: Classified[RegionLvl, String] =
    val salt = "/salt"
    Classified[RegionLvl, String]("hello").map { (s: String) => s + salt }

  def autoConversionInsideEnter: Classified[RegionLvl, Int] =
    val ci = Classified[RegionLvl, Int](10)
    Classified[RegionLvl, Int] {
      val n: Int = ci
      n * 2
    }

  // ----------------------------------------------------------------
  // Section 1b. enter focused (Classified.apply with a context-fn body)
  // ----------------------------------------------------------------

  def pureEnter: Classified[RegionLvl, Int] =
    Classified[RegionLvl, Int] { 42 }

  def enterCombinesValues: Classified[RegionLvl, Int] =
    val a = Classified[RegionLvl, Int](10)
    val b = Classified[RegionLvl, Int](32)
    Classified[RegionLvl, Int] {
      val x: Int = a
      val y: Int = b
      x + y
    }

  def enterMutates: Classified[RegionLvl, Int] =
    val ref = ClassifiedRef[RegionLvl, Int](7)
    Classified[RegionLvl, Int] {
      ref.update(ref.value * 2)
      ref.value
    }

  def nestedEnter: Classified[RegionLvl, Int] =
    Classified[RegionLvl, Int] {
      val inner: Int = Classified[RegionLvl, Int] { 21 }
      inner * 2
    }

  def enterReadsClassifiedAndRef: Classified[RegionLvl, Int] =
    val secret = Classified[RegionLvl, Int](5)
    val ref    = ClassifiedRef[RegionLvl, Int](10)
    Classified[RegionLvl, Int] {
      val s: Int = secret
      ref.update(s + ref.value)
      ref.value
    }

  // ----------------------------------------------------------------
  // Section 2. Level hierarchy (typecheck-only)
  // ----------------------------------------------------------------

  def outerNestsInner: Classified[OuterLvl, Classified[InnerLvl, String]] =
    val co = Classified[OuterLvl, String]("o")
    val ci = Classified[InnerLvl, String]("i")
    // Inside co.map's body we hold Context[OuterLvl], which by
    // contravariance satisfies EnterCap[InnerLvl] since InnerLvl <:
    // OuterLvl.
    co.map { (x: String) => ci.map { (y: String) => x + y } }

  // ----------------------------------------------------------------
  // Section 2b. Implicit raise: lifting a Classified to a stricter level
  // ----------------------------------------------------------------
  //
  // In the new design, the parent-to-child raise is just contravariant
  // subtyping on `Classified[-L, T]`. A `Classified[RegionLvl, Int]`
  // may be widened to `Classified[ChildLvl, Int]` because
  // `ChildLvl <: RegionLvl`. No `raise` combinator is needed.

  def raiseSimple: Classified[ChildLvl, Int] =
    Classified[RegionLvl, Int](7)

  def raiseThenMap: Classified[ChildLvl, Int] =
    val raised: Classified[ChildLvl, Int] = Classified[RegionLvl, Int](10)
    raised.map(_ * 2)

  def raiseConsumedInsideChildEnter: Classified[ChildLvl, Int] =
    val raised: Classified[ChildLvl, Int] = Classified[RegionLvl, Int](7)
    Classified[ChildLvl, Int] {
      val n: Int = raised
      n + 100
    }

  // ----------------------------------------------------------------
  // Section 2d. Diamond join: legitimate "high reads low" at the L3
  // common subtype. The result is pinned to JoinL3 — see the
  // negative compile tests for the symmetric "cannot descend" cases.
  // ----------------------------------------------------------------

  def diamondJoin: Classified[JoinL3, Int] =
    val c1 = Classified[JoinL1, Int](1)
    val c2 = Classified[JoinL2, Int](2)
    Classified[JoinL3, Int] {
      // Inside this body Context[JoinL3] gives ReadCap[JoinL3], and
      // by ReadCap covariance that is also a valid ReadCap[JoinL1]
      // and ReadCap[JoinL2] — so the auto-conversion fires for both.
      val a: Int = c1
      val b: Int = c2
      a + b
    }

  // ----------------------------------------------------------------
  // Section 2c. flatten: collapse a same-level nested Classified
  // ----------------------------------------------------------------

  def flattenSimple: Classified[RegionLvl, Int] =
    val inner: Classified[RegionLvl, Classified[RegionLvl, Int]] =
      Classified[RegionLvl, Classified[RegionLvl, Int]](Classified[RegionLvl, Int](21))
    inner.flatten.map(_ * 2)

  def flattenViaMap: Classified[RegionLvl, String] =
    val nested: Classified[RegionLvl, Classified[RegionLvl, String]] =
      Classified[RegionLvl, String]("hello").map { (s: String) =>
        Classified[RegionLvl, String](s.toUpperCase)
      }
    nested.flatten

  // ----------------------------------------------------------------
  // Section 3. Practical patterns (all at RegionLvl)
  // ----------------------------------------------------------------

  // 3.1 Password storage. The hash lives in a level-local
  // ClassifiedRef[Int] populated lazily on the first store call.
  private val storedHash = ClassifiedRef[RegionLvl, Int](0)

  def passwordStore(pw: String): Classified[RegionLvl, Unit] =
    Classified[RegionLvl, String](pw).map { (p: String) => storedHash.update(p.hashCode) }

  def passwordVerify(pw: String): Classified[RegionLvl, Boolean] =
    Classified[RegionLvl, String](pw).map { (p: String) => p.hashCode == storedHash.value }

  // 3.2 Aggregation pipeline.
  private val aggBuf = ClassifiedRef[RegionLvl, List[String]](Nil)

  def aggregateIngest(s: String): Classified[RegionLvl, Unit] =
    Classified[RegionLvl, String](s).map { (x: String) => aggBuf.update(x.take(3) :: aggBuf.value) }

  def aggregateSnapshot: Classified[RegionLvl, List[String]] =
    Classified[RegionLvl, List[String]] { aggBuf.value }

  // 3.3 Sanitized audit log. `maskEmail` is a top-level helper (see
  // above) so that the closure passed to `.map` doesn't capture
  // `Examples.this` under the `{any.rd}` bound.
  private val auditBuf = ClassifiedRef[RegionLvl, List[String]](Nil)

  def auditRecord(addr: String): Classified[RegionLvl, Unit] =
    Classified[RegionLvl, String](addr).map { (raw: String) => auditBuf.update(maskEmail(raw) :: auditBuf.value) }

  def auditSnapshot: Classified[RegionLvl, List[String]] =
    Classified[RegionLvl, List[String]] { auditBuf.value }

  // 3.4 Validation pipeline.
  private val accepted = ClassifiedRef[RegionLvl, List[String]](Nil)

  def validate(input: String): Classified[RegionLvl, Boolean] =
    Classified[RegionLvl, String](input).map { (s: String) =>
      val ok = s.length >= 5 && s.contains('@')
      if ok then accepted.update(s :: accepted.value)
      ok
    }

  // 3.5 Mixed-type risk assessment inside one Classified.apply body.
  // `Report` is a top-level case class (see above) so that its
  // constructor does not capture `Examples.this` under `{any.rd}`.
  def riskAssessment: Classified[RegionLvl, Report] =
    val name      = Classified[RegionLvl, String]("alice")
    val age       = Classified[RegionLvl, Int](34)
    val balance   = Classified[RegionLvl, Double](12500.50)
    val txns      = Classified[RegionLvl, List[Double]](List(-50.0, 120.0, -300.0, 45.0))
    val threshold = Classified[RegionLvl, Double](10000.0)

    Classified[RegionLvl, Report] {
      val totalSpent: Double = txns.filter(_ < 0).map(d => -d).sum
      val recentNet:  Double = txns.sum
      val alert: Option[String] =
        if balance > threshold && age < 25 then Some("young high balance")
        else if totalSpent > balance * 0.5 then Some("excessive spending")
        else None
      val summary = alert match
        case Some(reason) => s"ALERT: $reason for ${(name: String).take(1)}***"
        case None         => s"OK: ${(name: String).take(1)}*** age ${age / 10}0s net $$${recentNet.round}"
      Report(alert.isDefined, summary, totalSpent)
    }

  // ----------------------------------------------------------------
  // Section 4. Compiled but no leak (sanity)
  // ----------------------------------------------------------------

  def hashCodeStaysClassified: Classified[RegionLvl, Int] =
    Classified[RegionLvl, String]("secret").map { (s: String) => s.hashCode }

  def equalityComparesWrapperIdentity: Boolean =
    val a = Classified[RegionLvl, String]("secret")
    val b = Classified[RegionLvl, String]("secret")
    a == b
