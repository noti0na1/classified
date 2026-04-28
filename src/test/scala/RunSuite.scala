import language.experimental.captureChecking
import language.experimental.modularity

import classified.{*, given}

// Runtime suite that executes the safe-mode `Examples` and asserts on
// the extracted values. This file intentionally does NOT import
// `language.experimental.safe`, so it can use `null.asInstanceOf` to
// manufacture both the privileged `Context[Level]` (passed in to
// instantiate the safe-mode `Examples` class) and the per-level
// `ReadCap` instances needed to extract via `.value`. The forged
// instances are null at runtime; the cap traits are SharedCapability
// with no observable state, so the methods that take them as typing
// tokens work regardless of identity.
class RunSuite extends munit.FunSuite:

  // Helper that manufactures a fake capability instance. Legal here
  // because this file does not import `language.experimental.safe`.
  private inline def fake[T]: T = null.asInstanceOf[T]

  // Privileged Context[Level] forged outside safe mode and supplied
  // to `Examples`. Inside safe-mode Examples this is just a using
  // parameter; the asInstanceOf stays here.
  private given Context[Level] = fake

  // Forged ReadCap at the most specific level used by the suite.
  // `ReadCap[+L]` is covariant, so `ReadCap[ChildLvl]` is also a
  // valid `ReadCap[RegionLvl]` (high reads low). One given covers
  // every `.value` extraction in the main region tree.
  private given ReadCap[ChildLvl] = fake

  // Separate ReadCap for the diamond example — `JoinL3` is the join
  // of `JoinL1`/`JoinL2`, so `ReadCap[JoinL3]` covers all three
  // diamond levels via covariance. The two givens stay unambiguous
  // because `ChildLvl` and `JoinL3` live in different branches of
  // the level lattice.
  private given ReadCap[JoinL3] = fake

  private val examples = new Examples

  // ----------------------------------------------------------------
  // Section 1. Single region basics
  // ----------------------------------------------------------------

  test("basicMap uppercases the wrapped value") {
    assertEquals(examples.basicMap.value, "HELLO")
  }

  test("basicFlatMap composes two Classifieds at the same level") {
    assertEquals(examples.basicFlatMap.value, 3)
  }

  test("readOuterImmutable concatenates an outer constant inside map") {
    assertEquals(examples.readOuterImmutable.value, "hello/salt")
  }

  test("autoConversionInsideEnter doubles the value through the conversion") {
    assertEquals(examples.autoConversionInsideEnter.value, 20)
  }

  // ----------------------------------------------------------------
  // Section 1b. enter focused
  // ----------------------------------------------------------------

  test("pureEnter wraps a constant") {
    assertEquals(examples.pureEnter.value, 42)
  }

  test("enterCombinesValues sums two Classifieds via auto conversion") {
    assertEquals(examples.enterCombinesValues.value, 42)
  }

  test("enterMutates updates and returns the local ref") {
    assertEquals(examples.enterMutates.value, 14)
  }

  test("nestedEnter lifts the inner enter result via auto conversion") {
    assertEquals(examples.nestedEnter.value, 42)
  }

  test("enterReadsClassifiedAndRef combines auto-converted value with ref state") {
    assertEquals(examples.enterReadsClassifiedAndRef.value, 15)
  }

  // ----------------------------------------------------------------
  // Section 1c. raise (now implicit) and flatten
  // ----------------------------------------------------------------

  test("raiseSimple lifts a region-level Classified into the more-classified child") {
    assertEquals(examples.raiseSimple.value, 7)
  }

  test("raiseThenMap composes the implicit raise with a child-level map") {
    assertEquals(examples.raiseThenMap.value, 20)
  }

  test("raiseConsumedInsideChildEnter reads the raised wrapper inside child.enter") {
    assertEquals(examples.raiseConsumedInsideChildEnter.value, 107)
  }

  test("flattenSimple collapses a same-level nested Classified") {
    assertEquals(examples.flattenSimple.value, 42)
  }

  test("flattenViaMap unwraps a Classified produced inside map") {
    assertEquals(examples.flattenViaMap.value, "HELLO")
  }

  test("diamondJoin reads from JoinL1 and JoinL2 at the JoinL3 join") {
    // The result is `Classified[JoinL3, Int]`, pinned to the join.
    // The negative compile suite verifies that this value cannot be
    // narrowed back down to JoinL1 or JoinL2.
    assertEquals(examples.diamondJoin.value, 3)
  }

  // ----------------------------------------------------------------
  // Section 2. Practical patterns
  // ----------------------------------------------------------------

  test("password verify returns true after a matching store") {
    examples.passwordStore("hunter2").value
    assertEquals(examples.passwordVerify("hunter2").value, true)
    assertEquals(examples.passwordVerify("wrong").value, false)
  }

  test("integrity: passwordVerify is reachable only through privileged minting") {
    // The legitimate flow: privileged code mints the candidate string
    // wrapper inside `passwordVerify` and the verifier returns the
    // expected boolean. The companion compile-time test
    // `[integrity] forging a Classified[String] without an EnterCap is
    // rejected` proves that an unprivileged attacker cannot mint such
    // a wrapper to feed in directly. Together the two halves pin down
    // the integrity property.
    examples.passwordStore("hunter2").value
    assertEquals(examples.passwordVerify("hunter2").value, true)
    assertEquals(examples.passwordVerify("forged").value, false)
  }

  test("aggregation pipeline collects 3-letter prefixes in reverse insertion order") {
    examples.aggregateIngest("alpha").value
    examples.aggregateIngest("bravo").value
    examples.aggregateIngest("charlie").value
    assertEquals(examples.aggregateSnapshot.value, List("cha", "bra", "alp"))
  }

  test("sanitized log masks email addresses before storing") {
    examples.auditRecord("alice@example.com").value
    examples.auditRecord("bob@example.com").value
    assertEquals(
      examples.auditSnapshot.value,
      List("b***@example.com", "a***@example.com")
    )
  }

  test("validation accepts a long enough address with @ and rejects others") {
    assertEquals(examples.validate("alice@example.com").value, true)
    assertEquals(examples.validate("nope").value, false)
    assertEquals(examples.validate("a@b").value, false)
  }

  test("risk assessment flags excessive spending over half the balance") {
    val report = examples.riskAssessment.value
    assertEquals(report.totalSpent, 350.0)
    assert(report.summary.startsWith("OK"), s"unexpected summary: ${report.summary}")
    assertEquals(report.highRisk, false)
  }

  // ----------------------------------------------------------------
  // Section 4. Sanity: hashCode channel stays inside Classified
  // ----------------------------------------------------------------

  test("hashCodeStaysClassified yields the same Int as the underlying string's hashCode") {
    assertEquals(examples.hashCodeStaysClassified.value, "secret".hashCode)
  }

  test("equalityComparesWrapperIdentity is false for two distinct wrappers") {
    assertEquals(examples.equalityComparesWrapperIdentity, false)
  }

  // ----------------------------------------------------------------
  // Section 5. Declassification
  //
  // Policy authoring is outside safe mode by design (see negative
  // compile tests group F). RunSuite is non-safe mode, so it can
  // call `Policy.apply` directly. The minted policies could equally
  // be passed into the safe-mode `Examples` class to demonstrate the
  // apply-in-safe path; here we exercise the runtime semantics
  // directly.
  // ----------------------------------------------------------------

  test("declassify applies the policy's transformation") {
    // Floor is RegionLvl (not Level) so the implicit ReadCap[Lfloor]
    // resolution is unambiguous: only the suite's ReadCap[ChildLvl]
    // covers RegionLvl; ReadCap[JoinL3] sits in the diamond branch.
    val hashPolicy: Policy[RegionLvl, ChildLvl, String, Int] =
      Policy { (s: String) => s.hashCode }
    val secret   = Classified[ChildLvl, String]("hunter2")
    val released = secret.declassify(hashPolicy)
    assertEquals(released.value, "hunter2".hashCode)
  }

  test("declassify supports partial release (Child → Region, not all the way to Level)") {
    // Source: ChildLvl. Floor: RegionLvl (still classified, just
    // less so than Child). The result is Classified[RegionLvl, _]
    // and remains gated by ReadCap[RegionLvl].
    val maskPolicy: Policy[RegionLvl, ChildLvl, String, String] =
      Policy { (s: String) => s.take(1) + "***" }
    val secret   = Classified[ChildLvl, String]("alice@example.com")
    val released: Classified[RegionLvl, String] = secret.declassify(maskPolicy)
    assertEquals(released.value, "a***")
  }

  test("a release widens up the lattice via Classified's contravariance") {
    // Policy is invariant in Lfloor (the policy is pinned to the
    // floor it was authored at), but the *result* of declassify is a
    // Classified[Lfloor, U] which widens up by `Classified[-L]`. So a
    // release authored at the more-public RegionLvl can still flow
    // into more-classified slots like ChildLvl after the policy fires.
    val toRegion: Policy[RegionLvl, ChildLvl, Int, Int] =
      Policy { (n: Int) => n + 1 }
    val secret   = Classified[ChildLvl, Int](41)
    val released: Classified[RegionLvl, Int] = secret.declassify(toRegion)
    val asChild:  Classified[ChildLvl,  Int] = released
    assertEquals(asChild.value, 42)
  }

