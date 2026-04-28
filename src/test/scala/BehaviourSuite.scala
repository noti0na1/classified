import language.experimental.captureChecking
import language.experimental.modularity

import classified.{*, given}

// Runtime behaviour observable on a `Classified` value: `toString`
// masking, `hashCode` constancy, and `Classified.apply` trapping
// NonFatal exceptions. The wrapper itself is opaque, so these
// observations do not leak the wrapped contents.
//
// `Classified.apply` is gated on `using EnterCap[L]` (the integrity
// gate). To construct any Classified at all the suite has to provide a
// fake `Context[Level]`, from which the library's givens derive
// `EnterCap[L]` for every L (by contravariance) and `ReadCap[Level]`.
// Per-level `Context[L]` fakes are added where `.value` extraction
// needs them.
class RuntimeSuite extends munit.FunSuite:

  trait L1 extends Level
  trait L2 extends L1

  private given Context[Level] = null.asInstanceOf[Context[Level]]

  test("Classified.toString masks the wrapped value") {
    val c = Classified[L1, Int](0)
    assertEquals(c.toString, "Classified(***)")
  }

  test("Classified.toString does not leak the value type either") {
    val cs = Classified[L1, String]("a long secret string")
    assertEquals(cs.toString, "Classified(***)")
  }

  test("Classified.hashCode is the constant 0") {
    val c1 = Classified[L1, Int](1)
    val c2 = Classified[L1, String]("anything")
    val c3 = Classified[L1, List[Int]](List(1, 2, 3))
    assertEquals(c1.hashCode, 0)
    assertEquals(c2.hashCode, 0)
    assertEquals(c3.hashCode, 0)
  }

  test("Classified.apply traps NonFatal exceptions raised by the body") {
    val c = Classified[L1, Int](throw new RuntimeException("boom"))
    // The wrapper is constructed normally even though evaluating the
    // body would have thrown; the throw is captured as `Left(e)` and
    // would only re-surface inside a context that holds a ReadCap.
    assertEquals(c.toString, "Classified(***)")
  }

  test("Classified with captured exception and Classified with value are indistinguishable via toString/hashCode") {
    val success = Classified[L1, Int](42)
    val failure = Classified[L1, Int](throw new RuntimeException("secret"))
    // Both report the same opaque toString — the exception is not observable.
    assertEquals(success.toString, failure.toString)
    assertEquals(success.toString, "Classified(***)")
    // Both return constant hashCode 0 — no hash-based probing.
    assertEquals(success.hashCode, failure.hashCode)
    assertEquals(failure.hashCode, 0)
  }

  test("implicit raise rethrows a NonFatal exception captured in the source classified") {
    // The contravariant `Classified[-L, T]` widens a `Classified[L1, T]`
    // to `Classified[L2, T]` because `L2 <: L1`. The trapped exception
    // travels with the wrapper; reading at the stricter level rethrows.
    given Context[L2] = null.asInstanceOf[Context[L2]]
    val src: Classified[L2, Int] = Classified[L1, Int](throw new RuntimeException("boom"))
    val e = intercept[RuntimeException] { src.value }
    assertEquals(e.getMessage, "boom")
  }

  test("implicit raise preserves the value when the source classified holds a Right") {
    given Context[L2] = null.asInstanceOf[Context[L2]]
    val raised: Classified[L2, Int] = Classified[L1, Int](9 * 9)
    assertEquals(raised.value, 81)
  }

  test("flatten preserves the inner value of a same-level nested Classified") {
    given Context[L1] = null.asInstanceOf[Context[L1]]
    val nested = Classified[L1, Classified[L1, String]](Classified[L1, String]("ok"))
    assertEquals(nested.flatten.value, "ok")
  }

  test("ClassifiedRef returns the initial value via .value") {
    given Context[L1] = null.asInstanceOf[Context[L1]]
    val ref = ClassifiedRef[L1, Int](7)
    assertEquals(ref.value, 7)
  }

  test("ClassifiedRef.update mutates the stored value") {
    given Context[L1] = null.asInstanceOf[Context[L1]]
    val ref = ClassifiedRef[L1, Int](0)
    ref.update(42)
    assertEquals(ref.value, 42)
  }

  test("ClassifiedRef supports successive updates and reads the latest value") {
    given Context[L1] = null.asInstanceOf[Context[L1]]
    val ref = ClassifiedRef[L1, String]("first")
    ref.update("second")
    ref.update("third")
    assertEquals(ref.value, "third")
  }

  test("ClassifiedRef holds reference types like List unchanged") {
    given Context[L1] = null.asInstanceOf[Context[L1]]
    val ref = ClassifiedRef[L1, List[Int]](List(1, 2, 3))
    assertEquals(ref.value, List(1, 2, 3))
    ref.update(Nil)
    assertEquals(ref.value, Nil)
  }

  test("two ClassifiedRefs at the same level are independent") {
    given Context[L1] = null.asInstanceOf[Context[L1]]
    val a = ClassifiedRef[L1, Int](1)
    val b = ClassifiedRef[L1, Int](2)
    a.update(100)
    assertEquals(a.value, 100)
    assertEquals(b.value, 2)
  }
