// Compile-time negative cases: every snippet here is expected to be
// rejected by the full Scala 3 pipeline (typer + cc + safe mode).
// `CompileHelper.compile` invokes `dotty.tools.dotc.Main` directly so
// every phase runs, including capture checking which macro-based test
// helpers do not reach.
class NegativeCompileSuite extends munit.FunSuite:

  // Header that every snippet starts with: Level lattice and a
  // privileged `Context[Level]` taken as a using parameter at the
  // entry of the attack function. The level traits live at top level
  // so each snippet can reference them.
  private val header =
    """|trait LvlA  extends Level
       |trait LvlB  extends Level
       |trait Outer extends Level
       |trait Inner extends Outer
       |""".stripMargin

  // ================================================================
  // Group A. Type system: ReadCap / EnterCap / Context scoping
  // ================================================================

  test("[type] calling .value without a ReadCap is rejected") {
    val r = CompileHelper.compile(header +
      """|def attack(using EnterCap[Level]): Int =
         |  val c = Classified[LvlA, Int](7)
         |  c.value
         |""".stripMargin)
    assert(!r.succeeded, "expected a compile error, got none")
  }

  test("[type] auto conversion outside a ReadCap scope is rejected") {
    val r = CompileHelper.compile(header +
      """|def attack(using EnterCap[Level]): Int =
         |  val c = Classified[LvlA, Int](7)
         |  val n: Int = c
         |  n
         |""".stripMargin)
    assert(!r.succeeded, "expected a compile error, got none")
  }

  test("[type] Classified.apply without a satisfying EnterCap is rejected") {
    // No EnterCap or Context in scope at all, so the integrity gate on
    // Classified.apply has nothing to summon.
    val r = CompileHelper.compile(header +
      """|def attack: Unit =
         |  Classified[LvlA, Int](42)
         |  ()
         |""".stripMargin)
    assert(!r.succeeded, "expected a compile error, got none")
  }

  // ----------------------------------------------------------------
  // Integrity gate on `Classified.apply`.
  //
  // `Classified.apply` requires `using EnterCap[L]`. By contravariance
  // of `EnterCap[-L]`, holding an ancestor's enter cap is enough; but
  // an unrelated level (sibling) or no cap at all is rejected.
  // ----------------------------------------------------------------

  test("[integrity] forging a Classified[_, String] without an EnterCap is rejected (auth bypass scenario)") {
    // Scenario: a trusted authentication module exposes a verifier
    // that consumes a `Classified[LvlA, String]` (the candidate
    // password). The module trusts the wrapper to have been minted by
    // privileged code that already vetted the input. Without an
    // EnterCap satisfying `EnterCap[LvlA]`, an unprivileged attacker
    // cannot mint a wrapper around a chosen string.
    val r = CompileHelper.compile(header +
      """|def verify(candidate: Classified[LvlA, String])
         |          (using EnterCap[Level])
         |          : Classified[LvlA, Boolean] =
         |  candidate.map { (p: String) => (_: Context[LvlA]) ?=> p.length > 8 }
         |
         |// The attacker has no cap of any level. They try to mint a
         |// wrapper around an attacker-chosen string so they can hand
         |// it to `verify`.
         |def forge: Classified[LvlA, String] =
         |  Classified[LvlA, String]("forged-password")
         |""".stripMargin)
    assert(!r.succeeded, "forging a Classified[_, String] without an EnterCap must be rejected")
  }

  test("[integrity] holding only a sibling level's cap cannot mint into another sibling") {
    // Scenario: a multi-tenant system gives each tenant its own level.
    // Tenant A's code, which holds EnterCap[LvlA], tries to inject a
    // forged Classified into tenant B's level. LvlA and LvlB both
    // extend Level but neither is a subtype of the other, so by
    // contravariance EnterCap[LvlA] does not satisfy EnterCap[LvlB].
    val r = CompileHelper.compile(header +
      """|def attackTenantB(using EnterCap[LvlA]): Classified[LvlB, String] =
         |  Classified[LvlB, String]("alice tries to mint into bob's tenant")
         |""".stripMargin)
    assert(!r.succeeded, "minting into a sibling without that sibling's EnterCap must be rejected")
  }

  test("[type] writing to a sibling level's ClassifiedRef is rejected") {
    // `ClassifiedRef.update` requires `Context[L]`, which is invariant.
    // Inside `secret.map`'s body we hold `Context[LvlA]`, which is
    // unrelated to `Context[LvlB]`, so `refB.update` does not type
    // check.
    val r = CompileHelper.compile(header +
      """|def attack(using Context[Level]): Unit =
         |  val refB = ClassifiedRef[LvlB, Int](0)
         |  val secret = Classified[LvlA, Int](7)
         |  secret.map { (n: Int) => (_: Context[LvlA]) ?=> refB.update(n) }
         |  ()
         |""".stripMargin)
    assert(!r.succeeded, "expected a compile error, got none")
  }

  // ----------------------------------------------------------------
  // Subtype direction
  // ----------------------------------------------------------------

  test("[type] widening a Classified into a sibling level is rejected") {
    // `Classified[-L, T]` is contravariant in L, so widening only
    // works along the subtype chain. `LvlA` and `LvlB` are unrelated,
    // so a `Classified[LvlA, Int]` cannot be ascribed as
    // `Classified[LvlB, Int]`.
    val r = CompileHelper.compile(header +
      """|def attack(using Context[Level]): Unit =
         |  val c = Classified[LvlA, Int](7)
         |  val widened: Classified[LvlB, Int] = c
         |  ()
         |""".stripMargin)
    assert(!r.succeeded, "widening to a sibling level must be rejected")
  }

  test("[type] widening to a stricter level loses the looser ReadCap") {
    // After widening `Classified[Outer, Int]` to `Classified[Inner, Int]`,
    // reading requires `ReadCap[Inner]`. A `ReadCap[Outer]` is not a
    // subtype of `ReadCap[Inner]` (covariance flips the wrong way),
    // so the value remains unreachable at the looser level.
    val r = CompileHelper.compile(header +
      """|def attack(using ReadCap[Outer], EnterCap[Outer]): Int =
         |  val c: Classified[Inner, Int] = Classified[Outer, Int](7)
         |  c.value
         |""".stripMargin)
    assert(!r.succeeded, "reading a stricter-classified wrapper without the stricter cap must be rejected")
  }

  test("[type] flatten across two distinct levels is rejected") {
    // The flatten extension is `Classified[L, Classified[L, T]]` —
    // both layers must be at the *same* L. A nested
    // `Classified[Outer, Classified[Inner, T]]` does not match the
    // shape, so the call does not type check.
    val r = CompileHelper.compile(header +
      """|def attack(using Context[Level]): Classified[Outer, String] =
         |  val nested: Classified[Outer, Classified[Inner, String]] =
         |    Classified[Outer, Classified[Inner, String]](Classified[Inner, String]("x"))
         |  nested.flatten
         |""".stripMargin)
    assert(!r.succeeded, "cross-level flatten must be rejected")
  }

  // ================================================================
  // Group B. Capture checker: the {any.rd} bound on the body
  // ================================================================

  test("[capture] writing to a captured outer var inside map is rejected") {
    val r = CompileHelper.compile(header +
      """|def attack(using Context[Level]): Unit =
         |  val c = Classified[LvlA, Int](7)
         |  var leak = 0
         |  c.map { (n: Int) => (_: Context[LvlA]) ?=> leak = n }
         |  ()
         |""".stripMargin)
    assert(!r.succeeded, "expected a compile error, got none")
    assert(r.errorsContain("any.rd"), s"expected an `any.rd` capture diagnostic; got:\n${r.output}")
  }

  test("[capture] capturing a Mutable holder into a map closure is rejected") {
    val r = CompileHelper.compile(header +
      """|import caps.*
         |
         |class Sink extends Mutable:
         |  var s: String = ""
         |  update def write(v: String): Unit = s = v
         |
         |def attack(using Context[Level]): String =
         |  val c = Classified[LvlA, String]("secret")
         |  val sink = Sink()
         |  c.map { (s: String) => (_: Context[LvlA]) ?=> sink.write(s) }
         |  sink.s
         |""".stripMargin)
    assert(!r.succeeded, "expected a compile error, got none")
  }

  test("[capture] a Mutable handed in via Classified.map is read only inside f") {
    val r = CompileHelper.compile(header +
      """|import caps.*
         |
         |class MutHolder extends Mutable:
         |  var s: String = ""
         |  update def write(v: String): Unit = s = v
         |
         |def attack(using Context[Level]): String =
         |  val m = MutHolder()
         |  val cm = Classified[LvlA, MutHolder](m)
         |  val secret = Classified[LvlA, String]("secret")
         |  secret.map { (x: String) => (_: Context[LvlA]) ?=>
         |    cm.map { (mh: MutHolder) => (_: Context[LvlA]) ?=> mh.write(x) }
         |    ()
         |  }
         |  m.s
         |""".stripMargin)
    assert(!r.succeeded, "expected a compile error, got none")
    assert(r.errorsContain("read-only"), s"expected a read-only diagnostic; got:\n${r.output}")
  }

  test("[capture] capturing a Stateful holder into a map closure is rejected") {
    val r = CompileHelper.compile(header +
      """|import caps.*
         |
         |class Holder extends Stateful:
         |  var s: String = ""
         |
         |def attack(using Context[Level]): String =
         |  val c = Classified[LvlA, String]("secret")
         |  val h = Holder()
         |  c.map { (s: String) => (_: Context[LvlA]) ?=> h.s = s }
         |  h.s
         |""".stripMargin)
    assert(!r.succeeded, "expected a compile error, got none")
  }

  test("[capture] a function value capturing a write capability cannot be wrapped in Classified") {
    val r = CompileHelper.compile(header +
      """|import caps.*
         |
         |class MutHolder extends Mutable:
         |  var s: String = ""
         |  update def write(v: String): Unit = s = v
         |
         |def attack(using Context[Level]): String =
         |  val m = MutHolder()
         |  val effect: String -> Unit = (v: String) => m.write(v)
         |  val ceffect = Classified[LvlA, String -> Unit](effect)
         |  ""
         |""".stripMargin)
    assert(!r.succeeded, "expected a compile error, got none")
  }

  test("[capture] returning a side-effecting closure from map is rejected") {
    val r = CompileHelper.compile(header +
      """|import caps.*
         |
         |class MutHolder extends Mutable:
         |  var s: String = ""
         |  update def write(v: String): Unit = s = v
         |
         |def attack(using Context[Level]): String =
         |  val m = MutHolder()
         |  val secret = Classified[LvlA, String]("secret")
         |  val capsule: Classified[LvlA, () -> Unit] = secret.map { (x: String) =>
         |    (_: Context[LvlA]) ?=> () => m.write(x)
         |  }
         |  m.s
         |""".stripMargin)
    assert(!r.succeeded, "expected a compile error, got none")
  }

  test("[capture] capturing the Context parameter into an outer mutable is rejected") {
    // The closure body wants to squirrel the supplied `Context[LvlA]`
    // away into an outer `var stash` so it can be reused later. The
    // `{any.rd}` capture rule that catches plain outer-var writes
    // catches the ctx-capture variant too.
    val r = CompileHelper.compile(header +
      """|def attack(using Context[Level]): Unit =
         |  var stash: (Context[LvlA] | Null) = null
         |  Classified[LvlA, Int] {
         |    stash = summon[Context[LvlA]]
         |    0
         |  }
         |  ()
         |""".stripMargin)
    assert(!r.succeeded, "expected a compile error, got none")
    assert(r.errorsContain("any.rd"), s"expected an `any.rd` capture diagnostic; got:\n${r.output}")
  }

  test("[capture] Classified.apply body capturing a write capability is rejected") {
    val r = CompileHelper.compile(header +
      """|def attack(using Context[Level]): Unit =
         |  var leak = 0
         |  Classified[LvlA, Int] { leak = 7; 0 }
         |  ()
         |""".stripMargin)
    assert(!r.succeeded, "expected a compile error, got none")
    assert(r.errorsContain("any.rd"), s"expected an `any.rd` capture diagnostic; got:\n${r.output}")
  }

  test("[capture] inner.map cannot host an outer.map call") {
    // The result of outer.map is `Classified[Outer, U]^{ctx}`, where
    // `ctx` is whichever EnterCap satisfied the call. Inside
    // inner.map's body the body's bound is `{any.rd}`, so the picked
    // ctx flows out of bounds via the result's capture.
    val r = CompileHelper.compile(header +
      """|def attack(using Context[Level]): Unit =
         |  val co = Classified[Outer, String]("o")
         |  val ci = Classified[Inner, String]("i")
         |  ci.map { (y: String) => (_: Context[Inner]) ?=>
         |    co.map { (x: String) => (_: Context[Outer]) ?=> x + y }
         |  }
         |  ()
         |""".stripMargin)
    assert(!r.succeeded, "expected a compile error, got none")
    assert(r.errorsContain("any.rd"), s"expected an `any.rd` diagnostic; got:\n${r.output}")
  }

  // ================================================================
  // Group C. Safe mode: asInstanceOf, @rejectSafe, untagged Java APIs
  // ================================================================

  test("[safe] direct cast `c.asInstanceOf[T]` is rejected") {
    val r = CompileHelper.compile(header +
      """|def attack(using Context[Level]): Int =
         |  val c = Classified[LvlA, Int](7)
         |  c.asInstanceOf[Int]
         |""".stripMargin)
    assert(!r.succeeded, "expected a compile error, got none")
    assert(r.errorsContain("asInstanceOf"), s"expected an asInstanceOf diagnostic; got:\n${r.output}")
  }

  test("[safe] forging a Context via null.asInstanceOf is rejected") {
    val r = CompileHelper.compile(header +
      """|def attack(using EnterCap[Level]): Int =
         |  given Context[LvlA] = null.asInstanceOf[Context[LvlA]]
         |  val c = Classified[LvlA, Int](7)
         |  c.value
         |""".stripMargin)
    assert(!r.succeeded, "expected a compile error, got none")
    assert(r.errorsContain("asInstanceOf"), s"expected an asInstanceOf diagnostic; got:\n${r.output}")
  }

  test("[safe] subclass + asInstanceOf bypass attempt is rejected") {
    // In the new design `Classified` has a `private` constructor, so
    // the subclass attempt is rejected at the typer (the inherited
    // constructor is inaccessible) before `asInstanceOf` ever fires.
    // Either diagnostic is fine: both show the bypass route is closed.
    val r = CompileHelper.compile(header +
      """|def attack(using Context[Level]): String =
         |  class Backdoor[T](v: T) extends Classified[LvlA, T]:
         |    def leak: T = v
         |  val real: Classified[LvlA, String] = Classified[LvlA, String]("secret")
         |  val cast: Backdoor[String] = real.asInstanceOf[Backdoor[String]]
         |  cast.leak
         |""".stripMargin)
    assert(!r.succeeded, "expected a compile error, got none")
    assert(
      r.errorsContain("asInstanceOf") || r.errorsContain("cannot be accessed"),
      s"expected an asInstanceOf or constructor-privacy diagnostic; got:\n${r.output}"
    )
  }

  test("[safe] println from inside map is rejected") {
    val r = CompileHelper.compile(header +
      """|def attack(using Context[Level]): Unit =
         |  val c = Classified[LvlA, String]("secret")
         |  c.map { (s: String) => (_: Context[LvlA]) ?=> println(s) }
         |  ()
         |""".stripMargin)
    assert(!r.succeeded, "expected a compile error, got none")
    assert(r.errorsContain("println"), s"expected a println diagnostic; got:\n${r.output}")
  }

  test("[safe] println from inside a Classified.apply body is rejected") {
    val r = CompileHelper.compile(header +
      """|def attack(using Context[Level]): Unit =
         |  Classified[LvlA, Int] { println("hello"); 0 }
         |  ()
         |""".stripMargin)
    assert(!r.succeeded, "expected a compile error, got none")
    assert(r.errorsContain("println"), s"expected a println diagnostic; got:\n${r.output}")
  }

  test("[safe] System.out.println from inside map is rejected") {
    val r = CompileHelper.compile(header +
      """|def attack(using Context[Level]): Unit =
         |  val c = Classified[LvlA, String]("secret")
         |  c.map { (s: String) => (_: Context[LvlA]) ?=> System.out.println(s) }
         |  ()
         |""".stripMargin)
    assert(!r.succeeded, "expected a compile error, got none")
  }

  test("[safe] sys.error using the secret is rejected") {
    val r = CompileHelper.compile(header +
      """|def attack(using Context[Level]): Unit =
         |  val c = Classified[LvlA, String]("secret")
         |  c.map { (s: String) => (_: Context[LvlA]) ?=> sys.error(s) }
         |  ()
         |""".stripMargin)
    assert(!r.succeeded, "expected a compile error, got none")
  }

  test("[safe] throwing a RuntimeException carrying the secret is rejected") {
    val r = CompileHelper.compile(header +
      """|def attack(using Context[Level]): String =
         |  val c = Classified[LvlA, String]("secret")
         |  try
         |    c.map { (s: String) => (_: Context[LvlA]) ?=> throw new RuntimeException(s) }
         |    "unreached"
         |  catch
         |    case e: RuntimeException => e.getMessage
         |""".stripMargin)
    assert(!r.succeeded, "expected a compile error, got none")
  }

  test("[safe] throwing a ControlThrowable carrying the secret is rejected") {
    val r = CompileHelper.compile(header +
      """|class SafeLeak(val payload: String) extends scala.util.control.ControlThrowable
         |
         |def attack(using Context[Level]): Unit =
         |  val c = Classified[LvlA, String]("secret")
         |  c.map { (s: String) => (_: Context[LvlA]) ?=> throw SafeLeak(s) }
         |  ()
         |""".stripMargin)
    assert(!r.succeeded, "expected a compile error, got none")
  }

  test("[safe] launching a Thread is rejected") {
    val r = CompileHelper.compile(header +
      """|def attack(using Context[Level]): Unit =
         |  val c = Classified[LvlA, String]("secret")
         |  c.map { (s: String) => (_: Context[LvlA]) ?=> Thread { () => () }.start() }
         |  ()
         |""".stripMargin)
    assert(!r.succeeded, "expected a compile error, got none")
  }

  test("[safe] referencing a mutable StringBuilder is rejected") {
    val r = CompileHelper.compile(header +
      """|def attack(using Context[Level]): Unit =
         |  val c = Classified[LvlA, String]("secret")
         |  val sb = StringBuilder()
         |  c.map { (s: String) => (_: Context[LvlA]) ?=> sb.append(s); () }
         |  ()
         |""".stripMargin)
    assert(!r.succeeded, "expected a compile error, got none")
  }

  test("[safe] a top-level `var` is not allowed without Mutable / Stateful") {
    val r = CompileHelper.compile(
      """|object Sink:
         |  var last: String = ""
         |""".stripMargin)
    assert(!r.succeeded, "expected a compile error, got none")
    assert(
      r.errorsContain("Stateful") || r.errorsContain("Mutable") || r.errorsContain("untrackedCaptures"),
      s"expected a Stateful / Mutable / untrackedCaptures diagnostic; got:\n${r.output}"
    )
  }

  // ================================================================
  // Group D. ReadCap / EnterCap cannot leak
  //
  // The capability traits (`ReadCap`, `EnterCap`, `Context`) all have
  // `private` constructors and are only obtainable through the
  // library-supplied givens that derive them from a `Context[L]`. Two
  // routes a hypothetical attacker could try:
  //
  //   1. Forge a cap via `null.asInstanceOf` — closed by safe mode.
  //   2. Smuggle a cap derived inside an apply body out through a
  //      captured outer mutable — closed by the `{any.rd}` capture
  //      bound on the body.
  //
  // The two tests below pin down each route per cap type.
  // ================================================================

  test("[safe] forging a ReadCap via null.asInstanceOf is rejected") {
    val r = CompileHelper.compile(header +
      """|def attack(using EnterCap[Level]): Int =
         |  given ReadCap[LvlA] = null.asInstanceOf[ReadCap[LvlA]]
         |  val c = Classified[LvlA, Int](7)
         |  c.value
         |""".stripMargin)
    assert(!r.succeeded, "expected a compile error, got none")
    assert(r.errorsContain("asInstanceOf"), s"expected an asInstanceOf diagnostic; got:\n${r.output}")
  }

  test("[safe] forging an EnterCap via null.asInstanceOf is rejected") {
    val r = CompileHelper.compile(header +
      """|def attack: Classified[LvlA, Int] =
         |  given EnterCap[LvlA] = null.asInstanceOf[EnterCap[LvlA]]
         |  Classified[LvlA, Int](42)
         |""".stripMargin)
    assert(!r.succeeded, "expected a compile error, got none")
    assert(r.errorsContain("asInstanceOf"), s"expected an asInstanceOf diagnostic; got:\n${r.output}")
  }

  test("[capture] capturing a derived ReadCap into an outer mutable is rejected") {
    // `summon[ReadCap[LvlA]]` inside a `Classified[LvlA, _]` body
    // resolves to a ReadCap derived from the body's own `Context[LvlA]`
    // (capture set `{ctx}`). Storing it in an outer `var` would
    // outlive `ctx`, so the write violates the body's `{any.rd}`
    // bound — the same rule that catches plain outer-var writes.
    val r = CompileHelper.compile(header +
      """|def attack(using Context[Level]): Unit =
         |  var stash: (ReadCap[LvlA] | Null) = null
         |  Classified[LvlA, Int] {
         |    stash = summon[ReadCap[LvlA]]
         |    0
         |  }
         |  ()
         |""".stripMargin)
    assert(!r.succeeded, "expected a compile error, got none")
    assert(r.errorsContain("any.rd"), s"expected an `any.rd` capture diagnostic; got:\n${r.output}")
  }

  test("[capture] capturing a derived EnterCap into an outer mutable is rejected") {
    val r = CompileHelper.compile(header +
      """|def attack(using Context[Level]): Unit =
         |  var stash: (EnterCap[LvlA] | Null) = null
         |  Classified[LvlA, Int] {
         |    stash = summon[EnterCap[LvlA]]
         |    0
         |  }
         |  ()
         |""".stripMargin)
    assert(!r.succeeded, "expected a compile error, got none")
    assert(r.errorsContain("any.rd"), s"expected an `any.rd` capture diagnostic; got:\n${r.output}")
  }

  // ----------------------------------------------------------------
  // Constructor privacy. Each cap trait declares a `private` primary
  // constructor, so direct construction is closed off. Two routes a
  // user could try are an anonymous-class instance
  // (`new ReadCap[L] {}`) and a named subclass (`class MyCap extends
  // ReadCap[L]`); both attempt to invoke the private primary
  // constructor and are rejected at the typer.
  // ----------------------------------------------------------------

  test("[integrity] calling the ReadCap constructor via an anonymous class is rejected") {
    val r = CompileHelper.compile(header +
      """|def attack(using EnterCap[Level]): Int =
         |  given ReadCap[LvlA] = new ReadCap[LvlA] {}
         |  val c = Classified[LvlA, Int](7)
         |  c.value
         |""".stripMargin)
    assert(!r.succeeded, "expected a compile error, got none")
    assert(
      r.errorsContain("private") || r.errorsContain("cannot be accessed"),
      s"expected a private-constructor diagnostic; got:\n${r.output}"
    )
  }

  test("[integrity] calling the EnterCap constructor via an anonymous class is rejected") {
    val r = CompileHelper.compile(header +
      """|def attack: Classified[LvlA, Int] =
         |  given EnterCap[LvlA] = new EnterCap[LvlA] {}
         |  Classified[LvlA, Int](42)
         |""".stripMargin)
    assert(!r.succeeded, "expected a compile error, got none")
    assert(
      r.errorsContain("private") || r.errorsContain("cannot be accessed"),
      s"expected a private-constructor diagnostic; got:\n${r.output}"
    )
  }

  test("[integrity] calling the Context constructor via an anonymous class is rejected") {
    val r = CompileHelper.compile(header +
      """|def attack: Classified[LvlA, Int] =
         |  given Context[LvlA] = new Context[LvlA] {}
         |  Classified[LvlA, Int](42)
         |""".stripMargin)
    assert(!r.succeeded, "expected a compile error, got none")
    assert(
      r.errorsContain("private") || r.errorsContain("cannot be accessed"),
      s"expected a private-constructor diagnostic; got:\n${r.output}"
    )
  }

  test("[integrity] subclassing a cap trait to call its constructor is rejected") {
    // The named-subclass variant of the anonymous-class attack: declare
    // a custom `ReadCap` subtype and instantiate it. Both the subclass
    // declaration and `new MyReadCap[LvlA]` hit the private primary
    // constructor of `ReadCap`.
    val r = CompileHelper.compile(header +
      """|class MyReadCap[L <: Level] extends ReadCap[L]
         |
         |def attack(using EnterCap[Level]): Int =
         |  given ReadCap[LvlA] = new MyReadCap[LvlA]
         |  val c = Classified[LvlA, Int](7)
         |  c.value
         |""".stripMargin)
    assert(!r.succeeded, "expected a compile error, got none")
    assert(
      r.errorsContain("private") || r.errorsContain("cannot be accessed"),
      s"expected a private-constructor diagnostic; got:\n${r.output}"
    )
  }

  // ================================================================
  // Group E. Diamond lattice: L1 and L2 unrelated, L3 extends both
  //
  // With:
  //   trait DL1 extends Level
  //   trait DL2 extends Level
  //   trait DL3 extends DL1, DL2
  //
  // The contravariant `Classified[-L, T]` permits widening only along
  // the subtype chain. So:
  //   Classified[DL1, T] <: Classified[DL3, T]   ✓ (DL3 <: DL1)
  //   Classified[DL2, T] <: Classified[DL3, T]   ✓ (DL3 <: DL2)
  //   Classified[DL3, T] <: Classified[DL1, T]   ✗ (DL1 not <: DL3)
  //   Classified[DL3, T] <: Classified[DL2, T]   ✗ (DL2 not <: DL3)
  //
  // i.e. the diamond can be CLIMBED but not DESCENDED. Data at DL1
  // and DL2 can both be lifted into DL3 (the join), but the result is
  // pinned to DL3 — it cannot be re-projected onto either branch. So
  // DL1 and DL2 still cannot exchange information, even with the
  // common subtype DL3 sitting above them.
  //
  // The positive counterpart (legitimate read at the L3 join) is the
  // `diamondJoin` runtime test in RunSuite.
  // ================================================================

  test("[type] siblings DL1 and DL2 cannot read each other directly") {
    // `c2.value` requires `ReadCap[DL2]`. Holding `Context[DL1]` (and
    // therefore `ReadCap[DL1]`) is no help: ReadCap covariance only
    // gives `ReadCap[DL1] <: ReadCap[Level]`, never `<: ReadCap[DL2]`.
    val r = CompileHelper.compile(
      """|trait DL1 extends Level
         |trait DL2 extends Level
         |
         |def attack(using Context[DL1], EnterCap[Level]): Int =
         |  val c2: Classified[DL2, Int] = Classified[DL2, Int](42)
         |  c2.value
         |""".stripMargin)
    assert(!r.succeeded, "reading a sibling level's wrapper must be rejected")
  }

  test("[type] in a diamond, narrowing Classified[L3] to L2 is rejected (no descent)") {
    // The bridge attack: lift DL1 to DL3 (legal, contravariant), then
    // try to project DL3 onto DL2. Since DL2 is not a subtype of DL3,
    // the second step fails. So data at DL1 cannot reach DL2 via the
    // join.
    val r = CompileHelper.compile(
      """|trait DL1 extends Level
         |trait DL2 extends Level
         |trait DL3 extends DL1, DL2
         |
         |def attack(using Context[Level]): Classified[DL2, Int] =
         |  val c1: Classified[DL1, Int] = Classified[DL1, Int](42)
         |  val c3: Classified[DL3, Int] = c1   // OK: DL3 <: DL1
         |  val c2: Classified[DL2, Int] = c3   // BAD: DL3 not <: DL2 in the contravariant direction
         |  c2
         |""".stripMargin)
    assert(!r.succeeded, "narrowing through a diamond join must be rejected")
  }

  test("[type] in a diamond, narrowing Classified[L3] back to L1 is also rejected (symmetric)") {
    val r = CompileHelper.compile(
      """|trait DL1 extends Level
         |trait DL2 extends Level
         |trait DL3 extends DL1, DL2
         |
         |def attack(using Context[Level]): Classified[DL1, Int] =
         |  val c2: Classified[DL2, Int] = Classified[DL2, Int](42)
         |  val c3: Classified[DL3, Int] = c2   // OK: DL3 <: DL2
         |  val c1: Classified[DL1, Int] = c3   // BAD: DL3 not <: DL1
         |  c1
         |""".stripMargin)
    assert(!r.succeeded, "narrowing back to the other diamond branch must be rejected")
  }

  test("[type] in a diamond, holding Context[L3] still cannot mutate a Ref at L1 (Context invariant)") {
    // ClassifiedRef.update is gated on the *invariant* `Context[L]`,
    // so holding `Context[DL3]` does not authorise a write to a
    // `ClassifiedRef[DL1, _]` even though `DL3 <: DL1`. (Allowing it
    // would be a write-down: data observable at DL3 leaking into DL1
    // storage that less-classified code can read.)
    val r = CompileHelper.compile(
      """|trait DL1 extends Level
         |trait DL2 extends Level
         |trait DL3 extends DL1, DL2
         |
         |def attack(using Context[DL3], EnterCap[Level]): Unit =
         |  val refDL1 = ClassifiedRef[DL1, Int](0)
         |  refDL1.update(7)
         |  ()
         |""".stripMargin)
    assert(!r.succeeded, "writing down to an ancestor's Ref must be rejected")
  }
