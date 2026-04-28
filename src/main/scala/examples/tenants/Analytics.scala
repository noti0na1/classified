package classified
package examples
package tenants

import language.experimental.captureChecking
import language.experimental.modularity
import language.experimental.safe
import caps.*

// ========================================================================
//  Multi-tenant analytics example — cross-tenant pipeline (safe mode).
//
//  This pipeline holds only `Context[Level]` (the public root). It
//  has neither `Context[Acme]` nor `Context[Globex]`, and `Acme` /
//  `Globex` are unrelated siblings, so even by `ReadCap[+L]`
//  covariance there is no path to read either tenant's raw events
//  directly. The only way to obtain published data is via the vetted
//  per-tenant policies in `Definitions.scala`.
//
//  The structural property that matters for tenant isolation: any
//  attempt to write a method that takes only `Classified[Acme, _]`
//  and produces a `Classified[Globex, _]` (or vice versa) is rejected
//  by the type system — there is no policy or library combinator
//  that bridges sibling levels.
// ========================================================================

class CrossTenantAnalytics(using Context[Level]):

  // -------- per-tenant publication -------------------------------------

  def acmeUsers(events: Classified[Acme, List[UsageEvent]])
    : Classified[Level, Int] =
    events.declassify(Policies.acmeDistinctUsers)

  def globexUsers(events: Classified[Globex, List[UsageEvent]])
    : Classified[Level, Int] =
    events.declassify(Policies.globexDistinctUsers)

  def acmeFeatures(events: Classified[Acme, List[UsageEvent]])
    : Classified[Level, Map[String, Int]] =
    events.declassify(Policies.acmeFeatureHistogram)

  def globexFeatures(events: Classified[Globex, List[UsageEvent]])
    : Classified[Level, Map[String, Int]] =
    events.declassify(Policies.globexFeatureHistogram)

  // -------- cross-tenant comparison at the public Level ----------------

  // Combine declassified values from both tenants into a single
  // public-facing dashboard. Each side is published independently
  // through its own tenant-bound policy; the join happens after both
  // sides have already crossed to Level, so no cross-tenant flow is
  // possible at the raw-event level.
  def usersByTenant(
      acmeEvents:   Classified[Acme,   List[UsageEvent]],
      globexEvents: Classified[Globex, List[UsageEvent]]
    ): Classified[Level, Map[String, Int]] =
    val acmeCount   = acmeUsers(acmeEvents)
    val globexCount = globexUsers(globexEvents)
    Classified[Level, Map[String, Int]]:
      Map(
        "acme"   -> (acmeCount:   Int),
        "globex" -> (globexCount: Int)
      )

  // -------- parameterized policy applications --------------------------

  // Long-session count for Acme: declassify through a
  // threshold-bound policy. The threshold is a public parameter; the
  // policy pins both the input shape (raw event list) and the
  // operation (count over a duration predicate), so what's released
  // is genuinely a "long-session count," not whatever Int the
  // caller could synthesize via `.map`.
  def acmeLongSessionCount(thresholdMs: Int)
                          (events: Classified[Acme, List[UsageEvent]])
    : Classified[Level, Int] =
    events.declassify(Policies.acmeLongSessionCount(thresholdMs))

  // Per-feature mean duration for Acme.
  def acmeFeatureMeanMs(feature: String)
                       (events: Classified[Acme, List[UsageEvent]])
    : Classified[Level, Double] =
    events.declassify(Policies.acmeMeanDurationOfFeature(feature))

  // Mean duration for Globex, conditional on a minimum cohort size at
  // Acme (a typical "publish only if we also have enough at the
  // comparison tenant" pattern). The decision is made on declassified
  // counts at the public Level; raw events never cross tenants.
  def globexMeanIfAcmeAbove(minAcmeUsers: Int)(
      acmeEvents:   Classified[Acme,   List[UsageEvent]],
      globexEvents: Classified[Globex, List[UsageEvent]]
    ): Classified[Level, Option[Double]] =
    val acmeN  = acmeUsers(acmeEvents)
    val gMean  = globexEvents.declassify(Policies.globexMeanDurationMs)
    Classified[Level, Option[Double]]:
      if (acmeN: Int) >= minAcmeUsers then Some(gMean: Double) else None
