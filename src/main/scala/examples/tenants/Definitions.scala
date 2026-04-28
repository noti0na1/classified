package classified
package examples
package tenants

import language.experimental.captureChecking
import caps.*

// ========================================================================
//  Multi-tenant analytics example — definitions (trusted, outside safe
//  mode).
//
//  Two tenant levels (`Acme`, `Globex`) extend `Level` directly — they
//  are siblings in the lattice with no subtype relationship. By
//  `Classified[-L]` and `ReadCap[+L]` variance, code holding only a
//  cap for one tenant cannot read or mint anything at the other; the
//  level system enforces tenant isolation at the type level.
//
//  The only cross-tenant data the platform ever publishes is via the
//  per-tenant aggregate policies below. Each policy operates within a
//  single tenant and produces a de-identified value (count, mean,
//  histogram) that is safe to combine at the public Level.
// ========================================================================

@assumeSafe trait Acme   extends Level
@assumeSafe trait Globex extends Level

final case class UsageEvent(
    userId:     String,
    feature:    String,
    timestamp:  Long,
    durationMs: Int
)

@assumeSafe
object Policies:

  // -------- per-tenant: distinct active users --------------------------

  val acmeDistinctUsers: Policy[Level, Acme, List[UsageEvent], Int] =
    Policy { events => events.map(_.userId).distinct.size }

  val globexDistinctUsers: Policy[Level, Globex, List[UsageEvent], Int] =
    Policy { events => events.map(_.userId).distinct.size }

  // -------- per-tenant: feature usage histogram ------------------------
  // Releases only feature-name -> count pairs. UserIds and timestamps
  // never leave the tenant level.

  val acmeFeatureHistogram: Policy[Level, Acme, List[UsageEvent], Map[String, Int]] =
    Policy { events =>
      events.groupBy(_.feature).view.mapValues(_.size).toMap
    }

  val globexFeatureHistogram: Policy[Level, Globex, List[UsageEvent], Map[String, Int]] =
    Policy { events =>
      events.groupBy(_.feature).view.mapValues(_.size).toMap
    }

  // -------- per-tenant: mean session duration --------------------------

  val acmeMeanDurationMs: Policy[Level, Acme, List[UsageEvent], Double] =
    Policy { events =>
      if events.isEmpty then 0.0
      else events.map(_.durationMs.toDouble).sum / events.size
    }

  val globexMeanDurationMs: Policy[Level, Globex, List[UsageEvent], Double] =
    Policy { events =>
      if events.isEmpty then 0.0
      else events.map(_.durationMs.toDouble).sum / events.size
    }

  // -------- threshold-bound aggregations -------------------------------
  // Each policy pins the input shape (`List[UsageEvent]`) and a
  // specific operation (count of long sessions, mean among matching
  // sessions). We deliberately do NOT expose a generic
  // `Policy[Level, _, Int, Int] = Policy(identity)` lifter — that
  // would let ANY tenant-classified Int escape, including one
  // derived from raw user-id work via `.map`. Pinning the input to
  // the raw event list keeps the released numbers shape-bound to a
  // vetted aggregation.

  def acmeLongSessionCount(thresholdMs: Int): Policy[Level, Acme, List[UsageEvent], Int] =
    Policy { events => events.count(_.durationMs >= thresholdMs) }

  def globexLongSessionCount(thresholdMs: Int): Policy[Level, Globex, List[UsageEvent], Int] =
    Policy { events => events.count(_.durationMs >= thresholdMs) }

  def acmeMeanDurationOfFeature(feature: String): Policy[Level, Acme, List[UsageEvent], Double] =
    Policy { events =>
      val matched = events.filter(_.feature == feature)
      if matched.isEmpty then 0.0
      else matched.map(_.durationMs.toDouble).sum / matched.size
    }

  def globexMeanDurationOfFeature(feature: String): Policy[Level, Globex, List[UsageEvent], Double] =
    Policy { events =>
      val matched = events.filter(_.feature == feature)
      if matched.isEmpty then 0.0
      else matched.map(_.durationMs.toDouble).sum / matched.size
    }
