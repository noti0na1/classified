package classified
package examples
package medical

import language.experimental.captureChecking
import caps.*

// ========================================================================
//  Medical / PHI example — definitions (trusted, outside safe mode).
//
//  PHI = Personal Health Information. Records at this level are
//  readable only by code holding `Context[Phi]` (or stronger) — but
//  the research pipeline downstream is intentionally not given that
//  cap. The only way the pipeline can extract anything is through
//  the vetted `Policies.*` values below, each of which is a reviewed
//  assertion that "this aggregate is safe to publish at the public
//  Level."
//
//  This file is not annotated `@language.experimental.safe`, so the
//  policy author can use the (non-safe-mode) `Policy.apply` factory
//  directly. Safe-mode consumer code (see `Pipeline.scala`) cannot.
// ========================================================================

@assumeSafe
trait Phi extends Level

final case class PatientRecord(
    name:        String,
    dob:         String,        // ISO YYYY-MM-DD
    region:      String,
    ageInYears:  Int,
    diagnosis:   String,
    bloodType:   String
)

enum AgeBucket:
  case Pediatric, Adult, Senior

private def toBucket(years: Int): AgeBucket =
  if      years < 18 then AgeBucket.Pediatric
  else if years < 65 then AgeBucket.Adult
  else                    AgeBucket.Senior

@assumeSafe
object Policies:

  // Cohort cardinality. The trusted module attests that publishing a
  // count is safe — only the cohort size leaks, no identifiers.
  val cohortSize: Policy[Level, Phi, List[PatientRecord], Int] =
    Policy { rs => rs.size }

  // K-anonymity gate: a yes/no answer to "does the cohort have at
  // least k members?" Often used as a precondition before publishing
  // smaller aggregates. The threshold `k` is captured by value
  // (Int — no capture-set entry needed).
  def kAnonymousAtLeast(k: Int): Policy[Level, Phi, List[PatientRecord], Boolean] =
    Policy { rs => rs.size >= k }

  // Diagnosis frequency table. Identifiers are stripped; only the
  // diagnosis labels and their counts cross the boundary.
  val diagnosisFrequency: Policy[Level, Phi, List[PatientRecord], Map[String, Int]] =
    Policy { rs =>
      rs.groupBy(_.diagnosis).view.mapValues(_.size).toMap
    }

  // Per-bucket histogram. Releases age buckets and counts; no DOB,
  // no exact ages.
  val ageBucketHistogram: Policy[Level, Phi, List[PatientRecord], Map[AgeBucket, Int]] =
    Policy { rs =>
      rs.map(r => toBucket(r.ageInYears))
        .groupBy(identity)
        .view.mapValues(_.size)
        .toMap
    }

  // Mean age across the cohort, rounded — a single number, safe to
  // release once derived.
  val meanAgeRounded: Policy[Level, Phi, List[PatientRecord], Int] =
    Policy { rs =>
      if rs.isEmpty then 0
      else (rs.map(_.ageInYears.toDouble).sum / rs.size).round.toInt
    }

  // Region-bound cohort count — the region label is a public
  // identifier (not PHI), so the trusted module attests that it is
  // safe to publish a count of records matching it. The region is
  // captured by value into the resulting policy. We expose this as
  // a `def` rather than a generic `Policy[Level, Phi, Int, Int]`
  // identity lifter, because an identity policy on Int would let
  // ANY Phi-classified Int (including, say, an exact age leaked via
  // map) escape — pinning the input to `List[PatientRecord]` and
  // the operation to "count by region" keeps the released Int
  // shape-bound to a vetted aggregation.
  def cohortCountInRegion(region: String): Policy[Level, Phi, List[PatientRecord], Int] =
    Policy { rs => rs.count(_.region == region) }
