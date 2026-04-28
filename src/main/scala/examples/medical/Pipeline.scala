package classified
package examples
package medical

import language.experimental.captureChecking
import language.experimental.modularity
import language.experimental.safe
import caps.*

// ========================================================================
//  Medical / PHI example — research pipeline (safe mode).
//
//  This file IS in safe mode (`language.experimental.safe`). The
//  pipeline takes only a public `Context[Level]`, so it can mint and
//  read at the public floor — but it has no `Context[Phi]` and no
//  `ReadCap[Phi]`. The only way the pipeline can extract information
//  about PHI-classified records is by applying one of the vetted
//  `Policies.*` values defined in `Definitions.scala`.
//
//  Safe mode also rejects `Policy.apply` calls, so this file CANNOT
//  mint policies of its own — the trusted authority is locked off.
// ========================================================================

class ResearchPipeline(using Context[Level]):

  // -------- direct declassification ------------------------------------

  // Cohort size — release a single Int from a List[PatientRecord].
  def cohortSize(records: Classified[Phi, List[PatientRecord]])
    : Classified[Level, Int] =
    records.declassify(Policies.cohortSize)

  // Diagnosis frequency table at the public Level.
  def diagnosisFrequency(records: Classified[Phi, List[PatientRecord]])
    : Classified[Level, Map[String, Int]] =
    records.declassify(Policies.diagnosisFrequency)

  // Age-bucket histogram.
  def ageHistogram(records: Classified[Phi, List[PatientRecord]])
    : Classified[Level, Map[AgeBucket, Int]] =
    records.declassify(Policies.ageBucketHistogram)

  // -------- parameterized policy ---------------------------------------

  // Region-filtered count: declassify directly through a
  // region-bound policy minted by the trusted module. The region is
  // a public identifier (not PHI), and the policy pins both the
  // input shape (`List[PatientRecord]`) and the operation (count by
  // region) — no `Policy(identity)` sneaks in.
  def countInRegion(region: String)
                   (records: Classified[Phi, List[PatientRecord]])
    : Classified[Level, Int] =
    records.declassify(Policies.cohortCountInRegion(region))

  // -------- multi-step composition -------------------------------------

  // Composite: returns the diagnosis frequency only when the cohort
  // is k-anonymous; otherwise returns the empty map. Both decisions
  // are made on declassified values at Level — never on raw PHI.
  def safeDiagnosisFrequency(k: Int)
                            (records: Classified[Phi, List[PatientRecord]])
    : Classified[Level, Map[String, Int]] =
    val anon  = records.declassify(Policies.kAnonymousAtLeast(k))
    val freq  = records.declassify(Policies.diagnosisFrequency)
    Classified[Level, Map[String, Int]]:
      if anon then freq else Map.empty
