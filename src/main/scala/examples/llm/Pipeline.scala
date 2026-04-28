package classified
package examples
package llm

import language.experimental.captureChecking
import language.experimental.modularity
import language.experimental.safe
import caps.*

// ========================================================================
//  LLM example — prompt pipeline (safe mode).
//
//  This file IS in safe mode (`language.experimental.safe`). The
//  pipeline holds only `Context[Level]` — it has no `Context[Secure]`
//  and no `ReadCap[Secure]`. The only way it can extract information
//  from Secure-classified LLM results is by applying one of the vetted
//  `Policies.*` values defined in `Definitions.scala`.
//
//  Safe mode also rejects `Policy.apply` calls, so this file CANNOT
//  mint policies of its own.
// ========================================================================

class PromptPipeline(llm: LLM)(using Context[Level]):

  // -------- direct declassification ------------------------------------

  // Extract the response text from a classified LLM result.
  def extractResponse(result: Classified[Secure, LLMResult])
    : Classified[Level, String] =
    result.declassify(Policies.extractResponse)

  // Check whether the LLM call succeeded.
  def isSuccess(result: Classified[Secure, LLMResult])
    : Classified[Level, Boolean] =
    result.declassify(Policies.isSuccess)

  // Check the status: None = success (safe), Some(reason) = failure.
  def checkStatus(result: Classified[Secure, LLMResult])
    : Classified[Level, Option[String]] =
    result.declassify(Policies.resultStatus)

  // -------- end-to-end: prompt -> declassified response ------------------

  // Send a classified prompt to the LLM and extract the response.
  def complete(prompt: Classified[Secure, String])
    : Classified[Level, String] =
    llm.complete(prompt).declassify(Policies.extractResponse)

  // Send a classified prompt and check whether it succeeded.
  def completeAndCheck(prompt: Classified[Secure, String])
    : Classified[Level, Boolean] =
    llm.complete(prompt).declassify(Policies.isSuccess)

  // -------- parameterized policy ---------------------------------------

  // Preview the first `n` characters of the LLM response.
  def preview(n: Int)(prompt: Classified[Secure, String])
    : Classified[Level, String] =
    llm.complete(prompt).declassify(Policies.summary(n))

  // -------- multi-step composition -------------------------------------

  // Extract the response only if the LLM succeeded; otherwise return
  // the empty string. Both the success check and the extraction are
  // performed through vetted policies — the pipeline never reads raw
  // Secure data.
  def safeExtract(result: Classified[Secure, LLMResult])
    : Classified[Level, String] =
    val ok  = result.declassify(Policies.isSuccess)
    val txt = result.declassify(Policies.extractResponse)
    Classified[Level, String]:
      if ok then txt else ""

  // Complete a classified prompt and return the response length, but
  // only if the result is successful; otherwise return 0.
  def lengthIfSuccessful(prompt: Classified[Secure, String])
    : Classified[Level, Int] =
    val result = llm.complete(prompt)
    val ok     = result.declassify(Policies.isSuccess)
    val len    = result.declassify(Policies.responseLength)
    Classified[Level, Int]:
      if (ok: Boolean) then (len: Int) else 0
