package classified
package examples
package llm

import language.experimental.captureChecking
import caps.*

@assumeSafe
trait Secure extends Level

@assumeSafe
sealed trait LLMResult
case class Success(response: String) extends LLMResult
case class Failure(reason: String) extends LLMResult

@assumeSafe
trait LLM:
  def complete(prompt: String): LLMResult
  def complete(prompt: Classified[Secure, String]): Classified[Secure, LLMResult]

@assumeSafe
object Policies:

  // Status: None for success (safe to release), Some(reason) for failure.
  // Failure reasons are considered non-sensitive.
  val resultStatus: Policy[Level, Secure, LLMResult, Option[String]] =
    Policy: r =>
      r match
        case Success(response) => None
        case Failure(reason)   => Some(reason)

  // Extract the response text. Failures propagate via an exception
  // through the Classified machinery.
  val extractResponse: Policy[Level, Secure, LLMResult, String] =
    Policy: r =>
      r match
        case Success(response) => response
        case Failure(reason)   => throw new Exception(s"LLM failed: $reason")

  // Whether the LLM call succeeded.
  val isSuccess: Policy[Level, Secure, LLMResult, Boolean] =
    Policy: r =>
      r match
        case Success(_) => true
        case Failure(_) => false

  // First `n` characters of the response. Failures produce a marker.
  def summary(n: Int): Policy[Level, Secure, LLMResult, String] =
    Policy: r =>
      r match
        case Success(response) => response.take(n)
        case Failure(reason)   => s"[failed: $reason]"

  // Response length in characters (-1 for failures).
  val responseLength: Policy[Level, Secure, LLMResult, Int] =
    Policy: r =>
      r match
        case Success(response) => response.length
        case Failure(_)        => -1
