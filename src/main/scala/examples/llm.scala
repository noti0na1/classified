package classified
package examples

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

val LLMPolicy: Policy[Level, Secure, LLMResult, Option[String]] =
  Policy: r =>
    r match
      case Success(response) => None
      case Failure(reason)   => Some(reason)
   