import dotty.tools.dotc.Main
import dotty.tools.dotc.reporting.{Diagnostic, StoreReporter}

import java.io.File
import java.nio.file.Files

// Drives the full Scala 3 compiler on a snippet so that all phases run. 
// Macro helpers (`assertCompiles`, `compileErrors`, `typeChecks`) from many test frameworks 
// do not reach after typer.
//
// The classpath is read from the `classified.classpath` system
// property, which build.sbt populates from `Test / fullClasspath`
// before forking the test JVM.
object CompileHelper:

  final case class Result(succeeded: Boolean, errors: List[String]):
    def output: String = errors.mkString("\n")
    def errorsContain(s: String): Boolean = errors.exists(_.contains(s))

  // A StoreReporter subclass that exposes the buffered diagnostic
  // messages without needing a `Context`. The base class makes
  // `removeBufferedMessages` require a Context that we do not have
  // at the point where we want to read errors.
  private class CapturingReporter extends StoreReporter(outer = null, fromTyperState = false):
    def collectedErrors: List[String] =
      if infos == null then Nil
      else
        infos.nn.toList.collect {
          case d: Diagnostic if d.level >= 2 /* ERROR */ => d.msg.message
        }

  private lazy val classpath: String =
    sys.props
      .getOrElse("classified.classpath", System.getProperty("java.class.path"))
      .nn

  /** Compile `snippet` with the same experimental language features
    * the library itself uses. Returns whether the snippet compiled
    * cleanly and the captured error diagnostics.
    */
  def compile(snippet: String): Result =
    val source = File.createTempFile("snippet-", ".scala")
    source.deleteOnExit()
    Files.writeString(source.toPath, snippet)

    val outDir = Files.createTempDirectory("snippet-out-").toFile
    outDir.deleteOnExit()

    val args = Array(
      "-classpath", classpath,
      "-d", outDir.getAbsolutePath,
      "-language:experimental.captureChecking",
      "-language:experimental.modularity",
      "-language:experimental.safe",
      source.getAbsolutePath
    )

    val reporter = new CapturingReporter()
    Main.process(args, reporter, callback = null)
    Result(succeeded = !reporter.hasErrors, errors = reporter.collectedErrors)
