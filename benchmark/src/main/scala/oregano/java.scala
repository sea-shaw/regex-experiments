package experiments.benchmark.oregano

import java.util.concurrent.TimeUnit
import org.openjdk.jmh.annotations.*

@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@State(Scope.Thread)
class JavaBenchmarks {

  val inputGreedyCapture = "a" * 1000 + "b"
  val inputAlternationHit = "grault"
  val inputAlternationFail = "foo" * 1000 + "nope"
  val inputAmbiguous = "a" * 20 + "b"
  val inputRepeatingLiteral = "ab" * 100 + "c"
  val inputManyGroups = "abcdefghij"

  val patternGreedyCapture = "(a*)b".r
  val patternAlternationHit = "(foo|bar|baz|qux|quux|grault|garply|waldo|fred)".r
  val patternAlternationFail = "(foo|bar|baz)*quux".r
  val patternAmbiguous = "(a|aa)*b".r
  val patternRepeatingLiteral = "(ab)*c".r
  val patternManyGroups = "(a)(b)(c)(d)(e)(f)(g)(h)(i)(j)".r

  @Benchmark
  def matchGreedyCapture() = {
    patternGreedyCapture.unapplySeq(inputGreedyCapture)
  }

  @Benchmark
  def matchAlternationHit() = {
    patternAlternationHit.unapplySeq(inputAlternationHit)
  }

  @Benchmark
  def matchAlternationFail() = {
    patternAlternationFail.unapplySeq(inputAlternationFail)
  }

  @Benchmark
  def matchAmbiguousBacktrack() = {
    patternAmbiguous.unapplySeq(inputAmbiguous)
  }

  @Benchmark
  def matchRepeatingLiteral() = {
    patternRepeatingLiteral.unapplySeq(inputRepeatingLiteral)
  }

  @Benchmark
  def matchManyGroup() = {
    patternManyGroups.unapplySeq(inputManyGroups)
  }
}
