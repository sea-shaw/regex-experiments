package experiments.benchmark.oregano

import experiments.macros.catnip.r
import java.util.concurrent.TimeUnit
import org.openjdk.jmh.annotations.*

@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@State(Scope.Thread)
class CatnipBenchmarks {

  val inputGreedyCapture = "a" * 1000 + "b"
  val inputAlternationHit = "grault"
  val inputAlternationFail = "foo" * 1000 + "nope"
  val inputAmbiguous = "a" * 20 + "b"
  val inputRepeatingLiteral = "ab" * 100 + "c"
  val inputManyGroups = "abcdefghij"

  val patternGreedyCapture = r"(a*)b"
  val patternAlternationHit = r"(foo|bar|baz|qux|quux|grault|garply|waldo|fred)"
  val patternAlternationFail = r"(foo|bar|baz)*quux"
  val patternAmbiguous = r"(a|aa)*b"
  val patternRepeatingLiteral = r"(ab)*c"
  val patternManyGroups = r"(a)(b)(c)(d)(e)(f)(g)(h)(i)(j)"

  @Benchmark
  def matchGreedyCapture() = {
    patternGreedyCapture.unapply(inputGreedyCapture)
  }

  @Benchmark
  def matchAlternationHit() = {
    patternAlternationHit.unapply(inputAlternationHit)
  }

  @Benchmark
  def matchAlternationFail() = {
    patternAlternationFail.unapply(inputAlternationFail)
  }

  @Benchmark
  def matchAmbiguousBacktrack() = {
    patternAmbiguous.unapply(inputAmbiguous)
  }

  @Benchmark
  def matchRepeatingLiteral() = {
    patternRepeatingLiteral.unapply(inputRepeatingLiteral)
  }

  @Benchmark
  def matchManyGroup() = {
    patternManyGroups.unapply(inputManyGroups)
  }
}
