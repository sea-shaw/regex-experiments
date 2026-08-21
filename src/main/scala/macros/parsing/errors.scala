package experiments.macros.parsing

import parsley.errors.{DefaultErrorBuilder, ErrorBuilder}
import parsley.errors.tokenextractors.TillNextWhitespace

object errors {
  case class Pos(offset: Int, width: Int)
  case class PosError(pos: Pos, msg: String)

  object PosErrorBuilder extends ErrorBuilder[PosError] with TillNextWhitespace {

    override def build(pos: Position, source: Source, lines: ErrorInfoLines): PosError = {
      PosError(lines.pos, lines.lines.mkString("\n"))
    }

    type Position = Unit

    type Source = Unit

    override def pos(line: Int, col: Int): Position = ()

    override def source(sourceName: Option[String]): Source = ()

    type ErrorInfoLines = (lines: Seq[String], pos: Pos)

    override def vanillaError(unexpected: UnexpectedLine, expected: ExpectedLine, reasons: Messages, line: LineInfo): ErrorInfoLines = {
      (Seq.concat(unexpected, expected, reasons), line)
    }

    override def specializedError(msgs: Messages, line: LineInfo): ErrorInfoLines = (msgs, line)

    type ExpectedItems = Option[String]
    type Messages = Seq[Message]

    override def combineExpectedItems(alts: Set[Item]): ExpectedItems = DefaultErrorBuilder.disjunct(alts)

    override def combineMessages(alts: Seq[Message]): Messages = DefaultErrorBuilder.combineMessages(alts)

    type UnexpectedLine = Option[String]
    type ExpectedLine = Option[String]
    type Message = String
    type LineInfo = Pos

    override def unexpected(item: Option[Item]): UnexpectedLine = DefaultErrorBuilder.unexpected(item)

    override def expected(alts: ExpectedItems): ExpectedLine = DefaultErrorBuilder.expected(alts)

    override def reason(reason: String): Message = DefaultErrorBuilder.reason(reason)

    override def message(msg: String): Message = DefaultErrorBuilder.message(msg)

    override def lineInfo(line: String, linesBefore: Seq[String], linesAfter: Seq[String], lineNum: Int, errorPointsAt: Int, errorWidth: Int): LineInfo = {
      Pos(errorPointsAt, errorWidth)
    }

    override val numLinesBefore: Int = 0

    override val numLinesAfter: Int = 0

    type Item = String
    type Raw = String
    type Named = String
    type EndOfInput = String

    override def raw(item: String): Raw = DefaultErrorBuilder.raw(item)

    override def named(item: String): Named = DefaultErrorBuilder.named(item)

    override val endOfInput: EndOfInput = DefaultErrorBuilder.EndOfInput

    override val trimToParserDemand: Boolean = true
  }
}
