package ex1

/** Consider the Parser example shown in previous lesson. Analogously to
  * NonEmpty, create a mixin NotTwoConsecutive, which adds the idea that one
  * cannot parse two consecutive elements which are equal. Use it (as a mixin)
  * to build class NotTwoConsecutiveParser, used in the testing code at the end.
  * Note we also test that the two mixins can work together!!
  */

abstract class Parser[T]:
  def parse(t: T): Boolean // is the token accepted?
  def end: Boolean // is it ok to end here
  def parseAll(seq: Seq[T]): Boolean =
    (seq forall parse) & end // note &, not &&

object Parsers:
  def apply[T](f: T => Boolean, e: Boolean): Parser[T] =
    new Parser[T]:
      override def parse(t: T): Boolean = f(t)
      override def end: Boolean = e

class BasicParser(chars: Set[Char]) extends Parser[Char]:
  override def parse(t: Char): Boolean = chars.contains(t)
  override def end: Boolean = true

trait NonEmpty[T] extends Parser[T]:
  private[this] var empty = true
  abstract override def parse(t: T): Boolean =
    empty = false;
    super.parse(t) // who is super??
  abstract override def end: Boolean = !empty && super.end

class NonEmptyParser(chars: Set[Char])
    extends BasicParser(chars)
    with NonEmpty[Char]

trait NotTwoConsecutive[T] extends Parser[T]:
  private[this] var last: Option[T] = None
  abstract override def parse(t: T): Boolean =
    val result = super.parse(t)
    if last.contains(t) then false
    else
      last = Some(t)
      result
  abstract override def end: Boolean = super.end

class NotTwoConsecutiveParser(chars: Set[Char])
    extends BasicParser(chars)
    with NotTwoConsecutive[Char]

trait ShortenThenN[T](maxLength: Int) extends Parser[T]:
  private[this] var length = 0
  abstract override def parse(t: T): Boolean =
    if length < maxLength then
      length += 1
      super.parse(t)
    else false

class ShortenThenNParser(chars: Set[Char], maxLength: Int)
    extends BasicParser(chars)
    with ShortenThenN[Char](maxLength)

@main def checkParsers(): Unit =
  def parser = new BasicParser(Set('a', 'b', 'c'))
  println(parser.parseAll("aabc".toList)) // true
  println(parser.parseAll("aabcdc".toList)) // false
  println(parser.parseAll("".toList)) // true

  // Note NonEmpty being "stacked" on to a concrete class
  // Bottom-up decorations: NonEmptyParser -> NonEmpty -> BasicParser -> Parser
  def parserNE = new NonEmptyParser(Set('0', '1'))
  println(parserNE.parseAll("0101".toList)) // true
  println(parserNE.parseAll("0123".toList)) // false
  println(parserNE.parseAll(List())) // false

  // NotTwoConsecutive[Char] -> BasicParser -> Parser[Char]
  def parserNTC = new NotTwoConsecutiveParser(Set('X', 'Y', 'Z'))
  println(parserNTC.parseAll("XYZ".toList)) // true
  println(parserNTC.parseAll("XYYZ".toList)) // false
  println(parserNTC.parseAll("".toList)) // true

  // note we do not need a class name here, we use the structural type
  def parserNTCNE = new BasicParser(Set('X', 'Y', 'Z'))
    with NotTwoConsecutive[Char]
    with NonEmpty[Char]
  println(parserNTCNE.parseAll("XYZ".toList)) // true
  println(parserNTCNE.parseAll("XYYZ".toList)) // false
  println(parserNTCNE.parseAll("".toList)) // false

  def sparser: Parser[Char] =
    new BasicParser(Set('a', 'b', 'c'))
  println(sparser.parseAll("aabc".toList)) // true
  println(sparser.parseAll("aabcdc".toList)) // false
  println(sparser.parseAll("".toList)) // true

  def parserSTN =
    new ShortenThenNParser(Set('0', '1'), 3)
  println(parserSTN.parseAll("101".toList)) // true
  println(parserSTN.parseAll("1010".toList)) // false