// Copyright (C) 2021 John Maraist
// See the LICENSE.txt file distributed with this work for additional
// information regarding copyright ownership.
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
// implied, for NON-COMMERCIAL use.  See the License for the specific
// language governing permissions and limitations under the License.

package org.maraist.structext
import java.io.PrintWriter
import org.typelevel.paiges.Doc
import org.maraist.latex.{LaTeX, LaTeXdoc, LaTeXRenderable}

enum SpeakAs(val interpretAs: String, override val hashCode: Int) {
  case Cardinal extends SpeakAs("cardinal", 1)
  case Ordinal extends SpeakAs("ordinal", 2)
  case Characters extends SpeakAs("characters", 3)
  case Fraction extends SpeakAs("fraction", 4)
  case Expletive extends SpeakAs("expletive", 5)
  case Unit extends SpeakAs("unit", 6)
  case SpellOut extends SpeakAs("spell-out", 7)
  case Telephone extends SpeakAs("telephone", 8)
}

enum ProsodyRate(val rate: String, override val hashCode: Int) {
  case Percent(pct: Int) extends ProsodyRate(s"$pct%", 10+pct)
  case Omitted extends ProsodyRate("", 1)
  case XSlow extends ProsodyRate("x-slow", 2)
  case Slow extends ProsodyRate("slow", 3)
  case Medium extends ProsodyRate("medium", 4)
  case Fast extends ProsodyRate("fast", 5)
  case XFast extends ProsodyRate("x-fast", 6)
  case Default extends ProsodyRate("default", 7)
  def forArg(label: String): String = this match {
    case Omitted => ""
    case _ => s" $label=\"${rate}\""
  }
}

enum ProsodyPitch(val pitch: String, override val hashCode: Int) {
  case Hz(hz: Int) extends ProsodyPitch(s"${hz}Hz", 10+hz)
  case Omitted extends ProsodyPitch("", 1)
  case XLow extends ProsodyPitch("x-low", 2)
  case Low extends ProsodyPitch("low", 3)
  case Medium extends ProsodyPitch("medium", 4)
  case High extends ProsodyPitch("high", 5)
  case XHigh extends ProsodyPitch("x-high", 6)
  case Default extends ProsodyPitch("default", 7)
  def forArg(label: String): String = this match {
    case Omitted => ""
    case _ => s" $label=\"${pitch}\""
  }
}

enum ProsodyVolume(val volume: String, override val hashCode: Int) {
  case DB(db: Double) extends ProsodyVolume(s"${db}dB", 10+10*db.toInt)
  case Omitted extends ProsodyVolume("", 1)
  case Silent extends ProsodyVolume("silent", 2)
  case XSoft extends ProsodyVolume("x-soft", 3)
  case Soft extends ProsodyVolume("soft", 4)
  case Medium extends ProsodyVolume("medium", 5)
  case Loud extends ProsodyVolume("loud", 6)
  case XLoud extends ProsodyVolume("x-loud", 7)
  case Default extends ProsodyVolume("default", 8)
  def forArg(label: String): String = this match {
    case Omitted => ""
    case _ => s" $label=\"${volume}\""
  }
}

trait StructText extends LaTeXRenderable with Matchable {
  def toPlain(width: Int): String = toDoc.render(width)
  def docWordsSeparator: Doc = Doc.lineOrSpace
  def toDoc: Doc = Doc.fill(docWordsSeparator, toPlainWords.map(Doc.text))
  def toPlainWords: Array[String]
  def toHTML: String
  def toSSML: String
  def +(that: StructText): StructText = that match {
    case Sequence(txts) => new Sequence(this :: txts)
    case _ => Sequence(List(this, that))
  }
  def >(that: StructText): StructText = that match {
    case RunTogether(txts) => new RunTogether(this :: txts)
    case _ => RunTogether(List(this, that))
  }
  def fill(holeName: String, replacement: StructText): StructText
}

class WrappedStructText(val text: StructText,
  textStart: String = "", textEnd: String = "",
  htmlStart: String = "", htmlEnd: String = "",
  latexStart: String = "", latexEnd: String = "",
  ssmlStart: String = "", ssmlEnd: String = "", hashInc: Int = -1
) extends StructText {
  override def toString: String = text.toString
  override def toDoc: Doc =
    Doc.text(textStart) + super.toDoc + Doc.text(textEnd)
  override def toPlainWords: Array[String] = text.toPlainWords
  override def toHTML: String = htmlStart + text.toHTML + htmlEnd
  override def toSSML: String = ssmlStart + text.toSSML + ssmlEnd
  override def toLaTeX(doc: LaTeXdoc): Unit = {
    doc ++= latexStart
    text.toLaTeX(doc)
    doc ++= latexEnd
  }
  override def hashCode(): Int = {
    val mod = Int.MaxValue / 2 - 1
    val h1 = text.hashCode
    val h2 = textStart.hashCode
    val h3 = textEnd.hashCode
    val h4 = htmlStart.hashCode
    val h5 = htmlEnd.hashCode
    val h6 = latexStart.hashCode
    val h7 = latexEnd.hashCode
    (((((hashInc + h1 + h2) % mod + h3) % mod + h4) % mod + h5) % mod + h6)
      % mod + h7
  }
  override def fill(holeName: String, replacement: StructText): StructText =
    new WrappedStructText(text.fill(holeName, replacement),
      textStart, textEnd, htmlStart, htmlEnd, latexStart, latexEnd)
}

class Hole(val name: String) extends StructText {
  def err = throw new IllegalStateException(s"Unfilled hole $name")
  override def toString: String = err
  override def toPlainWords: Array[String] = err
  override def toHTML: String = err
  override def toLaTeX(doc: LaTeXdoc): Unit = err
  override def toSSML: String = err
  override def hashCode(): Int = 1
  override def fill(holeName: String, replacement: StructText): StructText =
    if holeName.equals(name) then replacement else this
}

class PlainText(val text: String) extends StructText {
  override def toString: String = text
  override def toPlainWords: Array[String] = text.split(" +")
  override def toHTML: String = scala.xml.Utility.escape(text)
  override def toSSML: String = text
  override def toLaTeX(doc: LaTeXdoc): Unit = (doc ++= LaTeX.quoteString(text))
  override def hashCode(): Int = text.hashCode
  override def fill(h: String, r: StructText): StructText = this
}

class Sequential(
  val texts: List[StructText],
  stringSep: String, plainSep: Doc, htmlSep: String, latexSep: String,
  ssmlSep: String = " "
) extends StructText {

  override def toString: String = texts.map(_.toString).mkString(stringSep)

  override def docWordsSeparator: Doc = plainSep

  override def toHTML: String = texts.map(_.toHTML).mkString(htmlSep)

  override def toSSML: String = texts.map(_.toSSML).mkString(ssmlSep)

  override def toPlainWords: Array[String] = {
    val bld = Array.newBuilder[String]
    for(t <- texts) do bld ++= t.toPlainWords
    bld.result
  }

  override def toLaTeX(doc: LaTeXdoc): Unit = {
    var sep = ""
    texts.map((t) => {
      doc ++= sep
      t.toLaTeX(doc)
      sep = latexSep
    })
  }

  override def hashCode(): Int =
    texts.map(_.hashCode).foldRight(10)((x, y) => x + y)

  override def fill(holeName: String, replacement: StructText): StructText =
    new Sequential(
      texts.map(_.fill(holeName, replacement)),
      stringSep, plainSep, htmlSep, latexSep)
}

class RunTogether(texts: List[StructText])
    extends Sequential(texts, "", Doc.empty, "", "") {
  override def >(that: StructText): StructText = that match {
    case RunTogether(txts) => new RunTogether(this :: txts)
    case _ => RunTogether(texts ++ List(that))
  }
  override def hashCode(): Int = 11 + super.hashCode
}

object RunTogether {
  def unapply(txt: RunTogether): Option[List[StructText]] = Some(txt.texts)
}

class Sequence(texts: List[StructText])
    extends Sequential(texts, " ", Doc.lineOrSpace, " ", " ") {
  override def +(that: StructText): StructText = that match {
    case Sequence(txts) => new Sequence(this :: txts)
    case _ => Sequence(texts ++ List(that))
  }
  override def hashCode(): Int = 12 + super.hashCode
}

object Sequence {
  def unapply(txt: Sequence): Option[List[StructText]] = Some(txt.texts)
}

class Bold(text: StructText)
extends WrappedStructText(
  text, Console.BOLD, Console.RESET, "<b>", "</b>", "\\textbf{", "}",
  hashInc=13)

class Italics(text: StructText)
extends WrappedStructText(text, "", "", "<i>", "</i>", "\\textit{", "}",
  hashInc=14)

class Slant(text: StructText)
extends WrappedStructText(text, "", "", "<i>", "</i>", "\\textsl{", "}",
  hashInc=15)

class Emph(text: StructText)
extends WrappedStructText(text, "", "", "<em>", "</em>", "\\emph{", "}",
  hashInc=16)

class SansSerif(text: StructText)
extends WrappedStructText(text, latexStart = "\\textsf{", latexEnd = "}",
  hashInc=17)

class SmallCaps(text: StructText)
extends WrappedStructText(text, latexStart = "\\textsc{", latexEnd = "}",
  hashInc=18)

class SpeakingHint(text: StructText, hint: SpeakAs)
extends WrappedStructText(text,
  ssmlStart = s"<say-as interpret-as=\"${hint.interpretAs}\">",
  ssmlEnd = "</say-as>",
  hashInc=19)

class Prosody(text: StructText,
  rate: ProsodyRate = ProsodyRate.Omitted,
  pitch: ProsodyPitch = ProsodyPitch.Omitted,
  range: ProsodyPitch = ProsodyPitch.Omitted,
  volume: ProsodyVolume = ProsodyVolume.Omitted)
    extends WrappedStructText(text,
      ssmlStart = s"<prosody${rate.forArg("rate")}${pitch.forArg("pitch")}${range.forArg("range")}${volume.forArg("volume")}>", ssmlEnd = "</prosody>") {
  override def hashCode(): Int =
    rate.hashCode + pitch.hashCode + range.hashCode
     + volume.hashCode + super.hashCode
}

class Sentence(text: StructText)
extends WrappedStructText(text, ssmlStart = "<s>", ssmlEnd = "</s>",
  hashInc=20)

class Underline(text: StructText)
extends WrappedStructText(
  text, Console.UNDERLINED, Console.RESET, "<u>", "</u>", "\\underline{", "}",
  hashInc=21)

// TODO But need some model of colors.
//
// class Color(color: String, text: StructText)
// extends WrappedStructText(text, "", "", "<>/", "</>", "\\text{", "}")

class Anchored(url: String, anchorText: StructText)
extends WrappedStructText(
  anchorText, htmlStart=s"<a href=\"$url\">", htmlEnd="</>",
  hashInc=22)

class Phonetic(text: StructText, phonetic: String)
extends WrappedStructText(
  text, "", s" (\"$phonetic\")",
  "", s" (\"$phonetic\")", "", s" (``$phonetic'')",
  hashInc=23)

object StructText {
  def str(text: String): StructText = PlainText(text)
  def seq(texts: StructText*): StructText = Sequence(List.from(texts))
  def bf(text: StructText): StructText = Bold(text)
  def it(text: StructText): StructText = Italics(text)
  def sf(text: StructText): StructText = SansSerif(text)
  def sl(text: StructText): StructText = Slant(text)
  def sc(text: StructText): StructText = SmallCaps(text)
  def blank(name: String): StructText = Hole(name)
  def emph(text: StructText): StructText = Emph(text)
  def speak(text: StructText, hint: SpeakAs) = SpeakingHint(text, hint)
  def underline(text: StructText): StructText = Underline(text)
  // def color(color: String, text: StructText): StructText = Color(color, text)
  def linked(url: String, text: StructText): StructText = Anchored(url, text)
  def phonetic(text: StructText, phonetic: String): StructText =
    Phonetic(text, phonetic)
  def sentence(text: StructText): StructText = Sentence(text)
  def prosody(text: StructText,
    rate: ProsodyRate = ProsodyRate.Omitted,
    pitch: ProsodyPitch = ProsodyPitch.Omitted,
    range: ProsodyPitch = ProsodyPitch.Omitted,
    volume: ProsodyVolume = ProsodyVolume.Omitted
  ) =
    Prosody(text, rate, pitch, range, volume)
}

given fromString: Conversion[String, StructText] with
  def apply(str: String): StructText = StructText.str(str)
