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
  def dump: String
}

class WrappedStructText(val text: StructText,
  val tag: String,
  textStart: String = "", textEnd: String = "",
  htmlStart: String = "", htmlEnd: String = "",
  latexStart: String = "", latexEnd: String = "",
  ssmlStart: String = "", ssmlEnd: String = ""
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
    (((((tag.hashCode + text.hashCode + textStart.hashCode) % mod
      + textEnd.hashCode) % mod + htmlStart.hashCode) % mod + htmlEnd.hashCode)
      % mod + latexStart.hashCode) % mod + latexEnd.hashCode
  }
  override def fill(holeName: String, replacement: StructText): StructText =
    new WrappedStructText(text.fill(holeName, replacement), tag,
      textStart, textEnd, htmlStart, htmlEnd, latexStart, latexEnd,
      ssmlStart, ssmlEnd)
  override def dump: String = s"$tag(${text.dump}$dumpArgs)"
  def dumpArgs: String = ""
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
  override def dump: String = s"Hole($name)"
}

class PlainText(val text: String) extends StructText {
  override def toString: String = text
  override def toPlainWords: Array[String] = text.split(" +")
  override def toHTML: String = scala.xml.Utility.escape(text)
  override def toSSML: String = text
  override def toLaTeX(doc: LaTeXdoc): Unit = (doc ++= LaTeX.quoteString(text))
  override def hashCode(): Int = text.hashCode
  override def fill(h: String, r: StructText): StructText = this
  override def dump: String = s"PlainText(\"$text\")"
}

class Pause(val weight: PauseWeight) extends StructText {
  override def toString: String = ""
  override def toPlainWords: Array[String] = Array.empty[String]
  override def toHTML: String = ""
  override def toSSML: String = s"<break${weight.toBreakArgs}/>"
  override def toLaTeX(doc: LaTeXdoc): Unit = { }
  override def hashCode(): Int = weight.hashCode
  override def fill(h: String, r: StructText): StructText = this
  override def dump: String = s"Pause($weight)"
}

class Sequential(
  val texts: List[StructText],
  stringSep: String, plainSep: Doc, htmlSep: String, latexSep: String,
  ssmlSep: String
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
      stringSep, plainSep, htmlSep, latexSep, ssmlSep)

  override def dump: String = s"Sequential(List(${texts.map(_.dump).mkString(", ")}), stringSep=\"$stringSep\", plainSep=\"$plainSep\", htmlSep=\"$htmlSep\", latexSep=\"$latexSep\", ssmlSep=\"$ssmlSep\")"
}

class RunTogether(texts: List[StructText])
    extends Sequential(texts, "", Doc.empty, "", "", "") {
  override def >(that: StructText): StructText = that match {
    case RunTogether(txts) => new RunTogether(this :: txts)
    case _ => RunTogether(texts ++ List(that))
  }
  override def hashCode(): Int = 11 + super.hashCode
  override def dump: String = {
    val textsStr = texts.map(_.dump).mkString(", ")
    s"RunTogether(List($textsStr))"
  }
}

object RunTogether {
  def unapply(txt: RunTogether): Option[List[StructText]] = Some(txt.texts)
}

class Sequence(texts: List[StructText])
    extends Sequential(texts, " ", Doc.lineOrSpace, " ", " ", " ") {
  override def +(that: StructText): StructText = that match {
    case Sequence(txts) => new Sequence(this :: txts)
    case _ => Sequence(texts ++ List(that))
  }
  override def hashCode(): Int = 12 + super.hashCode
  override def dump: String =
    s"Sequence(List(${texts.map(_.dump).mkString(", ")}))"
}


object Sequence {
  def unapply(txt: Sequence): Option[List[StructText]] = Some(txt.texts)
}

class Bold(text: StructText)
extends WrappedStructText(
  text, "Bold", Console.BOLD, Console.RESET, "<b>", "</b>", "\\textbf{", "}")

class Italics(text: StructText)
extends WrappedStructText(
  text, "Italics", "", "", "<i>", "</i>", "\\textit{", "}")

class Slant(text: StructText)
extends WrappedStructText(
  text, "Slant", "", "", "<i>", "</i>", "\\textsl{", "}")

class Emph(text: StructText)
extends WrappedStructText(
  text, "Emph", "", "", "<em>", "</em>", "\\emph{", "}")

class SansSerif(text: StructText)
extends WrappedStructText(
  text, "SansSerif", latexStart = "\\textsf{", latexEnd = "}")

class SmallCaps(text: StructText)
extends WrappedStructText(
  text, "SmallCaps", latexStart = "\\textsc{", latexEnd = "}")

class SpeakingHint(text: StructText, hint: SpeakAs)
extends WrappedStructText(text, "SpeakingHint",
  ssmlStart = s"<say-as ${hint.toSpeakAsArgs}>",
  ssmlEnd = "</say-as>") {
  override def fill(holeName: String, replacement: StructText): StructText =
    new SpeakingHint(text.fill(holeName, replacement), hint)
}

class Prosody(text: StructText,
  rate: ProsodyRate = ProsodyRate.Omitted,
  pitch: ProsodyPitch = ProsodyPitch.Omitted,
  range: ProsodyPitch = ProsodyPitch.Omitted,
  volume: ProsodyVolume = ProsodyVolume.Omitted)
extends WrappedStructText(text, "Prosody",
  ssmlStart = s"<prosody${rate.forArg("rate")}${pitch.forArg("pitch")}${range.forArg("range")}${volume.forArg("volume")}>",
  ssmlEnd = "</prosody>"
) {
  override def fill(holeName: String, replacement: StructText): StructText =
    new Prosody(text.fill(holeName, replacement), rate, pitch, range, volume)
  override def hashCode(): Int =
    rate.hashCode + pitch.hashCode + range.hashCode
     + volume.hashCode + super.hashCode
}

class Sentence(text: StructText)
extends WrappedStructText(
  text, "Sentence", ssmlStart = "<s>", ssmlEnd = "</s>")

class Underline(text: StructText)
extends WrappedStructText(text, "Underline",
  Console.UNDERLINED, Console.RESET, "<u>", "</u>", "\\underline{", "}")

// TODO But need some model of colors.
//
// class Color(color: String, text: StructText)
// extends WrappedStructText(text, "Color", "", "", "<>/", "</>", "\\text{", "}")

class Anchored(url: String, anchorText: StructText)
extends WrappedStructText(
  anchorText, "Anchored", htmlStart=s"<a href=\"$url\">", htmlEnd="</>")

class DQuoted(text: StructText)
    extends WrappedStructText(text, "DQuoted",
      latexStart = "``", latexEnd = "''",
      htmlStart = "\"", htmlEnd = "\"",
      textStart = "\"", textEnd = "\"")

class Phonetic(
  text: StructText, written: Option[String], val phonemes: Phonemes)
extends WrappedStructText(
  text, "Phonetic",
  textEnd = written.map(" (\"" + _ + "\")").getOrElse(""),
  htmlEnd = written.map(" (\"" + _ + "\")").getOrElse(""),
  latexEnd = written.map(" (``" + _ + "'')").getOrElse(""),
  ssmlStart = phonemes match {
    case Phonemes.IPA(phs) => s"<phoneme alphabet=\"ipa\" ph=\"$phs\">"
    case _ => ""
  },
  ssmlEnd = phonemes match {
    case Phonemes.IPA(_) => "</phoneme>"
    case _ => ""
  }
) {
  def this(text: StructText, written: Some[String]) =
    this(text, written, Phonemes.None)
  override def hashCode(): Int =
    written.hashCode + phonemes.hashCode + super.hashCode
}

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
  def doublequoted(text: StructText): StructText = DQuoted(text)
  def speak(text: StructText, hint: SpeakAs) = SpeakingHint(text, hint)
  def phone(number: String) = speak(str(number), SpeakAs.Telephone)
  def underline(text: StructText): StructText = Underline(text)
  def linked(url: String, text: StructText): StructText = Anchored(url, text)
  def phonetic(text: StructText, phonetic: String): StructText =
    Phonetic(text, Some(phonetic))
  def phonetic(text: StructText, phonetic: String, ipa: String): Phonetic =
    Phonetic(text, Some(phonetic), Phonemes.IPA(ipa))
  def phoneticIPA(text: StructText, ipa: String): Phonetic =
    Phonetic(text, None, Phonemes.IPA(ipa))
  def sentence(text: StructText): StructText = Sentence(text)
  def prosody(text: StructText,
    rate: ProsodyRate = ProsodyRate.Omitted,
    pitch: ProsodyPitch = ProsodyPitch.Omitted,
    range: ProsodyPitch = ProsodyPitch.Omitted,
    volume: ProsodyVolume = ProsodyVolume.Omitted
  ) =
    Prosody(text, rate, pitch, range, volume)
  def pause(weight: PauseWeight): StructText = Pause(weight)
  def spellout(acronym: String): StructText =
    speak(str(acronym), SpeakAs.SpellOut)
  def date(d: String, elements: String = "mdy"): StructText =
    speak(str(d), SpeakAs.Date(elements))
  def time(t: String, elements: String = "hms12"): StructText =
    speak(str(t), SpeakAs.Time(elements))
  // def color(color: String, text: StructText): StructText = Color(color, text)
}

given fromString: Conversion[String, StructText] with
  def apply(str: String): StructText = StructText.str(str)

