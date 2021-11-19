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
  def +(that: StructText): StructText = that match {
    case Sequence(txts) => new Sequence(this :: txts)
    case _ => Sequence(List(this, that))
  }
  def >(that: StructText): StructText = that match {
    case RunTogether(txts) => new RunTogether(this :: txts)
    case _ => RunTogether(List(this, that))
  }
}

class WrappedStructText(val text: StructText,
  textStart: String, textEnd: String,
  htmlStart: String, htmlEnd: String,
  latexStart: String, latexEnd: String
) extends StructText {
  override def toString: String = text.toString
  override def toDoc: Doc =
    Doc.text(textStart) + super.toDoc + Doc.text(textEnd)
  override def toPlainWords: Array[String] = text.toPlainWords
  override def toHTML: String = htmlStart + text.toHTML + htmlEnd
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
    (((((h1 + h2) % mod + h3) % mod + h4) % mod + h5) % mod + h6) % mod + h7
  }
}

class PlainText(val text: String) extends StructText {
  override def toString: String = text
  override def toPlainWords: Array[String] = text.split(" +")
  override def toHTML: String = scala.xml.Utility.escape(text)
  override def toLaTeX(doc: LaTeXdoc): Unit = (doc ++= LaTeX.quoteString(text))
  override def hashCode(): Int = text.hashCode
}

class Sequential(val texts: List[StructText],
  stringSep: String, plainSep: Doc, htmlSep: String, latexSep: String)
    extends StructText {

  override def toString: String = texts.map(_.toString).mkString(stringSep)

  override def docWordsSeparator: Doc = plainSep

  override def toHTML: String = texts.map(_.toHTML).mkString(htmlSep)

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
}

class RunTogether(texts: List[StructText])
    extends Sequential(texts, "", Doc.empty, "", "") {
  override def >(that: StructText): StructText = that match {
    case RunTogether(txts) => new RunTogether(this :: txts)
    case _ => RunTogether(texts ++ List(that))
  }
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
}

object Sequence {
  def unapply(txt: Sequence): Option[List[StructText]] = Some(txt.texts)
}

class Bold(text: StructText)
extends WrappedStructText(
  text, Console.BOLD, Console.RESET, "<b>", "</b>", "\\textbf{", "}")

class Italics(text: StructText)
extends WrappedStructText(text, "", "", "<i>", "</i>", "\\textit{", "}")

class Slant(text: StructText)
extends WrappedStructText(text, "", "", "<i>", "</i>", "\\textsl{", "}")

class Emph(text: StructText)
extends WrappedStructText(text, "", "", "<em>", "</em>", "\\emph{", "}")

class SansSerif(text: StructText)
extends WrappedStructText(text, "", "", "", "", "\\textsf{", "}")

class SmallCaps(text: StructText)
extends WrappedStructText(text, "", "", "", "", "\\textsc{", "}")

class Underline(text: StructText)
extends WrappedStructText(
  text, Console.UNDERLINED, Console.RESET,
  "<u>", "</u>", "\\underline{", "}")

// TODO But need some model of colors.
//
// class Color(color: String, text: StructText)
// extends WrappedStructText(text, "", "", "<>", "</>", "\\text{", "}")

class Anchored(url: String, anchorText: StructText)
extends WrappedStructText(
  anchorText, "", "", s"<a href=\"$url\">", "</>", "", ""
)

class Phonetic(text: StructText, phonetic: String)
extends WrappedStructText(
  text, "", s" (\"$phonetic\")",
  "", s" (\"$phonetic\")", "", s" (``$phonetic'')")

object StructText {
  def str(text: String): StructText = PlainText(text)
  def seq(texts: StructText*): StructText = Sequence(List.from(texts))
  def bf(text: StructText): StructText = Bold(text)
  def it(text: StructText): StructText = Italics(text)
  def sf(text: StructText): StructText = SansSerif(text)
  def sl(text: StructText): StructText = Slant(text)
  def sc(text: StructText): StructText = SmallCaps(text)
  def emph(text: StructText): StructText = Emph(text)
  def underline(text: StructText): StructText = Underline(text)
  // def color(color: String, text: StructText): StructText = Color(color, text)
  def linked(url: String, text: StructText): StructText = Anchored(url, text)
  def phonetic(text: StructText, phonetic: String): StructText =
    Phonetic(text, phonetic)
}

given fromString: Conversion[String, StructText] with
  def apply(str: String): StructText = StructText.str(str)
