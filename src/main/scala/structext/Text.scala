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
import org.maraist.latex.{LaTeX, LaTeXdoc, LaTeXRenderable}

trait StructText extends LaTeXRenderable {
  override def toPlain(width: Int): String = text
  def toPlainWords: Seq[String]
  def toHTML: String
}

class Literal(text: String) extends StructText {
  override def toString: String = text
  override def toPlainWords: Seq[String] = ???
  override def toHTML: String = scala.xml.Utility.escape(text)
  override def toLaTeX(doc: LaTeXdoc): Unit = (doc ++= LaTeX.quoteString(text))
}

class Sequence(texts: Seq[StructText]) extends StructText {
  override def toString: String = ???
  override def toPlainWords: Seq[String] = ???
  override def toHTML: String = ???
  override def toLaTeX(doc: LaTeXdoc): Unit = ???
}

class Bold(text: StructText) extends StructText {
  override def toString: String = ???
  override def toPlainWords: Seq[String] = ???
  override def toHTML: String = ???
  override def toLaTeX(doc: LaTeXdoc): Unit = ???
}

class Italics(text: StructText) extends StructText {
  override def toString: String = ???
  override def toPlainWords: Seq[String] = ???
  override def toHTML: String = ???
  override def toLaTeX(doc: LaTeXdoc): Unit = ???
}

class Emph(text: StructText) extends StructText {
  override def toString: String = ???
  override def toPlainWords: Seq[String] = ???
  override def toHTML: String = ???
  override def toLaTeX(doc: LaTeXdoc): Unit = ???
}

class SansSerif(text: StructText) extends StructText {
  override def toString: String = ???
  override def toPlainWords: Seq[String] = ???
  override def toHTML: String = ???
  override def toLaTeX(doc: LaTeXdoc): Unit = ???
}

class Underline(text: StructText) extends StructText {
  override def toString: String = ???
  override def toPlainWords: Seq[String] = ???
  override def toHTML: String = ???
  override def toLaTeX(doc: LaTeXdoc): Unit = ???
}

class Color(color: String, text: StructText) extends StructText {
  override def toString: String = ???
  override def toPlainWords: Seq[String] = ???
  override def toHTML: String = ???
  override def toLaTeX(doc: LaTeXdoc): Unit = ???
}

class Anchored(url: String, anchorText: StructText) extends StructText {
  override def toString: String = ???
  override def toPlainWords: Seq[String] = ???
  override def toHTML: String = ???
  override def toLaTeX(doc: LaTeXdoc): Unit = ???
}

class Phonetic(text: StructText, phonetic: String) extends StructText {
  override def toString: String = ???
  override def toPlainWords: Seq[String] = ???
  override def toHTML: String = ???
  override def toLaTeX(doc: LaTeXdoc): Unit = ???
}

object Text {
  def str(text: String): StructText = Literal(text)
  def seq(texts: StructText*): StructText = Sequence(texts)
  def bf(text: StructText): StructText = Bold(text)
  def it(text: StructText): StructText = Italics(text)
  def sf(text: StructText): StructText = SansSerif(text)
  def emph(text: StructText): StructText = Emph(text)
  def underline(text: StructText): StructText = Underline(text)
  def color(color: String, text: StructText): StructText = Color(color, text)
  def linked(url: String, text: StructText): StructText = Anchored(url, text)
  def phonetic(text: StructText, phonetic: String): StructText =
    Phonetic(text, phonetic)
}

