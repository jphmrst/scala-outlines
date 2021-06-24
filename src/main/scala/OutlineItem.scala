// Copyright (C) 2020 John Maraist
// See the LICENSE.txt file distributed with this work for additional
// information regarding copyright ownership.
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
// implied, for NON-COMMERCIAL use.  See the License for the specific
// language governing permissions and limitations under the License.

package org.maraist.outlines
import java.io.PrintWriter
import org.maraist.latex.{LaTeXdoc,LaTeXRenderable}

class OutlineItem[E](val heading: E, val items: Seq[OutlineItem[E]],
                     val summary: Option[E] = None)
extends LaTeXRenderable {

  def formatAsItem(
    dest: PrintWriter, lead: String, prefix: String = ""
  )(
    using controls: OutlineOptions
  ): Unit = {
    if (controls.summarize) {
      summary match {
        case Some(s) => dest.println(lead + prefix + s)
        case None => fullFormatAsItem(dest, lead, prefix)
      }
    } else {
      fullFormatAsItem(dest, lead, prefix)
    }
  }

  def fullFormatAsItem(dest:PrintWriter, lead:String, prefix:String)(
    using controls: OutlineOptions
  ): Unit = {
    dest.println(lead + prefix + heading)
    val sublead = "  " + lead
    for(item <- items) {
      item.formatAsItem(dest, sublead)
    }
  }

  override def toLaTeX(doc:LaTeXdoc): Unit = {
    doc ++= heading.toString()
    doc ++= "\n"
    items.size match {
      case 0 => { }
      case n => {
        doc ++= "\\begin{itemize}\n"
        for(item <- items) {
          doc ++= "\\item "
          item.toLaTeX(doc)
        }
        doc ++= "\\end{itemize}\n"
      }
    }
  }
}

object OutlineItem {
  def apply[E](e: E, items: OutlineItem[E]*): OutlineItem[E]
    = new OutlineItem[E](e, items)
  def item[E](e: E, items: OutlineItem[E]*): OutlineItem[E]
    = new OutlineItem[E](e, items)
  def summarized[E](summary: E, e: E, items: OutlineItem[E]*): OutlineItem[E]
    = new OutlineItem[E](e, items, Some(summary))

  given basicToItem[E]: Conversion[E, OutlineItem[E]] =
    new OutlineItem(_, Seq())
  given givenSeqToItem[E]: Conversion[Seq[E], OutlineItem[E]] with
    def apply(s: Seq[E]): OutlineItem[E] = {
      new OutlineItem[E](s.head, s.tail.map(basicToItem(_)))
    }
}
