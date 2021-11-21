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

enum Phonemes(override val hashCode: Int) {
  case None extends Phonemes(1)
  case IPA(phonemes: String) extends Phonemes(phonemes.hashCode)
}

enum PauseWeight(override val hashCode: Int, val toBreakArgs: String) {
  case Seconds(val secs: Int)
      extends PauseWeight(secs * 10000, s" time=\"${secs}s\"")
  case Milliseconds(val ms: Int)
      extends PauseWeight(ms * 10, s" time=\"${ms}ms\"")
  case None extends PauseWeight(1, " strength=\"none\"")
  case XWeak extends PauseWeight(2, " strength=\"x-weak\"")
  case Weak extends PauseWeight(3, " strength=\"weak\"")
  case Medium extends PauseWeight(4, " strength=\"medium\"")
  case Strong extends PauseWeight(5, " strength=\"strong\"")
  case XStrong extends PauseWeight(6, " strength=\"x-strong\"")
}

enum SpeakAs(val interpretAs: String, override val hashCode: Int) {
  case Cardinal extends SpeakAs("cardinal", 1)
  case Ordinal extends SpeakAs("ordinal", 2)
  case Characters extends SpeakAs("characters", 3)
  case Fraction extends SpeakAs("fraction", 4)
  case Expletive extends SpeakAs("expletive", 5)
  case Unit extends SpeakAs("unit", 6)
  case SpellOut extends SpeakAs("spell-out", 7)
  case Telephone extends SpeakAs("telephone", 8)
  case Date(val elements: String) extends SpeakAs("date", 9)
  def toSpeakAsArgs: String = this match {
    case Date(elements) =>
      s"interpret-as=\"${interpretAs}\" format=\"$elements\""
    case _ => s"interpret-as=\"${interpretAs}\""
  }
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

