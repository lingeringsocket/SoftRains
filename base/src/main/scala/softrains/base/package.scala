// SoftRains:  a Genuine People Personality for your home
// Copyright 2016 John V. Sichi
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.
package softrains

import org.joda.time._

import java.io._
import scala.io._

package object base
{
  def readClockTime = new DateTime(DateTimeZone.UTC)

  def computeUptime(startTime : DateTime, checkTime : DateTime) =
  {
    val diff = Seconds.secondsBetween(startTime, checkTime)
    diff.getSeconds match {
      case d if (d > 86400) =>
        "days:  " + diff.toStandardDays.getDays
      case h if (h > 3600) =>
        "hours:  " + diff.toStandardHours.getHours
      case m if (m > 60) =>
        "minutes:  " + diff.toStandardMinutes.getMinutes
      case _ =>
        "seconds:  " + diff.getSeconds
    }
  }

  def getResourcePath(resource : String) =
    getClass.getResource(resource).getPath

  def getResourceFile(resource : String) =
    new File(getResourcePath(resource))

  def readResource(resource : String) : String =
    Source.fromFile(getResourcePath(resource)).
      getLines.mkString("\n")

  implicit class PipedObject[A](value: A) {
    def |>[B](f: A => B): B = f(value)
  }
}
