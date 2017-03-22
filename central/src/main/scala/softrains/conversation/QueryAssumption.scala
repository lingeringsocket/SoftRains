// SoftRains:  a Genuine People Personality for your home
// Copyright 2016-2017 John V. Sichi
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
package softrains.conversation

object QueryAssumption extends Enumeration
{
  type QueryAssumption = Value

  val ASSUME_TRUE, ASSUME_FALSE, ASSUME_NOTHING = Value

  def generateConfirmation(assumption : QueryAssumption, truth : Boolean)
      : String =
  {
    assumption match {
      case ASSUME_TRUE => {
        if (truth) {
          "Yes, "
        } else {
          "No, "
        }
      }
      case ASSUME_FALSE => {
        if (truth) {
          "No, "
        } else {
          "Yes, "
        }
      }
      case _ => ""
    }
  }
}
