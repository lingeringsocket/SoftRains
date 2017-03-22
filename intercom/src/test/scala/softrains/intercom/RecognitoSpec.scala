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
package softrains.intercom

import softrains.base._

import com.bitsinharmony.recognito._

import org.specs2.mutable._

object JavaSoundKludge
{
  def wrap[T](eval : => T) =
  {
    val cl = classOf[javax.sound.sampled.AudioSystem].getClassLoader
    val old = Thread.currentThread.getContextClassLoader
    try {
      Thread.currentThread.setContextClassLoader(cl)
      eval
    } finally {
      Thread.currentThread.setContextClassLoader(old)
    }
  }
}

class RecognitoSpec extends Specification
{
  private def getAudioFile(resource : String) =
    getResourceFile("/audio/" + resource)

  private def printResults(results : java.util.List[MatchResult[String]])
  {
    if (true) {
      return
    }
    for (i <- 0 until results.size) {
      val result = results.get(i)
      println("Key = " + result.getKey)
      println("Likelihood = " + result.getLikelihoodRatio)
    }
    println()
  }

  "Recognito" should
  {
    "identify voices" in {
      JavaSoundKludge.wrap {
        val recognito = new Recognito[String](22050.0f)
        recognito.createVoicePrint(
          "Allison", getAudioFile("allison-train.wav"))
        recognito.createVoicePrint(
          "Lisa", getAudioFile("lisa-train.wav"))
        recognito.createVoicePrint(
          "Michael", getAudioFile("michael-train.wav"))
        val allisonResults =
          recognito.identify(getAudioFile("allison-test.wav"))
        allisonResults.get(0).getKey must be equalTo("Allison")
        allisonResults.get(0).getLikelihoodRatio must be equalTo(62)
        printResults(allisonResults)
        val lisaResults =
          recognito.identify(getAudioFile("lisa-test.wav"))
        lisaResults.get(0).getKey must be equalTo("Lisa")
        lisaResults.get(0).getLikelihoodRatio must be equalTo(45)
        printResults(lisaResults)
        val michaelResults =
          recognito.identify(getAudioFile("michael-test.wav"))
        printResults(michaelResults)
        michaelResults.get(0).getKey must be equalTo("Michael")
        michaelResults.get(0).getLikelihoodRatio must be equalTo(89)
      }
    }
  }
}
