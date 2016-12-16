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
package softrains.intercom

import java.io._

import com.bitsinharmony.recognito._

import org.specs2.mutable._

class RecognitoSpec extends Specification
{
  private def getAudioResource(resource : String) =
    new File(getClass.getResource("/audio/" + resource).getPath)

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
      val cl = classOf[javax.sound.sampled.AudioSystem].getClassLoader
      val old = Thread.currentThread.getContextClassLoader
      try {
        Thread.currentThread.setContextClassLoader(cl)
        val recognito = new Recognito[String](22050.0f)
        recognito.createVoicePrint(
          "Allison", getAudioResource("allison-train.wav"))
        recognito.createVoicePrint(
          "Lisa", getAudioResource("lisa-train.wav"))
        recognito.createVoicePrint(
          "Michael", getAudioResource("michael-train.wav"))
        val allisonResults =
          recognito.identify(getAudioResource("allison-test.wav"))
        allisonResults.get(0).getKey must be equalTo("Allison")
        allisonResults.get(0).getLikelihoodRatio must be equalTo(62)
        printResults(allisonResults)
        val lisaResults =
          recognito.identify(getAudioResource("lisa-test.wav"))
        lisaResults.get(0).getKey must be equalTo("Lisa")
        lisaResults.get(0).getLikelihoodRatio must be equalTo(45)
        printResults(lisaResults)
        val michaelResults =
          recognito.identify(getAudioResource("michael-test.wav"))
        printResults(michaelResults)
        michaelResults.get(0).getKey must be equalTo("Michael")
        michaelResults.get(0).getLikelihoodRatio must be equalTo(89)
      } finally {
        Thread.currentThread.setContextClassLoader(old)
      }
    }
  }
}
