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
package softrains.base

import akka.actor._
import akka.http.scaladsl._
import akka.http.scaladsl.model._
import akka.http.scaladsl.model.headers._
import akka.http.scaladsl.unmarshalling._
import akka.stream._
import akka.stream.scaladsl._
import akka.util._

import scala.collection._
import scala.util._

class HttpConsumer(actorSystem : ActorSystem)
{
  private implicit val system = actorSystem

  private implicit val materializer =
    ActorMaterializer(ActorMaterializerSettings(system))

  private implicit val executionContext = system.dispatcher

  private var failure = ""

  private val phaser = new java.util.concurrent.Phaser(1)

  private def fail(message : String)
  {
    failure = message
    phaser.arrive
  }

  def getFailure = failure

  def fetchString(
    endpoint : String,
    headerSeq : immutable.Seq[HttpHeader] = immutable.Seq.empty)(
    completion : String => Unit)
  {
    val responseFuture =
      Http().singleRequest(HttpRequest(uri = endpoint, headers = headerSeq))
    // one for request
    phaser.register
    // one for unmarshal
    phaser.register
    responseFuture onComplete {
      case Success(response) => {
        response match {
          case HttpResponse(StatusCodes.OK, headers, entity, _) => {
            val unmarshalFuture = Unmarshal(entity).to[String]
            unmarshalFuture onComplete {
              case Success(body) => {
                completion(body)
                phaser.arrive
              }
              case Failure(t) => {
                fail(t.getMessage)
              }
            }
          }
          case HttpResponse(code, _, _, _) => {
            fail("HTTP response code " + code)
          }
        }
        phaser.arrive
      }
      case Failure(t) => {
        fail(t.getMessage)
        phaser.arrive
      }
    }
  }

  def putString(endpoint : String, data : String)(
    completion : => Unit)
  {
    sendString(endpoint, data, HttpMethods.PUT)(completion)
  }

  def postString(endpoint : String, data : String)(
    completion : => Unit)
  {
    sendString(endpoint, data, HttpMethods.POST)(completion)
  }

  private def sendString(endpoint : String, data : String,
    httpMethod : HttpMethod)(
    completion : => Unit)
  {
    val request = HttpRequest(
      httpMethod,
      uri = endpoint,
      entity = HttpEntity(
        MediaTypes.`text/plain` withCharset HttpCharsets.`UTF-8`,
        ByteString(data)),
      headers = List(Accept(immutable.Seq(MediaRange(
        MediaTypes.`application/json`)))))
    val requestFuture = Http().singleRequest(request)
    phaser.register
    def success(entity : HttpEntity) {
      entity.dataBytes.runWith(Sink.ignore)
      completion
      phaser.arrive
    }
    requestFuture onComplete {
      case Success(response) => {
        response match {
          case HttpResponse(StatusCodes.OK, _, entity, _) => {
            success(entity)
          }
          case HttpResponse(StatusCodes.Accepted, _, entity, _) => {
            success(entity)
          }
          case HttpResponse(code, _, _, _) => {
            fail("HTTP response code " + code)
          }
        }
      }
      case Failure(t) => {
        fail(t.getMessage)
      }
    }
  }

  def postMap(endpoint : String, map : immutable.Map[String, String])(
    completion : HttpResponse => Unit)
  {
    val request = HttpRequest(
      HttpMethods.POST,
      uri = endpoint,
      entity = FormData(map).toEntity)
    val requestFuture = Http().singleRequest(request)
    phaser.register
    requestFuture onComplete {
      case Success(response) => {
        response match {
          case HttpResponse(StatusCodes.OK, _, entity, _) => {
            entity.dataBytes.runWith(Sink.ignore)
            completion(response)
            phaser.arrive
          }
          case HttpResponse(code, _, _, _) => {
            fail("HTTP response code " + code)
          }
        }
      }
      case Failure(t) => {
        fail(t.getMessage)
      }
    }
  }

  def waitForCompletion()
  {
    phaser.arriveAndAwaitAdvance
  }

  def ensureSuccess()
  {
    waitForCompletion
    if (!failure.isEmpty) {
      throw new Exception(failure)
    }
  }
}

