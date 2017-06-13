package com.example

import akka.actor._
import spray.routing._
import spray.http._
import spray.json._
import MediaTypes._
import DefaultJsonProtocol._
import spray.httpx.SprayJsonSupport._
import scala.util.matching.Regex
import java.util.Calendar
import java.text.SimpleDateFormat


case object MessageCreated
case class Message(text: String)
object Message {
  implicit val messageFormat = jsonFormat1(Message.apply)
}

class ApiServiceActor extends Actor with ApiService {
  def actorRefFactory = context
  def receive = runRoute(myRoute)
}

trait ApiService extends HttpService {
  val myRoute =
    pathPrefix("api") {
      pathPrefix("v1") {
        path("say") {
          get {
            respondWithMediaType(`application/json`) {
              complete(Message("Sorry?"))
            }
          }
        } ~
        path("say") {
          post {
            entity(as[Message]) { message => //requestContext =>
              val pattern = PatternMatcher.forMessage(message.text)
              complete(201, Message(pattern.matchToMessage(message.text)))
              //val responder = createResponder(requestContext)
              //responder ! MessagePosted
            }
          }
        }
      }
    }
}

// private def createResponder(requestContext:RequestContext) = {
//   context.actorOf(Props(new Responder(requestContext)))
// }

trait Pattern {
  val pattern: Regex
  val response: String

  def isMatching(msg: String): Boolean = {
    msg match {
      case pattern(_*) => true
      case _ => false
    }
  }

  def matchToMessage(msg: String): String = {
    val result = pattern.findFirstMatchIn(msg).get
    val names = result.groupNames.toList
    val values = List(response) ::: names
    values.reduceLeft((acc, i) => acc.replace(s"{{$i}}", result.group(i)))
  }
}

class StandardPattern(val pattern: Regex, val response: String) extends Pattern

class TimePattern(val pattern: Regex, val response: String) extends Pattern {
  override def matchToMessage(msg: String): String = {
    val formatter = new SimpleDateFormat("HH")
    val now = Calendar.getInstance().getTime()
    response.replace(raw"{{hours}}", formatter.format(now))
  }
}

object PatternMatcher {
  val patterns = List(
    new StandardPattern(new Regex(raw"Hi, (\w+)!", "name"), "Hi, my name is {{name}}."),
    new TimePattern(raw"What time is now\?".r, "It's {{hours}} o'clock!")
  )

  val defaultPattern = new StandardPattern("".r, "Well, I don't know what you are talking about.")

  def forMessage(msg: String): Pattern = {
    patterns.find(_.isMatching(msg)) match {
      case Some(item) => item
      case None => defaultPattern
    }
  }
}
