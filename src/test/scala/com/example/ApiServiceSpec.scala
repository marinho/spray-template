package com.example

import org.specs2.mutable.Specification
import spray.testkit.Specs2RouteTest
import spray.http._
import StatusCodes._
import spray.httpx.SprayJsonSupport._


class ApiServiceSpec
  extends Specification
  // with Matchers
  with Specs2RouteTest
  with ApiService
{
  def actorRefFactory = system

  "ApiService" should {
    "return a response" in {
      Get("/api/v1/say") ~> myRoute ~> check {
        status === OK
        responseAs[Message] === Message("Sorry?")
      }
    }

    "be able to say hi Linus" in {
      Post("/api/v1/say", Message("Hi, Linus!")) ~> myRoute ~> check {
        status === Created
        responseAs[Message] === Message("Hi, my name is Linus.")
      }
    }

    "be able to say hi Mario" in {
      Post("/api/v1/say", Message("Hi, Mario!")) ~> myRoute ~> check {
        status === Created
        responseAs[Message] === Message("Hi, my name is Mario.")
      }
    }

    "be able to ask" in {
      Post("/api/v1/say", Message("What time is now?")) ~> myRoute ~> check {
        status === Created
        responseAs[Message] === Message("It's 23 o'clock!")
      }
    }
  }
}
