package eiti.sag


import akka.actor.Actor
import akka.http.scaladsl.Http
import akka.http.scaladsl.model._
import akka.http.scaladsl.server.Directives._
import akka.stream.ActorMaterializer
import eiti.sag.HttpServer.{Kaboom, ShutdownServer}

import scala.util.Random

class HttpServer extends Actor {
  implicit val actorSystem = context.system
  implicit val materializer = ActorMaterializer()
  implicit val executionContext = actorSystem.dispatcher
  val random = new Random()

  val route =
    path("killagent") {
      get {
        context.actorSelection("akka://AnimalsKnowledgeBase/user/KnowledgeAgentsSupervisor") ! Kaboom
        val msg = "Kaboom"
        complete(HttpEntity(ContentTypes.`text/html(UTF-8)`, s"<h1>$msg</h1>"))
      }
    }

  val bindingFuture = Http().bindAndHandle(route, "localhost", 8080)

  println(s"Server online at http://localhost:8080/")


  override def receive: Receive = {
    case ShutdownServer() => bindingFuture
      .flatMap(_.unbind())
      .onComplete(_ => {})
    case _ => println("Webserver received a msg")
  }

  override def postStop() {
    bindingFuture
      .flatMap(_.unbind())
      .onComplete(_ => {})
  }
}

object HttpServer {
  final case class Kaboom()
  final case class ShutdownServer()
}