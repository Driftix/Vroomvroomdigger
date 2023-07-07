package controllers

import akka.actor.ActorSystem
import akka.stream.Materializer
import javax.inject._
import play.api.mvc._
import play.api.libs.streams._
import akka.stream.scaladsl._

@Singleton
class MyWebSocketController @Inject()(cc: ControllerComponents)
                                     (implicit system: ActorSystem, mat: Materializer) extends AbstractController(cc) {

  def socket: WebSocket = WebSocket.accept[String, String] { request =>
    // Logique de gestion des connexions websocket
    val in = Sink.ignore // Ignorer les messages d'entrée
    val out = Source.single("Bonjour !") // Envoyer un message de bienvenue

    Flow.fromSinkAndSource(in, out)
  }
}
