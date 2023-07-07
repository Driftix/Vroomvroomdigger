
package controllers

import play.api.mvc._
import play.api.libs.streams.ActorFlow
import javax.inject.Inject
import akka.actor.ActorSystem
import akka.stream.Materializer
import akka.actor._
import javax.inject._
import akka.actor.ActorSystem
import akka.stream.Materializer
import play.api.mvc._
import play.api.libs.streams._
import akka.stream.scaladsl._
import scala.concurrent.duration._
import play.api.libs.json._
import scala.math._


@Singleton
class HomeController @Inject()(cc: ControllerComponents)(implicit system: ActorSystem, mat: Materializer) extends AbstractController(cc) {
  def index: Action[AnyContent] = Action { implicit request =>
    Ok(views.html.index())
  }                
                              
  def socket = WebSocket.accept[JsValue,JsValue] { request =>
    ActorFlow.actorRef{ out =>
      MyWebSocketActor.props(out)
    }
  }                         
}

object MyWebSocketActor {
  def props(out: ActorRef) = Props(new MyWebSocketActor(out))
}

class MyWebSocketActor(out: ActorRef) extends Actor {

  var isMovingForward = false
  var isMovingBackward = false
  var isTurningRight = false
  var isTurningLeft = false
  var playerAcceleration : Double = 0.1
  var playerMaxSpeed : Double = 5
  var driftFactor : Double = 1
  var driftSpeed : Double = 0 
  var rotationSmoothing = 0.5
  var score = 0
  //var playerSpeed = 0

  def receive = {
    case msg: JsValue if msg("event").as[String] == "move" =>

      val coordinates = msg("coordinates").as[JsArray]
      val playerSpeed = msg("playerSpeed").as[Double]
      //val playerAcceleration = msg("playerAcceleration").as[Double]
      val playerMaxSpeed = 5
      val playerAcceleration = 1
      val playerAngle = msg("playerAngle").as[Double]

      val keysString = msg("key").as[String]
      val keysJson = Json.parse(keysString)

      // Utilisez keysJson comme un objet JSON pour accéder aux valeurs des clés

      
      out ! JsObject(Seq(
          "event" -> JsString("updatePos"),
          "newPos" -> playerInput(keysString, coordinates, playerAngle, playerSpeed)
      ))
      
    case _ =>
      println("Message inconnu")
  }


  def playerInput(keys : String, coordinates : JsArray, playerAngletmp : Double, playerSpeedtmp : Double) : JsObject = {

    val keysJson = Json.parse(keys)
    val zValue = (keysJson \ "z").as[Boolean]
    val sValue = (keysJson \ "s").as[Boolean]
    val qValue = (keysJson \ "q").as[Boolean]
    val dValue = (keysJson \ "d").as[Boolean]
    val isHandbrake = (keysJson \ "space").as[Boolean]
    var playerX : Double = coordinates(0).as[Double]
    var playerY : Double = coordinates(1).as[Double]
    var playerAngle = playerAngletmp
    var playerSpeed = playerSpeedtmp

    if (zValue) {
        if (playerSpeed >= 0 && !isHandbrake) {
            isMovingForward = true;
            isMovingBackward = false;
        }
        playerSpeed += playerAcceleration;
      } else if (sValue) {
        if (playerSpeed <= 0 && !isHandbrake) {
            isMovingBackward = true;
            isMovingForward = false;
        }
        playerSpeed -= playerAcceleration;
      } else {
        // Deceleration
        if (playerSpeed > 0) {
            playerSpeed -= playerAcceleration;
        } else if (playerSpeed < 0) {
            playerSpeed += playerAcceleration;
        }

        // Complete stop
        if (abs(playerSpeed) < playerAcceleration) {
            isMovingForward = false;
            isMovingBackward = false;
            playerSpeed = 0;
        }
    }
    if (playerSpeed > playerMaxSpeed) {
        playerSpeed = playerMaxSpeed;
    } else if (playerSpeed < -playerMaxSpeed) {
        playerSpeed = -playerMaxSpeed;
    }

    playerX += sin(playerAngle) * playerSpeed;
    playerY -= cos(playerAngle) * playerSpeed;

    if (isHandbrake && playerSpeed > 0 && (isTurningLeft || isTurningRight)) {
        //Ici il drift
        score +=1         
        playerSpeed *= driftFactor; // Réduction de la vitesse pendant le drift
        driftSpeed += 0.2; // Augmentation de la vitesse latérale pendant le drift

        var driftAngle = playerAngle - Pi * 2; // Angle de drift (perpendiculaire à la direction de déplacement)

        // Correction de la position lors de la rotation à droite pendant le drift
        if (isTurningRight) {
            playerX += cos(driftAngle) * driftSpeed;
            playerY += sin(driftAngle) * driftSpeed;
        } else {
            playerX += cos(playerAngle) * driftSpeed;
            playerY += sin(playerAngle) * driftSpeed;
        }
    }
    // Adjust rotation angle based on speed with exponential interpolation
    var rotationAngle = 0.1 * playerSpeed / playerMaxSpeed;
    if (qValue && playerSpeed > 0) {
        isTurningLeft = true;
        playerAngle = interpolateAngle(playerAngle, playerAngle - rotationAngle, rotationSmoothing);
    } else {
        isTurningLeft = false;
    }
    if (dValue && playerSpeed > 0) {
        isTurningRight = true;
        playerAngle = interpolateAngle(playerAngle, playerAngle + rotationAngle, rotationSmoothing);
    } else {
        isTurningRight = false;
    }

    // Restreindre la vitesse latérale maximale pendant le drift
    if (driftSpeed > playerSpeed * 0.5) {
        driftSpeed = playerSpeed * 0.5;
    }

    // Restreindre la vitesse latérale maximale globale
    var maxDriftSpeed = playerMaxSpeed * 0.5;
    if (driftSpeed > maxDriftSpeed) {
        driftSpeed = maxDriftSpeed;
    }

    val resultJson = Json.obj(
    "playerAngle" -> playerAngle,
    "playerX" -> playerX,
    "playerY" -> playerY,
    "playerSpeed" -> playerSpeed,
    "score" -> score
    )

    resultJson
    
  }
  def interpolateAngle(currentAngle : Double , targetAngle: Double, smoothing : Double) : Double = {
      currentAngle * (1 - smoothing) + targetAngle * smoothing;
  }
  
}

