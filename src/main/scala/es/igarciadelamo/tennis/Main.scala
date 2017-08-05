package es.igarciadelamo.tennis

import akka.actor._
import es.igarciadelamo.tennis.Main.system

case class Ball (target: Int)
case class StartGame(opposite : ActorRef)
case class EndGame(winner : ActorRef)
case object StartMatch


class TennisMatch extends Actor {

  val player1 = system.actorOf(Props(new Player(self)), name = "Agassi")
  val player2 = system.actorOf(Props(new Player(self)), name = "Sampras")

  def receive = {
    case StartMatch =>
      player1 ! new StartGame(player2)

    case w : EndGame =>
      println("The winner is " + w.winner.path.name)
      context.stop(self)
  }
}


class Player(tm : ActorRef) extends Actor {

  var position = 5
  val LOWER_LIMIT = 0
  val UPPER_LIMIT = 10
  val DEVIATION = 4
  val MOVEMENT = 3

  def name = self.path.name

  def hitTheBall : Int = {
    val rnd = new scala.util.Random
    val left = rnd.nextBoolean()
    val factor = if(left) -1 else 1
    position + factor * rnd.nextInt(DEVIATION)
  }

  def thereIsAWinner(b: Ball): Option[ActorRef] = {
    if(b.target < LOWER_LIMIT || b.target > UPPER_LIMIT){
      Some(self)
    }else if(position - MOVEMENT > b.target || position + MOVEMENT < b.target ){
      Some(sender)
    }else {
      None
    }
  }

  def receive = {

    case m : StartGame =>
      val where = hitTheBall
      println(s"First ball. Ball from $name (from $position) send the ball to $where")
      m.opposite ! Ball(where)

    case b : Ball => thereIsAWinner(b) match {
        case Some(a) => tm ! EndGame(a)
        case None => {
          val where = hitTheBall
          val newPosition = b.target
          println(s"Ball from $name (move from $position to $newPosition) send the ball to $where")
          position = newPosition
          sender ! Ball(where)
        }
      }
  }
}

object Main extends App {

  val system = ActorSystem("TennisMatch")
  val tennisMatch = system.actorOf(Props[TennisMatch])
  tennisMatch ! StartMatch

}
