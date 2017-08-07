package es.igarciadelamo.tennis

import akka.actor._

case class Ball(target: Int)
case class Play(opposite: ActorRef)
case class EndGame(winner: ActorRef)
case object StartGame


class TennisMatch extends Actor with ActorLogging {

  val player1 = context.actorOf(Props(new Player(self)), name = "Agassi")
  val player2 = context.actorOf(Props(new Player(self)), name = "Sampras")

  def receive = {
    case StartGame =>
      player1 ! Play(player2)

    case w: EndGame =>
      log.info("The winner is " + w.winner.path.name)
      context.stop(self)
      context.system.terminate()
  }

  override def postStop(): Unit =  log.info(s"Actor TennisMatch is stopped")
}


class Player(tm: ActorRef) extends Actor with ActorLogging {

  val LOWER_LIMIT = 0
  val UPPER_LIMIT = 10
  val DEVIATION = 4
  val MOVEMENT = 3

  var position = (UPPER_LIMIT - LOWER_LIMIT) / 2

  def name = self.path.name

  def hitTheBall: Int = {
    val rnd = scala.util.Random
    val left = rnd.nextBoolean()
    val factor = if (left) -1 else 1
    position + factor * rnd.nextInt(DEVIATION)
  }

  def thereIsAWinner(b: Ball): Option[ActorRef] = {
    if (b.target < LOWER_LIMIT || b.target > UPPER_LIMIT) {
      Some(self)
    } else if (position - MOVEMENT > b.target || position + MOVEMENT < b.target) {
      Some(sender)
    } else {
      None
    }
  }

  def receive = {

    case m: Play =>
      val where = hitTheBall
      log.info(s"First ball. Ball from position $position send to $where")
      m.opposite ! Ball(where)

    case b: Ball => thereIsAWinner(b) match {
      case Some(a) => tm ! EndGame(a)
      case None => {
        val where = hitTheBall
        val newPosition = b.target
        log.info(s"Move from position $position to $newPosition and send the ball to $where")
        position = newPosition
        sender ! Ball(where)
      }
    }
  }

  override def postStop(): Unit =  log.info(s"Actor $name is stopped")
}

object Main extends App {

  val system = ActorSystem("TennisMatch")
  val tennisMatch = system.actorOf(Props[TennisMatch], name = "TennisMatch")
  tennisMatch ! StartGame

}
