package es.igarciadelamo.tennis

import akka.actor.{ActorRef, ActorSystem, Props}
import akka.testkit.{DefaultTimeout, ImplicitSender, TestKit, TestProbe}
import org.scalatest.{FunSpecLike, Matchers}

class TennisTest extends TestKit(ActorSystem("testSystem"))
  with DefaultTimeout
  with ImplicitSender
  with FunSpecLike
  with Matchers {

  describe ("TennisMatch Behaviour"){

    val player1Mock = TestProbe()
    val player2Mock = TestProbe()

    val tm = system.actorOf(Props(creator = new TennisMatch {
      override val player1: ActorRef = player1Mock.ref
      override val player2: ActorRef = player2Mock.ref
    }))

    it("Receives a new match message"){
      tm.! (StartGame)
      player1Mock.expectMsg(Play(player2Mock.ref))
    }
  }


  describe ("Player Behaviour"){

    val tennisMatch = TestProbe()
    val opposite = TestProbe()

    val random = 5 + scala.util.Random.nextInt(3)

    val player = system.actorOf(Props(new Player(tennisMatch.ref){
      override def hitTheBall: Int = random
    }
    ))

    it("Receives a start match message"){
      player.! (Play(opposite.ref))
      opposite.expectMsg(Ball(random))
    }

    it("Receives a ball out of the tennis court by left"){
      player ! (Ball(-1))
      tennisMatch.expectMsg(EndGame(player))
    }

    it("Receives a ball out of the tennis court by right"){
      player ! (Ball(11))
      tennisMatch.expectMsg(EndGame(player))
    }

    it("Receives a winner ball by left"){
      player ! (Ball(0))
      tennisMatch.expectMsg(EndGame(self))
    }

    it("Receives a winner ball by right"){
      player ! (Ball(10))
      tennisMatch.expectMsg(EndGame(self))
    }

    it("Receives a ball to continue the game"){
      player ! (Ball(4))
      expectMsg(Ball(random))
    }
  }

}
