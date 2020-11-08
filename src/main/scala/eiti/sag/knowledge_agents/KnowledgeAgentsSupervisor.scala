package eiti.sag.knowledge_agents

import scala.concurrent.duration._
import akka.actor.SupervisorStrategy.{Escalate, Restart, Resume, Stop}
import akka.actor.{Actor, ActorRef, ActorSystem, OneForOneStrategy, PoisonPill, Props, SupervisorStrategy}
import akka.event.Logging
import eiti.sag.HttpServer.Kaboom
import eiti.sag.MainApp
import eiti.sag.knowledge_agents.KnowledgeAgent.{FetchedAlreadyLearnedAnimals, LearnAbout}
import eiti.sag.knowledge_agents.KnowledgeAgentsSupervisor.{InitAgents, KillAgent, StartLearning}
import eiti.sag.query.UsersQueryInstance

import scala.util.Random

class KnowledgeAgentsSupervisor extends Actor {

  var knowledgeAgentMap: List[ActorRef] = List()
  val log = Logging(context.system, this)

  def killSomeAgentAtRandom(): Unit = {
    val chosenAgent = knowledgeAgentMap(new Random().nextInt(knowledgeAgentMap.size))
    println("Sending Kaboom to: " + chosenAgent.path)
    chosenAgent ! Kaboom
  }

  override def receive: Receive = {
    case StartLearning() => startLearning()
    case LearnAbout(animal) => knowledgeAgentMap.foreach((a) => a ! LearnAbout(animal))
    case q: UsersQueryInstance => askAQuestion(q)
    case InitAgents() => initAgents()
    case Kaboom => killSomeAgentAtRandom()
    case _ => log.info("Supervisor - unknown message received")
  }

  def initAgents(): Unit = {

    val KnowledgeAgentAFS = context.system.actorOf(Props[KnowledgeAgentAFS], name = "KnowledgeAgentAFS")
    val KnowledgeAgentWikipedia = context.system.actorOf(Props[KnowledgeAgentWikipedia], name = "KnowledgeAgentWikipedia")
    val KnowledgeAgentWWF = context.system.actorOf(Props[KnowledgeAgentWWF], name = "KnowledgeAgentWWF")

    knowledgeAgentMap = KnowledgeAgentAFS :: knowledgeAgentMap
    knowledgeAgentMap = KnowledgeAgentWikipedia :: knowledgeAgentMap
    knowledgeAgentMap = KnowledgeAgentWWF :: knowledgeAgentMap

    for (elem <- knowledgeAgentMap) {
      elem ! FetchedAlreadyLearnedAnimals()
    }
  }

  def startLearning(): Unit = {
    val KnowledgeAgentAFS = context.system. actorSelection("akka://AnimalsKnowledgeBase/user/KnowledgeAgentAFS")
    val KnowledgeAgentWikipedia = context.system.actorSelection("akka://AnimalsKnowledgeBase/user/KnowledgeAgentWikipedia")
    val KnowledgeAgentWWF = context.system.actorSelection("akka://AnimalsKnowledgeBase/user/KnowledgeAgentWWF")

    KnowledgeAgentAFS ! LearnAbout("giraffe")
    KnowledgeAgentWikipedia ! LearnAbout("giraffe")
    KnowledgeAgentWWF ! LearnAbout("giraffe")
  }

  def askAQuestion(userQuery: UsersQueryInstance): Unit = {
    for (elem <- knowledgeAgentMap) {
      elem ! userQuery
    }
  }

  override val supervisorStrategy =
    OneForOneStrategy(maxNrOfRetries = 10, withinTimeRange = 1 minute) {
      case _ => Restart
    }
}

object KnowledgeAgentsSupervisor {

  val AgentName: String = "KnowledgeAgentsSupervisor"
  val FullAgentPath: String = "akka://" + MainApp.AnimalsKnowledgeSystemName +  "/user/" + AgentName

  final case class StartLearning()
  final case class InitAgents()
  final case class KillAgent(name: String)
}
