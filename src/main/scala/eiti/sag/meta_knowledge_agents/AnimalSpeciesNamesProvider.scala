package eiti.sag.meta_knowledge_agents

import akka.actor.Actor
import akka.event.Logging
import eiti.sag.knowledge_agents.KnowledgeAgent.LearnAbout
import eiti.sag.meta_knowledge_agents.MetaKnowledgeAgentsSupervisor.FindAnimalSpeciesToLearn
import net.ruippeixotog.scalascraper.browser.JsoupBrowser
import org.jsoup.Jsoup
import org.jsoup.nodes.{Element, Node, TextNode}
import org.jsoup.select.NodeVisitor

class AnimalSpeciesNamesProvider extends Actor {

  var animalList: List[String] = List()
  val log = Logging(context.system, this)

  def doFetchAnimalNames(): Unit = {

    val url = "https://a-z-animals.com/animals/";
    val html = JsoupBrowser().get(url)
    val doc = Jsoup.parse(html.toString)

    doc.traverse(new MyNodeVisitor())

    animalList = animalList ++ animalList.flatten(s => s.split(" ").toList).filter(s => s.isEmpty == false)
  }

  class MyNodeVisitor extends NodeVisitor {
    override def tail(node: Node, depth: Int): Unit = {}

    override def head(node: Node, depth: Int): Unit = {
      if(node.isInstanceOf[TextNode]) {

        val p = node.parent()
        val isok = p.asInstanceOf[Element].tagName().equals("b") &&
          p.parent().asInstanceOf[Element].tagName().equals("a") &&
          p.parent().parent().asInstanceOf[Element].tagName().equals("li") &&
          p.parent().parent().asInstanceOf[Element].attr("class").contains("az-phobia-link")

        if(isok) {
          animalList = node.asInstanceOf[TextNode].getWholeText :: animalList
        }
      }
    }
  }

  override def receive: Receive = {
    case msg: FindAnimalSpeciesToLearn =>
      if(animalList.isEmpty) self ! msg
      else {
        val animals = animalList.filter(a => msg.animalsLearnedAbout.contains(a) == false)
        if(animals.isEmpty) {
          log.info("Need to crawl more species names")
        } else {
          msg.sendTo ! LearnAbout(animals.head)
        }
      }
    case "fetch" =>
      doFetchAnimalNames()
    case _ => log.info("Dont understand message")
  }
}
