package eiti.sag.knowledge_agents

import java.io.{BufferedWriter, File, FileWriter}

import eiti.sag.HttpServer.Kaboom
import eiti.sag.knowledge_agents.KnowledgeAgent.{FetchedAlreadyLearnedAnimals, LearnAbout}
import eiti.sag.query.{QueryType, UsersQueryInstance}
import akka.actor.ReceiveTimeout
import net.ruippeixotog.scalascraper.browser.JsoupBrowser
import org.jsoup.Jsoup

import scala.concurrent.Await
import eiti.sag.meta_knowledge_agents.MetaKnowledgeAgentsSupervisor.AskForAnimalSpecies

import scala.concurrent.duration._


class KnowledgeAgentAFS extends KnowledgeAgent {

  val learned_animalsFile = "animal_facts_encyclopedia/learned_animals"
  val bag_of_wordsFile = "animal_facts_encyclopedia/bag_of_words"
  val nerFile = "animal_facts_encyclopedia/ner"
  val pos_ngramsFile = "animal_facts_encyclopedia/pos_ngrams"
  val sentencesFile = "animal_facts_encyclopedia/sentences"
  val lemmaSentencesFile = "animal_facts_encyclopedia/lemma_sentences"
  val chunkerFile = "animal_facts_encyclopedia/chunker"
  val tablesFile = "animal_facts_encyclopedia/tables"
  val baseUrl = "https://www.animalfactsencyclopedia.com/"

  def learn(animal :String): Unit = {
    log.info("AFS learning about " + animal)
    var animalUrl = ""
    if (animal.toLowerCase == "dog"){animalUrl = baseUrl + "All-About-Dogs.html"}
    else {animalUrl = baseUrl + animal.capitalize + "-facts.html"}

    log.info(animalUrl)
    if (checkUrlExists(animalUrl)) {

      getTables(animalUrl, animal)
      learnAbout(animalUrl, animal, bag_of_wordsFile, nerFile, pos_ngramsFile, sentencesFile, lemmaSentencesFile, chunkerFile)

      animalsLearnedAbout = animal :: animalsLearnedAbout
      persistAnimalsLearnedAbout(animalsLearnedAbout, learned_animalsFile)
      log.info("AFS finished learning about " + animal)
    } else { log.info("Cannot find info about " + animal)}
  }

  override def receive = {
    case Kaboom => kaboom()
    case FetchedAlreadyLearnedAnimals() => fetchAlreadLearnedAnimals(learned_animalsFile)
    case LearnAbout(animal: String) =>
      try {
        learn(animal)

        log.info("AFS learning about " + animal)
        val animalUrl = if (animal.toLowerCase == "dog")
          baseUrl + "All-About-Dogs.html"
        else baseUrl + animal.capitalize + "-facts.html"

      } catch {
        case t: Throwable =>
          animalsLearnedAbout = animal :: animalsLearnedAbout
          persistAnimalsLearnedAbout(animalsLearnedAbout, learned_animalsFile)
          throw t
      }
      animalsLearnedAbout = animal :: animalsLearnedAbout
      persistAnimalsLearnedAbout(animalsLearnedAbout, learned_animalsFile)
      askForAnimalToLearnAbout()

    case usersQueryInstance: UsersQueryInstance =>
      if (!animalsLearnedAbout.contains(usersQueryInstance.animal)) {
        println("AFS - I don't know anything about this animal. Let me learn.")
        learn(usersQueryInstance.animal)
      }

      usersQueryInstance.parsedType match {
        case QueryType.General =>
          try {chooseTableData(usersQueryInstance,tablesFile)}
          catch { case _ =>
            log.info("cannot read table file")
            sendAnswer(usersQueryInstance, "Sorry, cant answer", -1) }
        case QueryType.Location => searchKnowledgeAndSendAnswer(usersQueryInstance, nerFile)
        case _ =>
          try{ val full_sent = findSentence(usersQueryInstance.mainWords,usersQueryInstance.animal,lemmaSentencesFile,sentencesFile, usersQueryInstance)}
          catch {
            case _ => log.warning("Cannot find sentence")
            sendAnswer(usersQueryInstance, "Sorry, cant answer", -1)}
      }
      log.info("AFS is done")
      context.setReceiveTimeout(1 minute)

    case ReceiveTimeout ⇒
      log.info("Received timeout")

    case _      ⇒ log.info("received unknown message")
  }

  def getTables(pageTitle: String, animal:String): Unit = {
    val file = new File("animal_db/" + tablesFile + "/" + animal + ".txt")
    val bw = new BufferedWriter(new FileWriter(file))
    val url = pageTitle
    val html = JsoupBrowser().get(url)
    val doc = Jsoup.parse(html.toString)

    var table = doc.select("table[style*=text-align]")
    val rows = table.select("tr")

    var z = new Array[String](80)
    for (i <- 0 to 79)
      z(i)=""

    try{
      for(i <- 1 to rows.size-1)
      {
        val row = rows.get(i)
        for(j <- 0 to 3)
        {
          val cells = row.select("td")
          if(j<cells.size){
            val cell = cells.get(j).text()
            z((i-1)*4+j) = cell
          }
        }
      }

      for(l<-0 to 5){
        for(k<-0 to 3){
          bw.write(z(l*8+k) + knowledgeBaseSep + z(l*8+4+k) + "\n")
        }
      }
    }catch{
      case e: IndexOutOfBoundsException => {bw.close()
        return "brak wyniku"}
    }
    bw.close()
  }
}
