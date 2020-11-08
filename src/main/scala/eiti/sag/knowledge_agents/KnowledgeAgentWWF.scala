package eiti.sag.knowledge_agents

import java.io.{BufferedWriter, File, FileNotFoundException, FileWriter}

import akka.actor.ReceiveTimeout

import scala.concurrent.duration._
import eiti.sag.HttpServer.Kaboom
import eiti.sag.knowledge_agents.KnowledgeAgent.{FetchedAlreadyLearnedAnimals, LearnAbout}
import eiti.sag.query.{QueryType, UsersQueryInstance}
import net.ruippeixotog.scalascraper.browser.JsoupBrowser
import org.jsoup.Jsoup

class KnowledgeAgentWWF extends KnowledgeAgent {

  val learned_animalsFile = "wwf/learned_animals"
  val bag_of_wordsFile = "wwf/bag_of_words"
  val nerFile = "wwf/ner"
  val pos_ngramsFile = "wwf/pos_ngrams"
  val sentencesFile = "wwf/sentences"
  val lemmaSentencesFile = "wwf/lemma_sentences"
  val chunkerFile = "wwf/chunker"
  val tablesFile = "wwf/tables"
  val baseUrl = "https://www.worldwildlife.org/species/"

  def learn(animal : String): Unit ={
    log.info("WWF learning about " + animal)
    val animalUrl = baseUrl + animal
    if (checkUrlExists(animalUrl)) {
      getTables(animalUrl, animal)
      learnAbout(animalUrl, animal, bag_of_wordsFile, nerFile, pos_ngramsFile, sentencesFile, lemmaSentencesFile, chunkerFile)

      animalsLearnedAbout = animal :: animalsLearnedAbout
      persistAnimalsLearnedAbout(animalsLearnedAbout, learned_animalsFile)
      log.info("WWF finished learning " + animal)
    } else { log.info("Cannot find info about " + animal)}
  }

  override def receive = {
    case Kaboom => kaboom()
    case FetchedAlreadyLearnedAnimals() => fetchAlreadLearnedAnimals(learned_animalsFile)

    case LearnAbout(animal: String) =>
      if(animalsLearnedAbout.contains(animal) == false) {
        try {
          learn(animal)
          context.setReceiveTimeout(1 minute)
        } catch {
          case t: Throwable =>
            animalsLearnedAbout = animal :: animalsLearnedAbout
            persistAnimalsLearnedAbout(animalsLearnedAbout, learned_animalsFile)
            throw t
        }
        animalsLearnedAbout = animal :: animalsLearnedAbout
        persistAnimalsLearnedAbout(animalsLearnedAbout, learned_animalsFile)
      }

      askForAnimalToLearnAbout()

    case usersQueryInstance: UsersQueryInstance =>
      if (!animalsLearnedAbout.contains(usersQueryInstance.animal)) {
        ("WWF - I don't know anything about this animal. Let me learn.")
        learn(usersQueryInstance.animal)
      }

      usersQueryInstance.parsedType match {
        case QueryType.General =>
          try {chooseTableData(usersQueryInstance,tablesFile)}
          catch { case _ => log.info("cannot read table file")
            sendAnswer(usersQueryInstance, "Sorry, cant answer", -1)}
        case QueryType.Location => searchKnowledgeAndSendAnswer(usersQueryInstance, nerFile)
        case _ =>
          try{ val full_sent = findSentence(usersQueryInstance.mainWords,usersQueryInstance.animal,lemmaSentencesFile,sentencesFile,usersQueryInstance)}
          catch { case _ => log.warning("Cannot find sentence")
            sendAnswer(usersQueryInstance, "Sorry, cant answer", -1)}
      }

      log.info("WWF is done")
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

    var tableText=doc.select("div [class=container]")
    var tableText2=doc.select("[class=hdr]")
    try{
      for(i <- 0 to 12)
      {
        if(tableText2.get(i).text()=="Status")bw.write("Status;"+tableText.get(i).text()+"\n")
        if(tableText2.get(i).text()=="Population")bw.write("Population;"+tableText.get(i).text()+"\n")
        if(tableText2.get(i).text()=="Scientific Name")bw.write("Scientific Name;"+tableText.get(i).text()+"\n")
        if(tableText2.get(i).text()=="Weight")bw.write("Weight;"+tableText.get(i).text()+"\n")
        if(tableText2.get(i).text()=="Length")bw.write("Length;"+tableText.get(i).text()+"\n")
        if(tableText2.get(i).text()=="Habitats")bw.write("Habitats;"+tableText.get(i).text()+"\n")
        if(tableText2.get(i).text()=="Height")bw.write("Height;"+tableText.get(i).text()+"\n")
      }
    }catch{
      case e: IndexOutOfBoundsException => {bw.write("brak wyniku")
        bw.close()
        return "brak wyniku"}
    }
    bw.close()

  }
}
