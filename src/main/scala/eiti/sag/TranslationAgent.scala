package eiti.sag

import java.io.{BufferedInputStream, FileInputStream}

import akka.actor.{Actor, ActorRef, ActorSelection, ActorSystem, Identify, Kill, PoisonPill, Props, ReceiveTimeout, Terminated}
import akka.event.Logging
import eiti.sag.TranslationAgent.{Initial, SingleWordInSentence, TaggedQuery}
import eiti.sag.query.KnownPosTags.KnownPosTags

import scala.concurrent.duration._
import scala.language.postfixOps
import scala.concurrent.Await
import eiti.sag.query._
import opennlp.tools.postag.{POSModel, POSTaggerME}
import opennlp.tools.tokenize.WhitespaceTokenizer
import java.util.ArrayList

import eiti.sag.AnswerAgent.AwaitForAnswer
import eiti.sag.knowledge_agents.KnowledgeAgent.LearnAbout
import opennlp.tools.lemmatizer.DictionaryLemmatizer

import scala.collection.JavaConverters._
import scala.io.Source

class TranslationAgent extends Actor {
  val log = Logging(context.system, this)

  val lexiconFileName = "database/en-pos-maxent.bin"
  context.setReceiveTimeout(2 minutes)
  val model = new POSModel(new BufferedInputStream(new FileInputStream(lexiconFileName)))
  val tagger = new POSTaggerME(model)

  var stopwords: List[String] = List()

  def mainMenu(): Unit = {
    println("What would you like me to do?")
    println("1 - Learn")
    println("2 - Answer my question")
    println("3 - Close")
    var task = scala.io.StdIn.readLine()
    task match {
      case "1" => self ! "askToLearn"
      case "2" => self ! "askAboutAnimal"
      case "3" =>
        context.system.actorSelection("/user/*") ! Kill
        sys.exit()
      case _ =>
        println("Please type 1, 2 or 3")
        mainMenu()
    }
  }

  // Get animal name from user
  def askExplore(): String = {
    var animal = ""
    while (animal.isEmpty){
      println("Which animal are you interested in?")
      animal = scala.io.StdIn.readLine()
    }
    return animal
  }

  def getQuestion(animal:String): String = {
    var infoType = ""
    while (infoType.isEmpty){
      println("What do you want to know about " + animal + "?")
      infoType = scala.io.StdIn.readLine()
    }
    println("Okay. Looking for " + infoType)
    return infoType
  }

  def getQuestionType(question: String): QueryType.Value = {

    for (word <- question.split(" +")){
      for ((k,v) <- QueryMap.keywordListToQueryTypeMap) if(k.contains(word)) return v
    }
    return QueryType.None
  }

  def tag(text: String) = {
    // znaczenie tag'ów: http://paula.petcu.tm.ro/init/default/post/opennlp-part-of-speech-tags
    // nie dzielimy na zdania, bo zakładamy, że zapytanie użytkownika jest już pojedynczynym zdaniem
    val whitespaceTokenizerLine: Array[String] = WhitespaceTokenizer.INSTANCE.tokenize(text)
    val tags: Array[String] = tagger.tag(whitespaceTokenizerLine)
    val tokenizedSentence = (tags zip whitespaceTokenizerLine).zipWithIndex map {
      case ((tag: String, word: String), index: Int) => SingleWordInSentence(word, tag, KnownPosTags.fromString(tag), index)
    }

    TaggedQuery(tokenizedSentence)
  }

  def getMainWord(tag:TaggedQuery,question:String):List[(String,String)] = {
    val whitespaceTokenizerLine: Array[String] = WhitespaceTokenizer.INSTANCE.tokenize(question)
    var foundWord = new ArrayList[(String,String)]
    whitespaceTokenizerLine(0) match {
      case ("what" | "which") =>
        if ( whitespaceTokenizerLine.length > 1){
          whitespaceTokenizerLine(1) match {
            case "is" | "are" =>
              log.info("looking for noun")
              for (word <- tag.sentence.filter(!stopwords.contains(_)))
                word.posRaw match {
                  case "NN" | "NNS" | "NNP" | "NNSP" => foundWord.add((word.word.replaceAll("[\\?\\!,.]",""),word.posRaw))
                  case _ => for (word <- tag.sentence.filter(!stopwords.contains(_))) foundWord.add((word.word.replaceAll("[\\?\\!,.]",""),word.posRaw))
                }
            case "do" | "does" | "did" =>
              log.info("looking for verb")
              for (word <- tag.sentence.filter(!stopwords.contains(_))){
                log.info(word.toString)
                word.posRaw match {
                  case "VB" | "VBD" | "VBG" | "VBN" | "VBP" | "VBZ" => foundWord.add((word.word.replaceAll("[\\?\\!,.]",""), word.posRaw))
                  case _ => for (word <- tag.sentence.filter(!stopwords.contains(_))) foundWord.add((word.word.replaceAll("[\\?\\!,.]",""),word.posRaw))
                }
              }
            case _ => for (word <- tag.sentence.filter(!stopwords.contains(_))) foundWord.add((word.word.replaceAll("[\\?\\!,.]",""),word.posRaw))
          }
        } else {
          println("Can you be more specific?")
          self ! askAboutAnimals()
        }
      case word =>
        for (word <- tag.sentence.filter(!stopwords.contains(_))) foundWord.add((word.word.replaceAll("[\\?\\!,.]",""),word.posRaw))
      case _ =>
        println("Can you be more specific?")
        self ! askAboutAnimals()
    }
    for(i <- foundWord.asScala.toList) log.info(i.toString())
    return foundWord.asScala.toList
  }

  def getMainLemmas(tag:TaggedQuery,question:String):Array[String] = {
    val mainWords = getMainWord(tag,question)
    val bis = new BufferedInputStream(new FileInputStream("database/en-lemmatizer.dict"))
    val lemmaModel = new DictionaryLemmatizer(bis)
    var tokens = new ArrayList[String]
    var postags = new ArrayList[String]
    for (word <- mainWords) {tokens.add(word._1); postags.add(word._2)}
    val lemma = lemmaModel.lemmatize(tokens.asScala.toArray, postags.asScala.toArray)
    for ((lemmaWord,i) <- lemma.zipWithIndex){
      if (lemmaWord == "O") lemma(i) = tokens.asScala.toList(i)
    }
    val lemmaNoStop = lemma.filter(!stopwords.contains(_))
    for(i <- lemmaNoStop) log.info(i)
    return lemmaNoStop
  }

  // Choose one Agent with name matching pattern
  def choseOneAgent(patternName:String): ActorRef = {
    var test = context.actorSelection("akka://AnimalsKnowledgeBase/user/" + patternName + "*").resolveOne(5 second)
    var agent = Await.result(test,15 second)
    return agent
  }

  // Choose all Agents with name matching pattern
  def choseAllAgents(patternName:String): ActorSelection = {
    var agent = context.actorSelection("akka://AnimalsKnowledgeBase/user/" + patternName + "*")
    return agent
  }

  def askToLearn(): Unit = {
    var animals = askExplore().toLowerCase
    val animalsList: List[String] = animals.replaceAll(" ","").split(",").toList
    for (elem <- animalsList) {
      choseOneAgent("KnowledgeAgentsSupervisor") ! LearnAbout(elem)
    }

    choseOneAgent("MetaKnowledge") ! "fetch"
  }

  def askAboutAnimals() = {
    val animal = askExplore().toLowerCase
    val question = getQuestion(animal).replaceAll("[\\?\\!,.]","")
    val questionType = getQuestionType(question)
    val tagged = tag(question)
    val mainWords = getMainLemmas(tagged,question)

    if(questionType == null){
      log.info("Cannot resolve question type")
    }

    val q = UsersQueryInstance(animal, questionType, tagged, mainWords, animal)
    context.actorSelection("akka://AnimalsKnowledgeBase/user/KnowledgeAgentsSupervisor") ! q
    context.actorSelection("akka://AnimalsKnowledgeBase/user/AnswerAgent") ! AwaitForAnswer(q)
  }

  def getStopWords() = {
    val lines = Source.fromFile("database/stopwords").mkString.split("\n").filter(p => p.isEmpty == false)
    stopwords = lines.map(line => line.trim).toList
  }

  // Receive Message cases
  def receive = {
    case Initial() => getStopWords()
    case "mainMenu" => mainMenu()
    case "askAboutAnimal" => askAboutAnimals()
    case "askToLearn" => askToLearn()
    case ReceiveTimeout =>
      println("I was waiting sooo long. Let's try again.")
      mainMenu()
    case _ => log.info("received unknown message")
  }

  override def postRestart(reason: Throwable): Unit = {
    log.warning("Translation Agent restarted")
    println("Sorry, I was dead. Let's start again")
    self ! "mainMenu"
    //self ! LearnAbout("white shark")
  }
}

object TranslationAgent {
  case class SingleWordInSentence(word: String, posRaw: String, pos: KnownPosTags, index: Int)
  final case class Initial()

  case class TaggedQuery(sentence: Array[SingleWordInSentence]) {
    assert(sentence.map(word => word.index).sorted sameElements  sentence.map(w => w.index))

  }
}