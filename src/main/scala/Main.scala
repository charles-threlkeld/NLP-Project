object Main extends App {

  // Pipeline Idea:
  // Tokenize input
  // -> POS tagging
  // -> Break into sentences
  // -> Tag stop-words or phrases ("I mean", "that is", "actually"...)
  // -> greedy re-parse sentence

  // Example:
  // I would like a blue coat I mean a red coat
  // I/Pronoun would/Verb like/Verb a/Determiner blue/Adjective coat/Noun I/Pronoun mean/Verb a/Determiner red/Adjective coat/Noun
  //"I mean" -> stop word
  // Tuple(List(Pronoun,Verb,Verb,Determiner,Adjective,Noun),List(Determiner,Adjective,Noun))
  // List("I", "would", "like", "a", "red", "coat")
  // String: "I would like a red coat"

  //val rawSentences = {
  //  import scala.io.Source
  //  val filename = "test-rep.txt"
  //  val file: Source = Source.fromResources(filename)
  //  val rawText: String = file.getLines.mkString
  //  rawText.split(Array('\n', '.', '?', '!'))
  //}
  //
  //val senten = new nltk.Sentenceizer(input_file)
  //val tokens = new nltk.Tokenizer(senten)
  //val tagger = new nltk.POSTagger(tokens)
  //tagger.test("test-pos.txt")

  val rep = new nltk.Repair("train.txt")
  println("Baseline Dice Score for Training Set: " + rep.base)
  println("Training Set Dice Score after Repair: " + rep.repScore)

  val rep_test = new nltk.Repair("test.txt")
  println("Baseline Dice Score for Testing Set: " + rep_test.base)
  println("Testing Set Dice Score after Repair: " + rep_test.repScore)

  //val cp = new nltk.CrestParse()
  //val ttg = new nltk.TrainTestGen()
  //ttg.generate()

  //val repairable_sentences = cp.find_repairable_utterances()
  //for (i <- repairable_sentences) println(i)
}
