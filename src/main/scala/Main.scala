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

  //val input_file = "test-rep.txt"
  //val senten = new nltk.Sentenceizer(input_file)
  //val tokens = new nltk.Tokenizer(senten)
  //val tagger = new nltk.POSTagger(tokens)
  //tagger.test("test-pos.txt")

  val cp = new nltk.CrestParse()
  println(cp.corpus_map)
  val repairable_sentences = List()
  //val repairable_sentences = cp.find_repairable_utterances()
  for (i <- repairable_sentences) println(i)
}
