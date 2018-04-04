package nltk

class POSTagger(toks: Tokenizer) {

  // Map Penn tags to a simpler set
  // Input: String - Penn Tag
  // Output: String - Simple Tag
  def simplify(penn_tag: String): String = penn_tag match {
    // One of the set {Adjective, Adverb, Conjunction, Determiner, Noun, Number, Preposition, Verb, Beginning}
    case "CC" => "Conjunction"
    case "CD" => "Number"
    case "DT" => "Determiner"
    case "EX" => "Adverb"
    case "FW" => "Noun" // Foreigh Word mapped arbitrarily
    case "IN" => "Preposition"
    case "JJ" => "Adjective"
    case "JJR" => "Adjective"
    case "LS" => "Adjective"
    case "MD" => "Adverb"
    case "NN" => "Noun"
    case "NNP" => "Noun"
    case "NNPS" => "Noun"
    case "NNS" => "Noun"
    case "PDT" => "Adjective"
    case "POS" => "Verb"
    case "PRP" => "Noun"
    case "PRP$" => "Adjective"
    case "RB" => "Adverb"
    case "RBR" => "Adverb"
    case "RBS" => "Adverb"
    case "RP" => "Noun"
    case "SYM" => "Determiner"
    case "TO" => "Preposition"
    case "UH" => "Determiner"
    case "VB" => "Verb"
    case "VBD" => "Verb"
    case "VBG" => "Verb"
    case "VBN" => "Verb"
    case "VBP" => "Verb"
    case "VBZ" => "Verb"
    case "WDT" => "Determiner"
    case "WP" => "Noun"
    case "WP$" => "Adjective"
    case "WRB" => "Determiner"
    case _ => "Beginning" // Mistagged mapped to beginning of utterance
  }

  // Input: The filename of the testing or training file
  // Output: 2-tuple:
  //         List[String] of words in the training set
  //         List[String] of their associated POS tags
  def reformat_file(filename: String): (List[String], List[String]) = {

    def get_penn(line: String): String = {
      try { line.split(" ")(1).trim }
      catch { case e: Exception => "BLANK" }
    }
    import scala.io.Source
    val file = Source.fromResource(filename)
    val lines = file.getLines.toList
    val words = lines.map(_.split(" ")(0))
    val penn = lines map get_penn
    val tags = penn map simplify
    (words, tags)
  }

  // Training method for the POS Tagger
  // Input: filename of the corpus
  // Output: Map[String,Map[String,String]]
  //         Word -> Prev-POS -> This-POS
  def train(filename: String): Map[String,Map[String,String]] = {

    // Input: String - A single word in the corpus
    // Output: List[(String, String)] List of pairs:
    //              (prev-word-tag, this-word-tag)
    // Possible extension: most common following tag, for new words
    def tag_distinct(distinct_word: String, tagged_words: List[(String, String)]): Map[String,String] = {
      
      // get all indices of this specific word
      val (_, indices) = tagged_words.zipWithIndex
        .filter(_._1._1 == distinct_word)
        .unzip
       
      // Get all tags for this specific word
      val (_, distinct_tags) = indices.map(tagged_words.apply(_)).unzip
      /*
      // Get indices of previous words
      // TODO Handle beginning of file/utterance so that it is the default
      val prev_indices = indices map (_ - 1) filter (_ >= 0)

      // Get tags of previous word
      val (_, prev_tags) = prev_indices.map(tagged_words.apply(_)).unzip

      // Make tuples of prev_tags -> this_tags
      val tag_pairs = prev_tags zip distinct_tags

      //val distinct_prevs = prev_tags.distinct

      val this_tag_matches = (for (i <- 0 until distinct_prevs.size) yield {
        val tag_matches = tag_pairs filter (_._2 == distinct_prevs(i))
        if (tag_matches == List()) "Noun" else { // TODO handle error more gracefully
          tag_matches.groupBy(identity).maxBy(_._2.size)._1._2
        }
      }).toList

      val prev_tag_map: Map[String, String] = (distinct_prevs zip this_tag_matches).toMap
       */
      // Get the most common tag to use as a default
      val most_common_tag: String = distinct_tags.groupBy(identity).maxBy(_._2.size)._1
      /*
      val default_pair: (String, String) = ("Default", most_common_tag)
      prev_tag_map + default_pair
       */
      Map("Default" -> most_common_tag)
    }

    val (words, tags) = reformat_file(filename)
    val distinct_words = words.distinct

    distinct_words zip (distinct_words map (tag_distinct(_, (words zip tags)))) toMap
  }

  // Tests our model
  // Input: filename of the test data (test-pos.txt default)
  def test(filename: String = "test-pos.txt"): Unit = {
    val (words, tags) = reformat_file(filename)

    var prev: String = "Beginning"
    val model_tags: List[String] = (for (i <- 0 until words.size) yield {
      val tag: String = tag_word(words(i), prev, tag_map)
      prev = tag
      tag
    }).toList

    val correct_list: List[Int] = (for (i <- 0 until model_tags.size) yield {
      if (model_tags(i) == tags(i)) 1 else 0
    }).toList

    val correct: Int = correct_list.sum
    val tag_count = model_tags.size
    val percent = correct.toDouble / model_tags.size.toDouble * 100.0

    println(f"Got $correct of $tag_count, $percent percent!")
  }

  // Tag a specific word according to our model
  // Input: word - The raw word
  // Input: model - The local POS model to use
  // Input: prev_tag - The tag of the previous word (if availalble)
  // Output: String - The tag for the word
  def tag_word(word: String, prev_tag: String,  model: Model): String = {
    if (model contains word) {
      if (model(word) contains prev_tag) {
        return model(word)(prev_tag)
      } else {
        return model(word)("Default")
      }
    } else {
      return "Noun" // TODO make a better failure condition
    }
  }

  // Tag a list of token words according to our model
  // Input: s - list of words to tag
  // Output: List[String] - list of tags
  def tag_tokens(words: List[String]): List[String] = {
    var prev: String = "Beginning"
    val model_tags: List[String] = (for (i <- 0 until words.size) yield {
      val tag: String = tag_word(words(i), prev, tag_map)
      prev = tag
      tag
    }).toList
    model_tags
  }

  type Model = Map[String, Map[String,String]]

  val tag_map: Model = train("train-pos.txt")

  val s = toks.tokens

  val tags = tag_tokens(s)
}
