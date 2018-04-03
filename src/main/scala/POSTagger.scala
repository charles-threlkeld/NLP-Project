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
    def tag_distinct(distinct_word: String, tagged_words: List[(String, String)]): Map[String,String] = {

      // get all indices of this specific word
      val (_, indices) = tagged_words.zipWithIndex
        .filter(_._1._1 == distinct_word)
        .unzip
      // Get all tags for this specific word
      val (_, distinct_tags) = indices.map(tagged_words.apply(_)).unzip

      // Get indices of previous words
      // TODO Handle beginning of file/utterance so that it is the default
      val prev_indices = indices map (_ - 1)
      // Get tags of previous word
      val (_, prev_tags) = prev_indices.map(tagged_words.apply(_)).unzip

      // Make tuples of prev_tags -> this_tags
      val tag_pairs = prev_tags zip distinct_tags

      val distinct_prevs = prev_tags.distinct

      for (i <- 0 until distinct_prevs.size) yield {
        tag_pairs filter (_._2 == distinct_prevs(i))
      }



      val most_common_tag = distinct_tags.groupBy(identity).maxBy(_._2.size)._1

      if(distinct_tags.forall(x => x == distinct_tags(0))) {
        // If a word only has one tag
        Map("DEFAULT" -> distinct_tags(0))
      } else {
        // get the previous word's tag
        // send back a map of the most common prev-tag to this-tag pairs
        // TODO Handle beginning of a sentence

        val (_, indices) = tagged_words.zipWithIndex
          .filter(_._1._1 == distinct_word)
          .unzip
        val prev_indices = indices map (_ - 1)

        val prev_tags = prev_indices map (tagged_words(_)._2)

        val tag_pair = prev_tags zip distinct_tags

        val x = distinct_tags.groupBy(identity).maxBy(_._2.size)._1

        Map("VERB" -> x,
          "NOUN" -> x,
          "OTHER" -> x)
      }
    }

    val (words, tags) = reformat_file(filename)
    val distinct_words = words.distinct

    distinct_words zip (distinct_words map (tag_distinct(_, (words zip tags)))) toMap
  }

  def test(filename: String): Unit = {
    val model = train("train.txt")

    val (words, tags) = reformat_file("test.txt")

  }

  val tag_map = train("train-pos.txt")

  val s = toks.tokens

  val tags = s.map(tag_map(_))
}
