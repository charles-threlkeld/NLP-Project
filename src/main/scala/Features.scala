package nltk

object Features {

  type Feature = Sentenceizer => (Double, Sentenceizer)

  // Feature 1 checks to see if the utterance is only two tokens
  // If so, return the weight/tokens tuple
  // else, return an empty weight and the original utterance
  // Input: utterance, the repairable utterance
  // Output: Double - the weight for this feature output
  //         Sentenceizer - the this-feature repaired utterance
  private def f1(utterance: Sentenceizer): (Double, Sentenceizer) = {
    val tokens = new Tokenizer(utterance).tokens
    if (tokens.size == 2)
      (1.0, new Sentenceizer(Array(tokens(1))))
    else
      (0.0, utterance)
  }

  // Feature 2 checks for duplicated words within three tokens
  // E.g. what - what do you think?
  // E.g. can we - can we go in there?
  // If found, return the weight and repair
  // else return zero and the original
  // the positive case acts recursively to find more repairs, but does not change weight
  // Input: utterance - the repairable utterance
  // Output: Double - the weight for this feature output
  //         Sentenceizer - the this-feature repaired utterance
  private def f2(utterance: Sentenceizer): (Double, Sentenceizer) = {
    def helper(utterance: Sentenceizer, weight: Double): (Double, Sentenceizer) = {
      val tokens = new Tokenizer(utterance).tokens

      val repairIndices: List[(Int, Int)] = (for(i <- 0 to tokens.length) yield {
        val upperBound = if (tokens.length - 1 < i + 4)
          tokens.length - 1
        else
          i + 4
        val repairs = (for (j <- i + 1 to upperBound) yield {
          if (tokens(i) == tokens(j))
            (i,j)
          else
            null
        }).filter(_ != null)
        if (repairs.isEmpty)
          null
        else
          (-1,-1)
      }).toList.filter(_ != null)

      if (repairIndices.isEmpty)
        (weight, utterance)
      else {
        val i = repairIndices(0)._1
        val j = repairIndices(0)._2
        val repairTokens = tokens.slice(0, i) ++ tokens.slice(j, tokens.length)
        val utt = repairTokens mkString " "
        helper(new Sentenceizer(Array(utt)), 1.0)
      }
    }
    helper(utterance, 0.0)
  }

  val featureList: List[Feature] = List(f1, f2)

}
