package nltk

// This class extracts the raw spoken text
// from a given file of the CReST Corpus
// It wraps them in a Sentencizer object
// which can then be part-of-speech tagged
// and given to the repair module

class CrestParse() {


  // Load a corpus file
  // Input: A filename (e.g. "Muri_07_S3_merged.xml")
  // Output: A list of the lines in the file
  def get_file(filename: String): List[String] = {

    def slicer(text: String, id: String): String = {
      val sliceBegin = text.indexOf(id) + id.length
      val sliceEnd = (text.slice(sliceBegin, text.length)).indexOf("\"")
      return text.slice(sliceBegin, sliceBegin + sliceEnd)
    }

    import scala.io.Source
    val file = Source.fromResource(filename)
    val lines = file.getLines.toList
    val tiers = lines.filter(_ contains "<tier ")
    val tierIds = tiers.map(slicer(_, "display-name="))
    


    file.getLines.toList.filter(_ contains "start=")
  }

  // Transform xml to text and timestamps
  // Input: a single line of xml (<event ...>Text</event>)
  // Output: (Text, Begin-time, End-time)
  def process_xml_lines(xml_list: String): (Int, Int, String) = {

    // Idea:
    // Each xml doc has the information embedded into tiers that are
    // individuated by the display-name ID, where S# is the number of the speaker within
    // the corpus
    // Tiers have these display-name IDs:
    // DirectorS3: utterances, e.g. "are you there?"
    // DirectorS3WORD: tokens, e.g. "are"
    // MemberS3: utterances, e.g. "can you hear me?"
    // MembeerS3WORD: tokens, e.g. "can"
    // POS: part-of-speech, e.g. "VBP"
    // Dependency: ??, e.g. "ROOT"
    // Constituency: utterance phrase, e.g. "NP"

    // '08 files include the following additional tiers:
    // Director_Disfluency: disfluency type, e.g. "FP-B"
    // Member_Disfluency: disfluency type, e.g. "SP-M"

    // '10 files include the above plus these additional tiers:
    // Director UH/UM: non-word disfluencies, e.g. "UM-B"
    // Searcher UH/UM: non-word disfluencies, e.g. "SP-B"

    // Each tier has a list of events that occured within that spec.
    // Each event has a start timestamp and an end timestamp
    // After gathering the indices of each tier start, we can segment the corpus
    // into event types, and distinct sublists according the the tiers
    // Then, we can tag the utterances by following the start/end tags as linked lists
    // E.g. an utterance may have start="T5" end="T10", so we find the token with
    // start="T5", and add it to a list.
    // Then check the token's end tag. If it is "T10", then stop adding tokens to the list
    // If it is not "T10", find the token that has start="T10" and recur.
    // After finding stop="T10", find the part-of-speech tags that have been tagged
    // for the tokens of the utterance.
    // Finally, also tag any disfluencies if they occur, in the appropriate
    // corpus files (i.e. '08 and '10 files)

    // Do this for each utterance in the tiers Director and Speaker

    val len = xml_list.length
    val firstTimestampStart = xml_list indexOf "T" + 1
    val firstTimestampEnd = xml_list.substring(firstTimestampStart, len) indexOf "\""
    val t1 = (xml_list.substring(firstTimestampStart, firstTimestampStart + firstTimestampEnd)).toInt

    val truncatedXml = xml_list.substring(firstTimestampStart + firstTimestampEnd, len)
    val len_trunc = truncatedXml.length
    val secondTimestampStart = (truncatedXml indexOf "T") + 1
    val secondTimestampEnd = truncatedXml.substring(firstTimestampStart, len) indexOf "\""
    val t2 = (truncatedXml.substring(secondTimestampStart, secondTimestampStart + secondTimestampEnd)).toInt

    val textBegin = (truncatedXml indexOf ">") + 1
    val textEnd = truncatedXml indexOf "<"
    val text = truncatedXml.substring(textBegin, textEnd)

    return (t1, t2, text)
  }

  val corpus = List("Muri_07_S3_merged.xml",
    "Muri_07_S4_merged.xml",
    "Muri_07_S5_merged.xml",
    "Muri_07_S6_merged.xml",
    "Muri_07_S7_merged.xml",
    "Muri_08_S1_merged.xml",
    "Muri_08_S2_merged.xml",
    "Muri_08_S3_merged.xml",
    "Muri_08_S4_merged.xml",
    "Muri_08_S5_merged.xml",
    "Muri_08_S7_merged.xml",
    "Muri_10_S2_merged.xml",
    "Muri_10_S3_merged.xml",
    "Muri_10_S4_merged.xml",
    "Muri_10_S5_merged.xml",
    "Muri_10_S6_merged.xml",
    "Muri_10_S7_merged.xml",
    "Muri_10_S8_merged.xml",
    "Muri_10_S9_merged.xml",
    "Muri_10_S10_merged.xml",
    "Muri_10_S11_merged.xml",
    "Muri_10_S1_merged.xml")

  val filename = "Muri_07_S3_merged.xml"
  val lines = get_file(filename)
  val processed_lines = lines map process_xml_lines
  // TODO: Match words to POS tags
  // Match word-tag tuples to utterances


}

