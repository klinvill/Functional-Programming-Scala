import java.io.Serializable
import forcomp.Anagrams.Occurrences
import forcomp.Anagrams.dictionaryByOccurrences
import forcomp.Anagrams.Sentence
import forcomp.Anagrams.sentenceOccurrences
import forcomp.Anagrams.combinations
import forcomp.Anagrams.Word
import forcomp.Anagrams.wordOccurrences
import forcomp.Anagrams.subtract

dictionaryByOccurrences

def sentenceAnagrams(sentence: Sentence): List[Sentence] = {
  val occurs = sentenceOccurrences(sentence)
  val combos = combinations(occurs)
  val allWords = (for (combo <- combos) yield dictionaryByOccurrences.get(combo) match {
    case Some(words) => words
    case None => Nil
  }).filter(words => words.length >= 1).flatten
  sentenceAnagramsHelper(allWords, occurs, Nil).map((s)=>s.permutations.toList).flatten
}

def sentenceAnagramsHelper(words: List[Word], occurrences: Occurrences, acc: Sentence): List[Sentence] = {
  // found an anagram! Return the accumulated list of words
  if (occurrences.isEmpty) List(acc)

  // no more anagrams can be found here
  else if (words.isEmpty) List()

  // the word could be part of an anagram
  // 2 cases: pick the word, and don't pick the word
  else if (canMatch(words.head, occurrences)) {
    // pick the word
    val newOccurrences = subtract(occurrences, wordOccurrences(words.head))
    val picked_res = sentenceAnagramsHelper(words.tail, newOccurrences, words.head::acc)
    // don't pick the word
    val not_picked_res = sentenceAnagramsHelper(words.tail, occurrences, acc)
    picked_res++not_picked_res
  }

  // the word doesn't fit into the remaining occurrences, skip it and continue
  else sentenceAnagramsHelper(words.tail, occurrences, acc)
}


/*def sentenceAnagramsHelper(words: List[Word], occurrences: Occurrences): List[Sentence] = words match {
  case List() => List()
  case (w: Word)::(ws: List[Word]) => {
    if (occurrences.isEmpty) List()
    else if (canMatch(w, occurrences))
        (sentenceAnagramsHelper(ws, subtract(occurrences, wordOccurrences(w))).map((s)=>w::s)) ++ sentenceAnagramsHelper(ws, occurrences)
    else sentenceAnagramsHelper(ws, occurrences)
  }
}*/

def canMatch(word: Word, occurrences: Occurrences): Boolean = {
  val mappedOccurences = occurrences.toMap
  wordOccurrences(word).forall { case (char: Char, freq: Int) => mappedOccurences.get(char) match {
    case Some(freq2) => freq <= freq2
    case None => false
  }}
}

def wordCombinations (words: Sentence): List[Sentence] = {
  words.permutations.toList
}

val sentence = List("Linux", "rulez")
val sentence2 = List("I", "love", "you")
wordCombinations(sentence2)
val out = sentenceAnagrams(sentence)
val test = List("nil", "Zulu", "Rex")
val out_occur = sentenceOccurrences(sentence)
sentenceAnagramsHelper(test, out_occur, List())
out.contains("nil")
out.contains("Zulu")
out.contains("Rex")
canMatch("nil", out_occur)
canMatch("Zulu", out_occur)
canMatch("Rex", out_occur)
(for (i <- 1 until 10) yield i).toList