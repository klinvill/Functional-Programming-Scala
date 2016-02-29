package forcomp

import java.util.NoSuchElementException

import common._

import math.abs

object Anagrams {

  /** A word is simply a `String`. */
  type Word = String

  /** A sentence is a `List` of words. */
  type Sentence = List[Word]

  /** `Occurrences` is a `List` of pairs of characters and positive integers saying
   *  how often the character appears.
   *  This list is sorted alphabetically w.r.t. to the character in each pair.
   *  All characters in the occurrence list are lowercase.
   *
   *  Any list of pairs of lowercase characters and their frequency which is not sorted
   *  is **not** an occurrence list.
   *
   *  Note: If the frequency of some character is zero, then that character should not be
   *  in the list.
   */
  type Occurrences = List[(Char, Int)]

  /** The dictionary is simply a sequence of words.
   *  It is predefined and obtained as a sequence using the utility method `loadDictionary`.
   */
  val dictionary: List[Word] = loadDictionary

  /** Converts the word into its character occurrence list.
   *
   *  Note: the uppercase and lowercase version of the character are treated as the
   *  same character, and are represented as a lowercase character in the occurrence list.
   *
   *  Note: you must use `groupBy` to implement this method!
   */
  def wordOccurrences(w: Word): Occurrences = {
    val lower_list = w.toLowerCase().toList
    val mapped_freq = lower_list.groupBy((c: Char) => c).mapValues((cs: List[Char]) => cs.length)
    mapped_freq.toList.sorted
  }


  /** Converts a sentence into its character occurrence list. */
  def sentenceOccurrences(s: Sentence): Occurrences =
    wordOccurrences(s.mkString)

  /** The `dictionaryByOccurrences` is a `Map` from different occurrences to a sequence of all
   *  the words that have that occurrence count.
   *  This map serves as an easy way to obtain all the anagrams of a word given its occurrence list.
   *
   *  For example, the word "eat" has the following character occurrence list:
   *
   *     `List(('a', 1), ('e', 1), ('t', 1))`
   *
   *  Incidentally, so do the words "ate" and "tea".
   *
   *  This means that the `dictionaryByOccurrences` map will contain an entry:
   *
   *    List(('a', 1), ('e', 1), ('t', 1)) -> Seq("ate", "eat", "tea")
   *
   */
  lazy val dictionaryByOccurrences: Map[Occurrences, List[Word]] =
    dictionary.groupBy((w: Word) => wordOccurrences(w))

  /** Returns all the anagrams of a given word. */
  def wordAnagrams(word: Word): List[Word] = {
    val occurences = wordOccurrences(word)
    dictionaryByOccurrences.get(occurences) match {
      case Some(words) => words
      case None => throw new NoSuchElementException("Word not found in dictionary")
    }
  }

  /** Returns the list of all subsets of the occurrence list.
   *  This includes the occurrence itself, i.e. `List(('k', 1), ('o', 1))`
   *  is a subset of `List(('k', 1), ('o', 1))`.
   *  It also include the empty subset `List()`.
   *
   *  Example: the subsets of the occurrence list `List(('a', 2), ('b', 2))` are:
   *
   *    List(
   *      List(),
   *      List(('a', 1)),
   *      List(('a', 2)),
   *      List(('b', 1)),
   *      List(('a', 1), ('b', 1)),
   *      List(('a', 2), ('b', 1)),
   *      List(('b', 2)),
   *      List(('a', 1), ('b', 2)),
   *      List(('a', 2), ('b', 2))
   *    )
   *
   *  Note that the order of the occurrence list subsets does not matter -- the subsets
   *  in the example above could have been displayed in some other order.
   */
  def combinations(occurrences: Occurrences): List[Occurrences] = occurrences match {
    case List() => List(List())
    case (char: Char, freq: Int) :: xs => {
      val combos = if (freq == 1) combinations(xs) else combinations((char, freq - 1) :: xs)
      combos union (for {
        combo <- combos
        if combo.forall{case (ychar: Char, yfreq: Int) => ychar!=char}
      } yield (char, freq) :: combo)
    }
  }

  /** Subtracts occurrence list `y` from occurrence list `x`.
   *
   *  The precondition is that the occurrence list `y` is a subset of
   *  the occurrence list `x` -- any character appearing in `y` must
   *  appear in `x`, and its frequency in `y` must be smaller or equal
   *  than its frequency in `x`.
   *
   *  Note: the resulting value is an occurrence - meaning it is sorted
   *  and has no zero-entries.
   */
  def subtract(x: Occurrences, y: Occurrences): Occurrences = {
    val condensedMap = (x ++ y).groupBy { case (char, freq) => char }
    val transformedMap = condensedMap.mapValues {
      case List((char, freq)) => freq
      case (char1: Char, freq1: Int) :: List((char2: Char, freq2: Int)) => abs(freq1 - freq2)
    }
    transformedMap.filter{case (char, freq) => freq != 0}.toList.sorted
  }

  /** Returns a list of all anagram sentences of the given sentence.
   *
   *  An anagram of a sentence is formed by taking the occurrences of all the characters of
   *  all the words in the sentence, and producing all possible combinations of words with those characters,
   *  such that the words have to be from the dictionary.
   *
   *  The number of words in the sentence and its anagrams does not have to correspond.
   *  For example, the sentence `List("I", "love", "you")` is an anagram of the sentence `List("You", "olive")`.
   *
   *  Also, two sentences with the same words but in a different order are considered two different anagrams.
   *  For example, sentences `List("You", "olive")` and `List("olive", "you")` are different anagrams of
   *  `List("I", "love", "you")`.
   *
   *  Here is a full example of a sentence `List("Yes", "man")` and its anagrams for our dictionary:
   *
   *    List(
   *      List(en, as, my),
   *      List(en, my, as),
   *      List(man, yes),
   *      List(men, say),
   *      List(as, en, my),
   *      List(as, my, en),
   *      List(sane, my),
   *      List(Sean, my),
   *      List(my, en, as),
   *      List(my, as, en),
   *      List(my, sane),
   *      List(my, Sean),
   *      List(say, men),
   *      List(yes, man)
   *    )
   *
   *  The different sentences do not have to be output in the order shown above - any order is fine as long as
   *  all the anagrams are there. Every returned word has to exist in the dictionary.
   *
   *  Note: in case that the words of the sentence are in the dictionary, then the sentence is the anagram of itself,
   *  so it has to be returned in this list.
   *
   *  Note: There is only one anagram of an empty sentence.
   */
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

  def canMatch(word: Word, occurrences: Occurrences): Boolean = {
    val mappedOccurrences = occurrences.toMap
    wordOccurrences(word).forall { case (char: Char, freq: Int) => mappedOccurrences.get(char) match {
      case Some(freq2) => freq <= freq2
      case None => false
    }}
  }
}
