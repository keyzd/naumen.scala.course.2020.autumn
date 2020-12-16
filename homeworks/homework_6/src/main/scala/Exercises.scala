object Exercises {
  def reverse[T](seq: Seq[T]): Seq[T] = {
	  for(i <- seq.length-1 to 0 by -1) yield seq(i)
  }

  /**
   * https://ru.wikipedia.org/wiki/Числа_Фибоначчи
   *
   * @param idx
   * @return
   */
  def fibonacci4Index(idx: Int): Int = idx match {
	  case 0 | 1 => idx
	  case _ => fibonacci4Index(idx-1) + fibonacci4Index(idx-2)
  }

  def fibonacci(idx: Int): Seq[Int] = {
	  for(i <- 0 to idx) yield fibonacci4Index(i)
  }

  lazy val MORSE = Map("A" -> ".-", "B" -> "-...", "C" -> "-.-.", "D" -> "-..", "E" -> ".", "F" -> "..-.",
                       "G" -> "--.", "H" -> "....", "I" -> "..", "J" -> ".---", "K" -> "-.-", "L" -> ".-..",
                       "M" -> "--", "N" -> "-.", "O" -> "---", "P" -> ".--.", "Q" -> "--.-", "R" -> ".-.",
                       "S" -> "...", "T" -> "-", "U" -> "..-", "V" -> "...-", "W" -> ".--", "X" -> "-..-",
                       "Y" -> "-.--", "Z" -> "--..")

  def morse(text: String): String = {
	  text.map(c =>
	  			if(c.isLetter) " "+MORSE(c.toUpper.toString)
				else if(c.isSpaceChar) " "+c
				else c
			  ).mkString.substring(1)
  }

  def wordReverse(text: String): String = {
	  var word = scala.collection.mutable.ListBuffer[Char]()
	  var textReversed = scala.collection.mutable.ListBuffer[String]()
	  text.foreach(c =>
	  	if(c.isLetter)
			word += c
		else {
			if(word.length != 0) {
				word = word.reverse
				if(word.last.isUpper) {
					word(0) = word(0).toUpper
					word(word.length-1) = word.last.toLower
				}
				textReversed += word.mkString
				word = scala.collection.mutable.ListBuffer[Char]()
			}
			textReversed += c.toString
		}
	  )
	  textReversed.mkString
  }
}
