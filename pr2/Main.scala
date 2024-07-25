/**
 * Лабораторная работа №2
 * Отображения, трейты и функции высшего порядка
 *
 * Написать скрипт, принимающий в качестве первого параметра
 * путь к текстовому файлу (plain text).
 * В соответствии с вариантом задания скрипт должен вывести в
 * стандартный поток результаты обработки, а в случае возникновения
 * ошибки вывести соответствующее сообщение в поток ошибок
 *
 * Вариант 4(24): Вернуть среднюю длину слова для каждой строки текста.
 */

object Main {
  def main(args: Array[String]): Unit = {
    if (args.length < 1) {
      throw new Exception("Please provide the path to the text file as a parameter.")
    }

    val filePath = args(0)

    val source = scala.io.Source.fromFile(filePath)
    val lines = try source.getLines().toList finally source.close()

    val averageWordLengths = lines.map(calculateAverageWordLength)

    averageWordLengths.foreach(println)
  }

  /**
   * Вычисление средней длины слов в строке
   * @param line строка
   * @return средняя длина слов в строке
   */
  def calculateAverageWordLength(line: String): Double = {
    val words = line.split("\\s+")
    val totalLength = words.map(_.length).sum
    val averageLength = if (words.nonEmpty) totalLength / words.length.toDouble else 0
    averageLength
  }
}