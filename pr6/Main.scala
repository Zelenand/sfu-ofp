import java.io.PrintWriter
import scala.annotation.tailrec
import scala.io.{Source, StdIn}

object Main {
  var rsa: Option[RSA] = None
  private var isRun = true

  def main(args: Array[String]): Unit = {
    while (isRun) {

      println("1. Создание RSA из P Q")
      println("2. Создание RSA из существующих ключей")
      println("3. Зашифровать сообщение")
      println("4. Расшифровать сообщение")
      println("5. Зашифровать в файл")
      println("6. Расшифровать из файла")
      println("7. Выход")

      var choice: Int = -1

      try {
        choice = scala.io.StdIn.readInt()
      } catch {
        case e: NumberFormatException =>
      }

      choice match {
        case 1 => makeRsaFromPq()
        case 2 => makeRsaFromExists()
        case 3 =>
          if (rsa.isDefined) encryptFromConsole()
          else println("Сначала создайте RSA")
        case 4 =>
          if (rsa.isDefined) decryptFromConsole()
          else println("Сначала создайте RSA")
        case 5 =>
          if (rsa.isDefined) encryptToFile()
          else println("Сначала создайте RSA")
        case 6 =>
          if (rsa.isDefined) decryptFromFile()
          else println("Сначала создайте RSA")
        case 7 => isRun = false
        case _ => println("Некорректный ввод")
      }

    }
  }


  private def makeRsaFromPq(): Unit = {
    val (p, q) = pickPQ()
    val rsa = RSA.makeFromPQ(p, q)
    println(s"Создан RSAEncoder: $rsa")
    this.rsa = Option.apply(rsa)
  }

  private def makeRsaFromExists(): Unit = {
    val n = Utils.readBigInt("Введите значение для N:")
    val e = Utils.readBigInt("Введите значение для E:")
    val d = Utils.readBigInt("Введите значение для D:")
    val rsa = new RSA(n, e, d)
    println(s"Создан RSAEncoder: $rsa")
    this.rsa = Option.apply(rsa)
  }

  private def encryptFromConsole(): Unit = {
    println("Введите сообщение:")
    val message = StdIn.readLine()
    println(s"Зашифрованное сообщение:\n${rsa.get.encrypt(message)}")
  }

  private def decryptFromConsole(): Unit = {
    println("Введите сообщение:")
    val message = StdIn.readLine()
    try {
      println(s"Расшифрованное сообщение:\n${rsa.get.decrypt(message)}")
    } catch {
      case e: NumberFormatException => System.err.println(s"Ошибка: некорректный ввод ${e.getMessage}")
    }
  }

  private def encryptToFile(): Unit = {
    println("Введите имя файла:")
    val filePath = StdIn.readLine()
    println("Введите сообщение:")
    val message = StdIn.readLine()
    val enc = rsa.get.encrypt(message)
    val writer = new PrintWriter(filePath)
    try {
      writer.write(enc)
      println("Зашифрованное сообщение сохранено в файл")
    } catch {
      case e: Exception => System.err.println(e.getMessage)
    } finally {
      writer.close()
    }
  }

  private def decryptFromFile(): Unit = {
    println("Введите имя файла:")
    val filePath = StdIn.readLine()
    val source = Source.fromFile(filePath)
    try {
      println(s"Расшифрованное сообщение:\n${rsa.get.decrypt(source.mkString)}")
    } catch {
      case e: Exception => System.err.println(e.getMessage)
    } finally {
      source.close()
    }
  }

  private def pickPQ(): (BigInt, BigInt) = {
    val primes = pickPrimes()

    primes.zipWithIndex.foreach {
      case (prime, index) =>
        println(s"$index. $prime")
    }

    val p = primes(chooseIndex("Выберете индекс числа для p", primes.size))
    val q = primes(chooseIndex("Выберете индекс числа для q", primes.size))
    println(s"p: $p\nq: $q")
    (p, q)
  }

  @tailrec
  private def pickPrimes(): List[BigInt] = {
    val leftBound = Utils.readBigInt("Введите левую границу диапазона простых чисел:")
    val rightBound = Utils.readBigInt("Введите правую границу диапазона простых чисел:")
    if (rightBound < leftBound || leftBound < 1 || rightBound < 1) {
      System.err.println("Ошибка Некорректный ввод")
      pickPrimes()
    } else {
      val primes = Utils.primes(leftBound, rightBound)
      if (primes.isEmpty) {
        println("В диапазоне нет простых чисел")
        pickPrimes()
      } else {
        primes
      }
    }
  }

  @tailrec
  private def chooseIndex(prompt: String, listSize: Int): Int = {
    println(prompt)
    val index = StdIn.readInt()
    if (index < 0 || index >= listSize) {
      System.err.println("Ошибка: введите корректный индекс.")
      chooseIndex(prompt, listSize)
    } else {
      index
    }
  }
}