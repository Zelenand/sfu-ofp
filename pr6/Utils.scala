import scala.annotation.tailrec
import scala.io.StdIn
import scala.util.Random

object Utils {

  private val randomizer = new Random()

  def isPrime(n: BigInt): Boolean = n.isProbablePrime(100)

  def primes(n: BigInt, m: BigInt): List[BigInt] = (n to m).filter(isPrime).toList

  def randomPrime(bits: Int): BigInt = BigInt.probablePrime(bits, randomizer)


  @tailrec
  def readBigInt(prompt: String): BigInt = {
    println(prompt)
    try {
      BigInt(StdIn.readLine())
    } catch {
      case _: NumberFormatException =>
        System.err.println("Ошибка: введите числовые значения.")
        readBigInt(prompt)
    }
  }

}
