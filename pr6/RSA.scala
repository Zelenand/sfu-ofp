import scala.annotation.tailrec
import scala.util.Random

class RSA (val n: BigInt, val e: BigInt, val d: BigInt) {

  override def toString: String = {
    s"RSA(public_key=($e, $n), private_key=($d, $n))"
  }
  
  def publicKey: (BigInt, BigInt) = (e, n)

  def privateKey: (BigInt, BigInt) = (d, n)

  def encrypt(s: String): String = {
    RSA.encrypt(s, e, n)
  }

  def decrypt(s: String): String = {
    RSA.decrypt(s, d, n)
  }

}


object RSA {

  private val delimiter = "✷"

  def makeFromPQ(p: BigInt, q: BigInt): RSA = {
    val n = p * q
    val phi = (p - 1) * (q - 1)

    val random = new Random()

    @tailrec
    def findE(e: BigInt): BigInt = {
      if (e.gcd(phi) == 1) e
      else findE(BigInt(phi.bitLength, random))
    }

    val e = findE(BigInt(phi.bitLength, random))
    val d = e.modInverse(phi)
    new RSA(n, e, d)
  }


  def encrypt(s: String, e: BigInt, n: BigInt): String = {
    s.toCharArray.map(
      BigInt(_)
        .modPow(e, n)
    ).mkString(delimiter)
  }

  def decrypt(s: String, d: BigInt, n: BigInt): String = {
    s.split(delimiter).map(
      BigInt(_)
        .modPow(d, n)
        .toChar
    ).mkString("")
  }
}
