package series02

import java.net._
import java.io._
import scala.io._
import scala.math._
import scala.util.Random
import java.lang.Exception
import scala.util.matching.Regex
import javax.crypto._
import javax.crypto.spec._
import java.security.MessageDigest
import java.util.Arrays
import scala.collection.mutable.ArrayBuffer

/* Université de Neuchâtel
 * Security
 * Assignment 2
 * Laura Rettig (laura.rettig@unifr.ch)
 * October 22, 2014
 */


object RSAClient {
	def main(args: Array[String]) {
		val (e,d,n) = RSA
		val KU = (e,n)	// public key
		val KR = (d,n)	// private key
		
		// connect to server
		val s = new Socket(InetAddress.getByName("localhost"), 20000)
		val in = new BufferedSource(s.getInputStream)
		val out = new PrintStream(s.getOutputStream)
		
		// initial public key exchange (server sends, client responds)
		var KU_server = (BigInt(0), BigInt(0));
		new Regex("\\D*(\\d*),(\\d*)\\D*").findAllIn(in.getLines.next).matchData foreach {
			m => KU_server = (BigInt(m.group(1)), BigInt(m.group(2)))
		}
		
		out.println(KU); out.flush
		// secure connection enabled
		println(KU_server + " public key of server received")
		
		// shared symmetrical session key
		val x = in.getLines.next
		println(x)
		val key = RSAdecrypt(BigInt(x), KR)
		
		println("got the key " + key)
		
		// transform to SHA-1 key for cipher encryption
		var keySHA = MessageDigest.getInstance("SHA-1").digest(key.toString.getBytes("UTF-8"))
		keySHA = Arrays.copyOf(keySHA, 16) // use only first 128 bit
		
		// secure cipher connection
		val cipher = Cipher.getInstance("AES")
		val secretKey = new SecretKeySpec(keySHA, "AES")
		cipher.init(Cipher.ENCRYPT_MODE, secretKey)
		val cos = new CipherOutputStream(s.getOutputStream, cipher)
		
		// listen on port X
		val server_listen = new ServerSocket(8080)
		val socket = server_listen.accept
		val in_listen = new BufferedSource(socket.getInputStream).getLines
		
		// forward encrypted GET request to server
		val in_request = in_listen.next
		println("HTTP request made: " + in_request)
		cos.write(in_request.getBytes("UTF-8"))
		
		cos.close; s.close
	}
	
	// function RSA computes the key pair
	def RSA: (BigInt, BigInt, BigInt) = {
		val p = BigInt.probablePrime(8, Random)
		val q = BigInt.probablePrime(8, Random)
		val n = p*q
		val phi = (p-1)*(q-1)
		val e = Random.shuffle(Range.BigInt(BigInt(1),phi,BigInt(2)).filter(x => relPrime(phi, x))).head
		val d = e.modInverse(phi)
		(e,d,n)
	}
	
	def RSAdecrypt(c: BigInt, KR: (BigInt, BigInt)): BigInt = {
		// decrypt with own private key
		println("c " + c)
		val (d,n) = KR
		val m = c.modPow(d, n)
		println("m " + m)
		m
	}
	
	// greatest common divisor
	def gcd(a: BigInt, b: BigInt): BigInt = if (b==0) a else gcd(b, a%b)
	
	// relatively prime numbers to i
	def relPrime(i: BigInt, x: BigInt) = gcd(i,x) == 1

}