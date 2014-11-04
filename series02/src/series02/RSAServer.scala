package series02

import java.net._
import java.io._
import scala.io._
import scala.util.Random
import scala.util.matching.Regex
import scala.math._
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

object RSAServer {
	def main(args: Array[String]) {
		val (e,d,n) = RSA
		val KU = (e,n)	// public key
		val KR = (d,n)	// private key
		
		// listen on port Y
		val server = new ServerSocket(20000)
		val socket = server.accept
		val in = new BufferedSource(socket.getInputStream)
		val out = new PrintStream(socket.getOutputStream)
		
		// initial public key exchange (server sends, client responds)
		out.println(KU); out.flush
		 
		var KU_client = (BigInt(0),BigInt(0));
		new Regex("\\D*(\\d*),(\\d*)\\D*").findAllIn(in.getLines.next).matchData foreach {
			m => KU_client = (BigInt(m.group(1).toInt), BigInt(m.group(2).toInt))
		}
		// secure connection enabled
		println(KU_client + " public key of client received")
		
		// generate and transmit symmetrical key
		val key = BigInt.probablePrime(8, Random)
		println("key " + key)
		out.println(RSAencrypt(key, KU_client)); out.flush
		
		// transform to SHA-1 key for cipher encryption
		var keySHA = MessageDigest.getInstance("SHA-1").digest(key.toString.getBytes("UTF-8"))
		keySHA = Arrays.copyOf(keySHA, 16) // use only first 128 bit
		
		// secure cipher connection: receive encrypted HTTP GET request
		val cipher = Cipher.getInstance("AES")
		cipher.init(Cipher.DECRYPT_MODE, new SecretKeySpec(keySHA, "AES"))
		val cis = new CipherInputStream(socket.getInputStream, cipher)
		
		var request = ArrayBuffer[Byte]()
		var block = cis.read.toByte
		while (block != -1) { request += block; block = cis.read.toByte }
		val stringRequest = new String(request.toArray, "UTF-8")
		println("HTTP request made to server: " + stringRequest)
		
		// forward HTTP GET request to google (or really anywhere)
		val websocket = new Socket
		websocket.connect(new InetSocketAddress("www.google.ch", 80))
		val ws_in = new BufferedReader(new InputStreamReader(websocket.getInputStream))
		val ws_out = new PrintWriter(websocket.getOutputStream)
		ws_out.println(stringRequest + "\r\n\r\n")
		ws_out.flush

		// print result
		
		while (true) { println(ws_in.readLine) }
		
		websocket.close; socket.close
	}
	
	// function RSA computes the key pair; short bit length due to lack of powerful machine
	def RSA: (BigInt, BigInt, BigInt) = {
		val p = BigInt.probablePrime(8, Random)
		val q = BigInt.probablePrime(8, Random)
		val n = p*q
		val phi = (p-1)*(q-1)
		val e = Random.shuffle(Range.BigInt(BigInt(1),phi,BigInt(2)).filter(x => relPrime(phi, x))).head
		val d = e.modInverse(phi)
		(e,d,n)
	}
	
	def RSAencrypt(m: BigInt, KU_other: (BigInt, BigInt)): Int = {
		// encrypt with the other's public key
		val (e,n) = KU_other
		val c = m.modPow(e,n)
		c.toInt
	}
	
	// greatest common divisor
	def gcd(a: BigInt, b: BigInt): BigInt = if (b==0) a else gcd(b, a%b)
	
	// relatively prime numbers to i
	def relPrime(i: BigInt, x: BigInt) = gcd(i,x) == 1
}