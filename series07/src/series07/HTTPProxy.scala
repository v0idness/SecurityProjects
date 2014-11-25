package series07

import java.net._
import java.io._
import scala.io.BufferedSource
import scala.io.Source
import scala.util.matching.Regex

/* Université de Neuchâtel
 * Security
 * Assignment 7
 * Laura Rettig (laura.rettig@unifr.ch)
 * November 22, 2014
 */

object HTTPProxy {
	def main(args: Array[String]) {
		proxyServer(8080)
	}

	def proxyServer(port: Int) {
		val server = new ServerSocket(port)
		while (true) {
			(new Thread(new ProxyServerThread(server.accept))).start
		}
		server.close
	}
	
}

class ProxyServerThread(socket: Socket) extends Runnable {
	def run() {		
		val proxy_in = new BufferedSource(socket.getInputStream)
		val lines = proxy_in.getLines
		//val proxy_out = new BufferedWriter(new OutputStreamWriter(socket.getOutputStream))
		val proxy_out = new PrintWriter(socket.getOutputStream)
		
		var hostConn = new Socket
		var http_out: BufferedWriter = null
		var http_in: BufferedReader = null
		
		var host, path = ""
			
		var currentLine = "\n"
		var requestBuilder = ""
		
		while (currentLine != null && currentLine != "" && !lines.isEmpty) {
			currentLine = lines.next
			// parse only GETs
			if (currentLine.split(" ")(0) == "GET") {
				val (h, p) = processGet(currentLine)
				// TODO apply filter to host
				host = h
				path = if (p == "") "/" else p
				println(Thread.currentThread().getName() + " # " + host + " : " + path)
				
				try {
					hostConn.connect(new InetSocketAddress(host, 80))
					http_out = new BufferedWriter(new OutputStreamWriter(hostConn.getOutputStream))
					http_in = new BufferedReader(new InputStreamReader(hostConn.getInputStream))					
				} catch {
					case e: Exception => ""
				}
				
				currentLine = "GET " + path + " HTTP/1.1"
			}
			requestBuilder = requestBuilder + currentLine + "\r\n"
			
		}
		// only a GET establishes a connection. All others are ignored.
		if (hostConn.isConnected) {
			println(Thread.currentThread().getName() + " # " + requestBuilder)
			http_out.write(requestBuilder); http_out.flush
			println(Thread.currentThread().getName() + " # " + "sent")
		}
		
		if (hostConn.isConnected) {
			var currReadLine = ""
			do { 
				currReadLine = http_in.readLine
				println(Thread.currentThread().getName() + " # " + currReadLine)
				proxy_out.println(currReadLine)
			} while (currReadLine != null)
			proxy_out.flush
			/*var http_in2 = hostConn.getInputStream
			var i = 0
			while (i != -1) {
				var av = http_in2.available-1
				println(Thread.currentThread().getName() + " # " + av)
				var b = Array[Byte](av.toByte)
				i = http_in2.read(b, 0, av)
				socket.getOutputStream().write(b)
			}*/
		}
	}
	
	def processGet(request: String): (String, String) = {
		val url = request.split(" ")(1)
				val pat = "http://([^/]*)(/?.*)".r
				val pat(host, path) = url
				(host, path)
	}
	
	def filterHost(host: String): Boolean = {
		val re_wildcards = for (l <- Source.fromFile("data/wildcards.txt").getLines) yield new Regex(stringToReString(l))
		//host.split("((?=\\p{Punct})|\\s+|(?<=\\p{Punct}))").filterNot(x => wildcards.contains(x.toLowerCase)).mkString(" ")
		true
	}
	
	def stringToReString(s: String): String = s.replace(".", "\\\\.").replace("*", ".*")
}
