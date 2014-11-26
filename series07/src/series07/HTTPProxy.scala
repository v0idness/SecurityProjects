package series07

import java.net._
import java.io._
import io.BufferedSource
import io.Source
import util.matching.Regex
import util.control.Breaks._

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
		val proxy_out = new PrintWriter(socket.getOutputStream)
		
		var hostConn = new Socket
		var http_out: BufferedWriter = null
		var http_in: BufferedReader = null
		
		var host, path = ""
		var currentLine = "\n"
		var requestBuilder = ""
		var foundBlocked = false
		
		breakable {
			while (currentLine != null && currentLine != "" && !lines.isEmpty) {
				currentLine = lines.next
						// parse only GETs
						if (currentLine.split(" ")(0) == "GET") {
							val (h, p) = processGet(currentLine)
									if (filterHost(h)) host = h else { foundBlocked = true; break }
									path = if (p == "") "/" else p
							
							try {
								hostConn.connect(new InetSocketAddress(host, 80))
								http_out = new BufferedWriter(new OutputStreamWriter(hostConn.getOutputStream))
								http_in = new BufferedReader(new InputStreamReader(hostConn.getInputStream))					
							} catch {
							case e: Exception => 1
							}
							currentLine = "GET " + path + " HTTP/1.1"
						}
				requestBuilder = requestBuilder + currentLine + "\r\n"
			}
		}
		
		// only a GET establishes a connection. All others are ignored.
		if (hostConn.isConnected && !foundBlocked) {
			println(Thread.currentThread().getName() + " # " + requestBuilder)
			http_out.write(requestBuilder); http_out.flush
		
			var currReadLine = ""
			do { 
				currReadLine = http_in.readLine
				println(Thread.currentThread().getName() + " # " + currReadLine)
				proxy_out.println(currReadLine)
			} while (currReadLine != null)
			proxy_out.flush
		} else if (foundBlocked) {
			proxy_out.println("HTTP/1.1 403 Forbidden\r\n")
			proxy_out.println("Content-Type: text/plain; charset=UTF-8\r\n")
			proxy_out.println("\r\n")
			proxy_out.println("Content blocked by proxy\r\n")
			proxy_out.flush
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
		var count_m = 0
		for (re <- re_wildcards) count_m += (re findAllIn host).length
		if (count_m == 0) true else false
	}
	
	def stringToReString(s: String): String = s.replace(".", "\\.").replace("*", ".*")
}
