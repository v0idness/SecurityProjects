package series07

import java.net._
import java.io._
import scala.io._

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
			proxyServerThread(server.accept)
		}
		server.close
		
		// will need data output
	}
	
	def proxyServerThread(socket: Socket) {
		println("new 'thread' started")
		
		// String input is okay, will contain HTTP requests, not images/data
		val in = new BufferedSource(socket.getInputStream)
		val lines = in.getLines
		val host, path = null
		while (lines.hasNext) {
			val currentLine = lines.next
			println(currentLine)
			if (currentLine.split(" ")(0) == "GET") {
				val (host, path) = processGet(currentLine)
				println(host + " : " + path)
			}
			
		}

		// identify GET request
		
		// identify host
		
		// apply filter
		
		// connect to host
		
		// wait for response
		
		//return response
	}
	
	def processGet(request: String): (String, String) = {
		val url = request.split(" ")(1)
		val pat = "http://([^/]*)(/.*)".r
		val pat(host, path) = url
		(host, path)
	}
}
