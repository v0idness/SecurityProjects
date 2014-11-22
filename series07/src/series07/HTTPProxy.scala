package series07

import java.net._
import java.io._

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
		val server = new ServerSocket(25000)
		val serversocket = server.accept
		// String input is okay, will contain HTTP requests, not images
		val in = new BufferedReader(new InputStreamReader(serversocket.getInputStream))
		val out = new PrintWriter(serversocket.getOutputStream, true)
		
		// will need data output
	}
	
	def proxyServerThread(request: String) {
		
	}
}
