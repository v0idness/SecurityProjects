package series07

import java.net._
import java.io._
import io.BufferedSource
import io.Source
import util.matching.Regex
import util.control.Breaks._
import org.apache.commons.codec.digest.DigestUtils

/* Université de Neuchâtel
 * Security
 * Assignment 7, 8
 * Laura Rettig (laura.rettig@unifr.ch)
 * December 8, 2014
 */

object HTTPProxy {
	def main(args: Array[String]) {
		//proxyServer(8080)
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
		
		var fullPath, host, path = ""
		var currentLine = "\n"
		var requestBuilder = ""
		var foundBlocked = false
		var allowCache = true
		
		/*
		 * for caching: 
		 * check cache-allow
		 * check if already cached
		 * if already cached: send if-modified-since & then, if the result is empty, return cache; else: overwrite
		 */
		
		breakable {
			while (currentLine != null && currentLine != "" && !lines.isEmpty) {
				currentLine = lines.next
						// parse only GETs
						if (currentLine.split(" ")(0) == "GET") {
							val (h, p) = processGet(currentLine)
							fullPath = currentLine.split(" ")(1)
							if (filterHost(h)) host = h else { foundBlocked = true; break }
							path = if (p == "") "/" else p
							
							try {
								hostConn.connect(new InetSocketAddress(host, 80))
								http_out = new BufferedWriter(new OutputStreamWriter(hostConn.getOutputStream))				
							} catch {
							case e: Exception => println(Thread.currentThread().getName() + " # could not connect to host: " + e.printStackTrace())
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
		
			var buffer = Array[Byte](4096.toByte)
			var n = 0
			val i = hostConn.getInputStream
			val o = socket.getOutputStream
			val f = new File("cache/" + md5(fullPath)); f.createNewFile
			// overwrite existing cache
			// if (!f.createNewFile) { f.delete; f.createNewFile }
			val fo = new FileOutputStream(f)
			n = i.read(buffer)
			while (n != -1) { o.write(buffer); fo.write(buffer); n = i.read(buffer) }
			
			o.close
			hostConn.close
		} else if (foundBlocked) {
			proxy_out.println("HTTP/1.1 403 Forbidden\r\n")
			proxy_out.println("Content-Type: text/plain; charset=UTF-8\r\n")
			proxy_out.println("\r\n")
			proxy_out.println("Content blocked by proxy\r\n")
			proxy_out.flush
		}
		socket.close
	}
	
	def processGet(request: String): (String, String) = {
		val url = request.split(" ")(1)
				val pat = "http://([^/]*)(/?.*)".r
				val pat(host, path) = url
				(host, path)
	}
	
	def filterHost(host: String): Boolean = {
		val re_wildcards = for (l <- Source.fromFile("data/wildcards.txt").getLines) yield stringToRe(l)
		var count_m = 0
		for (re <- re_wildcards) count_m += (re findAllIn host).length
		(count_m == 0)
	}
	
	def stringToRe(s: String): Regex = new Regex(s.replace(".", "\\.").replace("*", ".*"))
	
	def cacheAllowed(response: String): Boolean = 
		!((new Regex(".*Cache-Control: (no-cache|max-age=0|no-store).*") findAllIn response).length > 0)
	
	def inCache(request: String): Boolean = new File("data/cache").list.contains(md5(request.split(" ")(1)))
	
	def md5(s: String): String = DigestUtils.md5Hex(s)

	/*def addToCache(fullPath: String)
	// compute hash
	md5(fullPath)
	// write to file*/
	
}
