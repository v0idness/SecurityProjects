package series07

import java.net._
import java.io._
import collection.mutable.ArrayBuffer
import io.BufferedSource
import io.Source
import util.matching.Regex
import util.control.Breaks._
import org.apache.commons.codec.digest.DigestUtils
import org.apache.commons.io.IOUtils

/* Université de Neuchâtel
 * Security
 * Assignment 7, 8: HTTP Proxy with blocking and caching
 * Laura Rettig (laura.rettig@unifr.ch)
 * December 9, 2014
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
		
		var hostConn = new Socket
		var http_out: BufferedWriter = null
		
		var fullPath, host, path = ""
		var currentLine = "\n"
		var requestBuilder = ""
			
		var foundBlocked = false
		
		var f: File = null
		var cached = false
		var allowCache = true
		
		var get_f = false
		
		breakable {
			while (currentLine != null && currentLine != "" && !lines.isEmpty) {
				currentLine = lines.next
				// parse only GETs
				if (currentLine.split(" ")(0) == "GET") {
					get_f = true
					val (h, p) = processGet(currentLine)
					fullPath = currentLine.split(" ")(1)
					cached = inCache(fullPath)
					f = new File("data/cache/" + md5(fullPath))
					if (f.length == 0) cached = false		// exclude empty cache
					if (filterHost(h)) host = h else { foundBlocked = true; break }
					path = if (p == "") "/" else p
					
					try {
						hostConn.connect(new InetSocketAddress(host, 80))
						http_out = new BufferedWriter(new OutputStreamWriter(hostConn.getOutputStream))				
					} catch {
					case e: Exception => println(Thread.currentThread().getName() + " # could not connect to host: " + e.printStackTrace())
					}
					currentLine = "GET " + path + " HTTP/1.1"
				} else if (!get_f) {
					// TODO: support CONNECT
					println(Thread.currentThread().getName() + " # " + currentLine)
					break
				}
				
				requestBuilder = requestBuilder + currentLine + "\r\n"
			}
		}
		

		if (hostConn.isConnected && !foundBlocked && !cached) {
			println(Thread.currentThread().getName() + " # " + requestBuilder)
			http_out.write(requestBuilder); http_out.flush
		
			val i = hostConn.getInputStream
			val o = socket.getOutputStream
			
			// TODO: compare timestamp of cached file with "last-modified" of response
			// send if-modified-since & then, if the result is empty, return cache; else: overwrite
			
			var streamBuffer = new ArrayBuffer[Byte]()
			var buffer = Array[Byte](100.toByte)
			var n = i.read(buffer)
			while (n != -1) { streamBuffer ++= buffer; o.write(buffer); n = i.read(buffer) }
			
			if (cacheAllowed(new String(streamBuffer.toArray[Byte]))) {
				f.createNewFile
				val fo = new FileOutputStream(f)
				fo.write(streamBuffer.toArray[Byte]); fo.flush()
			}
			
			o.close
			hostConn.close
			
		} else if (foundBlocked) {
			respondForbidden(socket)
		} else if (cached) {
			respondFromCache(socket, f)
		}
		socket.close
	}
	
	def processGet(request: String): (String, String) = {
		val pat = "http://([^/]*)(/?.*)".r
		val pat(host, path) = request.split(" ")(1)
		(host, path)
	}
	
	def filterHost(host: String): Boolean = {
		val re_wildcards = for (l <- Source.fromFile("data/wildcards.txt").getLines) yield stringToRe(l)
		var count_m = 0
		for (re <- re_wildcards) count_m += (re findAllIn host).length
		(count_m == 0)
	}
	
	def stringToRe(s: String): Regex = 
		new Regex(s.replace(".", "\\.").replace("*", ".*"))
	
	def cacheAllowed(response: String): Boolean = 
		!((new Regex(".*Cache-Control:.*(no-cache|max-age=0|no-store).*") findAllIn response).length > 0)
	
	def inCache(path: String): Boolean = 
		new File("data/cache").list.contains(md5(path))
	
	def md5(s: String): String = DigestUtils.md5Hex(s)
	
	def respondForbidden(socket: Socket) {
		val o = new PrintWriter(socket.getOutputStream)
		o.println("HTTP/1.1 403 Forbidden\r\n")
		o.println("Content-Type: text/plain; charset=UTF-8\r\n")
		o.println("\r\n")
		o.println("Content blocked by proxy\r\n")
		o.flush
	}
	
	def respondFromCache(socket: Socket, f: File) {
		println(Thread.currentThread().getName() + " # responding from cache; file " + f.getName)
		val o = socket.getOutputStream
		o.write(IOUtils.toByteArray(new FileInputStream(f)))
		o.flush
	}
	
}
