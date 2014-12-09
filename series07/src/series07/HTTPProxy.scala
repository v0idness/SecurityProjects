package series07

import java.net._
import java.io._
import io.BufferedSource
import io.Source
import util.matching.Regex
import util.control.Breaks._
import org.apache.commons.codec.digest.DigestUtils
import org.apache.commons.io.IOUtils
import collection.mutable.ArrayBuffer

/* Université de Neuchâtel
 * Security
 * Assignment 7, 8
 * Laura Rettig (laura.rettig@unifr.ch)
 * December 8, 2014
 */

object HTTPProxy {
	def main(args: Array[String]) {
		proxyServer(8080)
		//test()
	}

	def proxyServer(port: Int) {
		val server = new ServerSocket(port)
		while (true) {
			(new Thread(new ProxyServerThread(server.accept))).start
		}
		server.close
	}
	
	def test() {
		val server = new ServerSocket(8080)
		while(true) {
			val socket = server.accept
			val proxy_in = new BufferedSource(socket.getInputStream)
			val lines = proxy_in.getLines
			val proxy_out = new PrintWriter(socket.getOutputStream)
			val currentLine = lines.next
			println(currentLine)
			
			if (currentLine.split(" ")(0) == "GET") {
				val f = new File("data/cache/f9bdecf8f71cd6eaa84d2c1bdfcd5ff6")	
				val bfi = new BufferedReader(new FileReader(f)).lines()
				var s: String = ""
				//bfi.forEach(s -> { proxy_out.println(s) } )
				proxy_out.flush
				
			/* 
				val fi = new FileInputStream(f)
				val o = socket.getOutputStream
				var buffer = Array[Byte](4096.toByte)
				var n = fi.read(buffer)
				println(fi.available)
				// TODO doesn't seem to work
				while (n != -1) { o.write(buffer); n = fi.read(buffer) }
			 * 
			 */
				println("all clear")
				
			}
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
							cached = inCache(fullPath)
							f = new File("data/cache/" + md5(fullPath))
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
		if (hostConn.isConnected && !foundBlocked && !cached) {
			println(Thread.currentThread().getName() + " # " + requestBuilder)
			http_out.write(requestBuilder); http_out.flush
		
			val i = hostConn.getInputStream
			val o = socket.getOutputStream
			
			// TODO: compare timestamp of cached file with "last-modified" of response
			
			
			/* var buffer = Array[Byte](4096.toByte)
			var n = i.read(buffer)
			// TODO: buffer the response. then write to output; translate to string to check caching; possibly write to cache
			while (n != -1) { o.write(buffer); fo.write(buffer); n = i.read(buffer) } */
			
			println(Thread.currentThread().getName() + " # before")
			val bStream = IOUtils.toByteArray(i)
			println(Thread.currentThread().getName() + " # bstream " + bStream.length)
			
			o.write(bStream); o.flush()
			if (cacheAllowed(new String(bStream))) {
				f.createNewFile
				val fo = new FileOutputStream(f)
				println(Thread.currentThread().getName() + " # caching allowed ")
				fo.write(bStream); fo.flush()
			}
			o.close
			hostConn.close
		} else if (foundBlocked) {
			// send 403 for blocked patterns
			val proxy_out = new PrintWriter(socket.getOutputStream)
			proxy_out.println("HTTP/1.1 403 Forbidden\r\n")
			proxy_out.println("Content-Type: text/plain; charset=UTF-8\r\n")
			proxy_out.println("\r\n")
			proxy_out.println("Content blocked by proxy\r\n")
			proxy_out.flush
		} else if (cached) {
			println(Thread.currentThread().getName() + " # " + requestBuilder)
			// respond from cache
			println(Thread.currentThread().getName() + " # responding from cache")
			val fi = new FileInputStream(f)
			println(Thread.currentThread().getName() + " # " + f.getAbsolutePath() + " " + fi.available())
			val o = socket.getOutputStream
			var buffer = Array[Byte](4096.toByte)
			var n = fi.read(buffer)
			// TODO doesn't seem to work
			while (n != -1) { o.write(buffer); n = fi.read(buffer) }
			println(Thread.currentThread().getName() + " # wrote from cache	")
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

	/*def addToCache(fullPath: String)
	// compute hash
	md5(fullPath)
	// write to file*/
	
}
