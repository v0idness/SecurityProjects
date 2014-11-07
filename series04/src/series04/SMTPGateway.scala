package series04

import java.net._
import java.io._
import scala.io.Source
import scala.io.BufferedSource
import util.control.Breaks._


/* Université de Neuchâtel
 * Security
 * Assignment 4, 5
 * Laura Rettig (laura.rettig@unifr.ch)
 * November 7, 2014
 */

object SMTPGateway {
	def main(args: Array[String]) {
		forwardSMTP("127.0.0.1", 25)
		println("client closed; gateway shutting down")
	}
	
	def forwardSMTP(host: String, port: Int) {
		// port to gateway for access from client
		val server = new ServerSocket(25000)
		val serversocket = server.accept
		val in = new BufferedReader(new InputStreamReader(serversocket.getInputStream))
		val out = new PrintWriter(serversocket.getOutputStream, true)
		
		// connection to "real" SMTP server
		val socket = new Socket(host, port)
		val smtp_out = new BufferedWriter(new OutputStreamWriter(socket.getOutputStream))
		val smtp_in = new BufferedReader(new InputStreamReader(socket.getInputStream))
		
		out.println(smtp_in.readLine)
		var command = in.readLine
		breakable {
			var f_data = false 	// flag for data part: between DATA and ., the keyword filter is applied
			var data = ""
			while (command != null) {
				if (command == ".") f_data = false
				if (f_data) {
					data = data + command + "\n"
					command = filterKeywords(command)
					if (antiVirScan(command)) { out.println("FOUND VIRUS. NOT SENDING."); break } 
				}
				smtp_out.write(command+"\r\n"); smtp_out.flush				
				if(!f_data) out.println(smtp_in.readLine) 
				if (command.toLowerCase == "data") f_data = true
				command = in.readLine
			}
		}
		
	}
	
	def filterKeywords(line: String): String = {
		val keys = Source.fromFile("data/keywords.txt").getLines.toList
		line.split("((?=\\p{Punct})|\\s+|(?<=\\p{Punct}))").filterNot(x => keys.contains(x.toLowerCase)).mkString(" ")
	}
	
	def antiVirScan(content: String): Boolean = {
		val scanner = new ClamScan(60000, "localhost", 3310)
		val result: ScanResult = scanner.scan(new ByteArrayInputStream(content.getBytes))
		println(result.res + " : " + result.status)
		if (result.status.toString == "PASSED") return false
		else { println(result.res + " : " + result.status); return true }
	}
}