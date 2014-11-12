package series04

import java.net._
import java.io._
import scala.io.Source
import scala.io.BufferedSource
import util.control.Breaks._
import scala.util.matching.Regex


/* Université de Neuchâtel
 * Security
 * Assignment 4, 5, 6
 * Laura Rettig (laura.rettig@unifr.ch)
 * November 12, 2014
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
			while (command != null) {
				if (command.toLowerCase.contains("mail from") && antiSpam(hostFromEmail(command))) {
					out.println("SPAMMER DETECTED. NOT SENDING."); break }
				
				if (command == ".") f_data = false
				
				if (f_data) {
					if (antiVirScan(command)) { out.println("FOUND VIRUS. NOT SENDING."); break } 
					command = filterKeywords(command)
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
		if (result.status.toString == "PASSED") return false
		else return true
	}
	
	def antiSpam(host: String): Boolean = {
		// reverse order of domain or octets (IP address)
		val reverse_host = host.split('.').toList.reverse.mkString(".")
		try {
			// if the lookup succeeds, we have found a spam address
			InetAddress.getAllByName(reverse_host + ".dnsbl.sorbs.net")
			return true
		} catch {
			// if the lookup fails, we're good
			case e: UnknownHostException => return false
		}
	}
	
	def hostFromEmail(address: String): String = {
		val pat = ".*@(.*)".r
		val pat(host) = address
		host
	}
}
