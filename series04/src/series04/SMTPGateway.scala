package series04

import java.net._
import java.io._
import scala.io.Source
import scala.io.BufferedSource
import util.control.Breaks._
import scala.util.matching.Regex
import org.xbill.DNS._


/* Université de Neuchâtel
 * Security
 * Assignment 4, 5
 * Laura Rettig (laura.rettig@unifr.ch)
 * November 12, 2014
 */

object SMTPGateway {
	def main(args: Array[String]) {
		//forwardSMTP("127.0.0.1", 25)
		//println("client closed; gateway shutting down")
		println(antiSpam(hostFromAddress("laura@80.218.18.1")))
		//println(antiSpam(hostFromAddress("laura@demo.local")))
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
	 	// get IP from domain IF not in IP address format
		val pat = "(\\d+\\.\\d+\\.\\d+\\.\\d+)".r
		val ip = host match {
			case pat(ipa) => ipa
			case _ => getIP(host)
		}
		// reverse order of octets
		// append server
		
		// A: address
		// NXDOMAIN: nope, we're good
		false
	}
	
	def hostFromAddress(address: String): String = {
		val pat = ".*@(.*)".r
		val pat(host) = address
		host
	}
	
	def getIP(address: String): String = {
		// DNS lookup
		address
	}
	
	def lookup(host: String, nameServer: String): Array[String] = {
		val l = new Lookup(host)
		l.setResolver(new SimpleResolver(nameServer))
		l.run()
		if (l.getResult() == Lookup.SUCCESSFUL)
			l.getAnswers().map(_.rdataToString())
		else
			Array.empty[String]
	}
}
