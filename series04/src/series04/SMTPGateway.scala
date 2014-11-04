package series04

import javax.mail.Session
import javax.mail.Store
import javax.mail.internet.MimeMessage
import java.util.Properties
import java.net._
import java.io.BufferedReader
import java.io.InputStreamReader
import java.io.PrintWriter
import scala.io.Source
import scala.io.BufferedSource


/* Université de Neuchâtel
 * Security
 * Assignment 4, 5, 6
 * Laura Rettig (laura.rettig@unifr.ch)
 * November 4, 2014
 */

object SMTPGateway {
	def main(args: Array[String]) {
		forwardSMTP
		println("closing")
	}
	
	def forwardSMTP {
		// port to gateway
		val server = new ServerSocket(25000)
		val serversocket = server.accept
		val in = new BufferedReader(new InputStreamReader(serversocket.getInputStream))
		
		// connection to "real" server
		val socket = new Socket("localhost", 25)
		val out = new PrintWriter(socket.getOutputStream)
		val smtp_in = new BufferedReader(new InputStreamReader(socket.getInputStream))
		
		var command = in.readLine
		var f_data = false
		while (command != null) {
			// filtering only applied to data part of the SMTP command sequence
			if (f_data) command = filterKeywords(command)
			if (command.toLowerCase == "data") f_data = true
			out.println(command)
			println(command)
			println(smtp_in.readLine)
			command = in.readLine
		}
		
	}
	
	def sendMail {
		// forwarding filtered mail to true SMTP server
		/*
		 * receive filtered message as string
		 * trim with regular expression
		 * set mail components according to javax mail
		 * send to smtp server
		 */
		
		val props = new Properties
				//props.put("127.0.0.1:25", "local-ms")
				//props.put("smtp.gmail.com", "gmail-ms")
	}
	
	def filterKeywords(line: String): String = {
		val keys = Source.fromFile("data/keywords.txt").getLines.toList
		line.split("((?=\\p{Punct})|\\s+)").filterNot(x => keys.contains(x.toLowerCase)).mkString(" ")
	}
}