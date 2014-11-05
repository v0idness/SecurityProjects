package series04

import java.net._
import java.io._
import scala.io.Source
import scala.io.BufferedSource


/* Université de Neuchâtel
 * Security
 * Assignment 4
 * Laura Rettig (laura.rettig@unifr.ch)
 * November 5, 2014
 */

object SMTPGateway {
	def main(args: Array[String]) {
		forwardSMTP
		println("client closed; gateway shutting down")
	}
	
	def forwardSMTP {
		// port to gateway for access from client
		val server = new ServerSocket(25000)
		val serversocket = server.accept
		val in = new BufferedReader(new InputStreamReader(serversocket.getInputStream))
		val out = new PrintWriter(serversocket.getOutputStream, true)
		
		// connection to "real" SMTP server
		val socket = new Socket("127.0.0.1", 25)
		val smtp_out = new BufferedWriter(new OutputStreamWriter(socket.getOutputStream))
		val smtp_in = new BufferedReader(new InputStreamReader(socket.getInputStream))
		
		var response = smtp_in.readLine
		out.println(response)
		var command = in.readLine
		var f_data = false 	// flag for data part: between DATA and ., the keyword filter is applied
		while (command != null) {
			if (command == ".") f_data = false
			if (f_data) command = filterKeywords(command)
			smtp_out.write(command+"\r\n"); smtp_out.flush
			if(!f_data) { response = smtp_in.readLine; out.println(response) }
			if (command.toLowerCase == "data") f_data = true
			command = in.readLine
		}
		
	}
	
	def filterKeywords(line: String): String = {
		val keys = Source.fromFile("data/keywords.txt").getLines.toList
		line.split("((?=\\p{Punct})|\\s+|(?<=\\p{Punct}))").filterNot(x => keys.contains(x.toLowerCase)).mkString(" ")
	}
}