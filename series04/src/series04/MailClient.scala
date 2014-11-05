package series04

import java.net.Socket
import java.io.PrintStream
import java.io.BufferedReader
import java.io.InputStreamReader

/* Université de Neuchâtel
 * Security
 * Assignment 4
 * Laura Rettig (laura.rettig@unifr.ch)
 * November 5, 2014
 */

object MailClient {
	def main(args: Array[String]) {
		println("Mail client started")
		val socket = new Socket("localhost", 25000)
		val out = new PrintStream(socket.getOutputStream)
		val in = new BufferedReader(new InputStreamReader(socket.getInputStream))
		
		out.println("HELO demo.local")
		println(in.readLine)
		
		print("Enter sender address\nmail from: ")
		val mail_from = Console.readLine
		out.println("MAIL FROM: " + mail_from)
		println(in.readLine)
		
		print("Enter recipient address\nrcpt to: ")
		val rcpt_to = Console.readLine
		out.println("RCPT TO: " + rcpt_to)
		println(in.readLine)
				
		print("Enter subject: ")
		val subject = Console.readLine
		out.println("DATA\nSubject: " + subject + "\n")
		println(in.readLine)
		
		println("Enter text. To terminate, type \"SEND\" on a new line:")
		var text_line = Console.readLine
		while (text_line != "SEND") { 
			out.println(text_line)
			text_line = Console.readLine
		}
		out.println(".")
		println(in.readLine)
		out.println("quit")
		println(in.readLine)
		
		socket.close
	}
}