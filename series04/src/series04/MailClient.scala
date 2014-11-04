package series04

import java.net.Socket
import java.io.PrintStream
/*import javax.swing._
import java.awt._*/

object MailClient {
	def main(args: Array[String]) {
		println("Mail client started")
		val socket = new Socket("localhost", 25000)
		val out = new PrintStream(socket.getOutputStream)
		
		out.println("HELO MacBook-Pro.local")
		
		print("Enter sender address\nmail from: ")
		val mail_from = Console.readLine
		out.println("MAIL FROM: " + mail_from)
		
		print("Enter recipient address\nrcpt to: ")
		val rcpt_to = Console.readLine
		out.println("RCPT TO: " + rcpt_to)
				
		print("Enter subject: ")
		val subject = Console.readLine
		out.println("DATA\nSubject: " + subject + "\n")
		
		println("Enter text. To terminate, type \"SEND\" on a new line:")
		var text_line = Console.readLine
		while (text_line != "SEND") { 
			out.println(text_line)
			text_line = Console.readLine
		}
		out.println(".")
		out.println("quit")
		
		socket.close
		// SwingUtilities.invokeLater(new Runnable { def run { showGui } } )
	}
	
	/*def showGui {
		val frame = new JFrame("Send E-Mail via SMTP")
		frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
		val panel = new JPanel(new SpringLayout)
		
		val label_from = new JLabel("Sender")
		val from = new JTextField(20)
		panel.add(label_from)
		label_from.setLabelFor(from)
		panel.add(from)
		
		val label_to = new JLabel("Recipient")
		val to = new JTextField(20)
		panel.add(label_to)
		label_to.setLabelFor(to)
		panel.add(to)
		
		val label_subject = new JLabel("Subject")
		val subject = new JTextField(20)
		panel.add(label_subject)
		label_subject.setLabelFor(subject)
		panel.add(subject)
		
		val text = new JTextArea(10,20)
		val scrl = new JScrollPane(text)
		text.setLineWrap(true)
		scrl.setVerticalScrollBarPolicy(ScrollPaneConstants.VERTICAL_SCROLLBAR_ALWAYS)
		
		val send = new JButton("Send")

		panel.add(scrl); panel.add(send)
		
		SpringUtilities.
		
		
		frame.getContentPane.add(panel)
		//frame.setSize(300, 200)
		
		frame.pack
		frame.setVisible(true)
	}*/
}