package series04


import java.io.ByteArrayInputStream
import java.io.DataOutputStream
import java.io.IOException
import java.io.InputStream
import java.net.InetSocketAddress
import java.net.Socket
import java.net.SocketException

/*
 * adapted from clamavj
 * https://github.com/philvarner/clamavj
 */

class ClamScan(timeout: Int, host: String, port: Int) {
	
	val CHUNK_SIZE = 2048
	val INSTREAM = "zINSTREAM\0".getBytes
    val PING: Array[Byte] = "zPING\0".getBytes
    val STATS: Array[Byte] = "nSTATS\n".getBytes
    
    def stats: String = return cmd(STATS)
    
    def ping: Boolean = return "PONG\0".equals(cmd(PING))
    
    def cmd(cmd: Array[Byte]): String = {
		val socket = new Socket
		socket.connect(new InetSocketAddress(host, port))
		socket.setSoTimeout(timeout)
	
		val dos = new DataOutputStream(socket.getOutputStream)
		dos.write(cmd)
		dos.flush
		
		val is = socket.getInputStream
		var read = CHUNK_SIZE
		val buffer: Array[Byte] = new Array[Byte](CHUNK_SIZE)
		var response = new StringBuilder
		
		while (read==CHUNK_SIZE) {
			read = is.read(buffer)
			response.append(new String(buffer, 0, read))
		}
		
		dos.close
		socket.close
		
		response.toString		
	}
	
	def scan(in: InputStream): ScanResult = {

        val socket = new Socket
        socket.connect(new InetSocketAddress(host, port))
        socket.setSoTimeout(timeout);

        val dos = new DataOutputStream(socket.getOutputStream)
        dos.write(INSTREAM)

        var read = CHUNK_SIZE;
        val buffer = new Array[Byte](CHUNK_SIZE)
        var response = ""
        
        while (read == CHUNK_SIZE) {
        	read = in.read(buffer)
        	if (read > 0) { // if previous read exhausted the stream
        		dos.writeInt(read)
                dos.write(buffer, 0, read)
                }
            }

        dos.writeInt(0)
        dos.flush

        read = socket.getInputStream.read(buffer)
        if (read > 0) response = new String(buffer, 0, read)

        dos.close
        socket.close

        new ScanResult(response.trim)
    }


}
