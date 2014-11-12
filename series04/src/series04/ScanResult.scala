package series04

/*
 * adapted from clamavj
 * https://github.com/philvarner/clamavj
 */

class ScanResult(result: String) {
	object Status extends Enumeration {
		type Status = Value
		val PASSED, FAILED, ERROR = Value
	}
	
	var status = Status.FAILED 
	var res = result
	
	val STREAM_PREFIX = "stream: "
    val RESPONSE_OK = "stream: OK"
    val FOUND_SUFFIX = "FOUND"
    val RESPONSE_SIZE_EXCEEDED = "INSTREAM size limit exceeded. ERROR"
    val RESPONSE_ERROR_WRITING_FILE = "Error writing to temporary file. ERROR"
    	
    if (result == null) status = Status.ERROR 
    else if (RESPONSE_OK == result) status = Status.PASSED
    else if (RESPONSE_SIZE_EXCEEDED == result) status = Status.ERROR
    else if (RESPONSE_ERROR_WRITING_FILE == result) status = Status.ERROR
    


}
