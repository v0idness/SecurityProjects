package series01

import scala.io.Source
import scala.collection.mutable.ArrayBuffer
import java.nio.file._
import java.io.IOException
import java.io.FileInputStream
import org.apache.commons.codec.digest._


/* Université de Neuchâtel
 * Security
 * Assignment 1
 * Laura Rettig (laura.rettig@unifr.ch)
 * October 7, 2014
 */

object FileIntegrity {
	
	def main(args: Array[String]) {
		
		var idx = ArrayBuffer[(String, String, String, Boolean)]()
		// index structure: dynamic array of Tuple4
		// (filename, filepath i.e. full path to parent directory of the file, hash, flag isFolder)
		
		
		println("Please enter a path to a directory")
		val root: Path = Paths.get(Console.readLine)
		
		var running = true
		while (running) {
			println("\nChoose operation:\n(1) index \t(2) analyze \t(3) quit")
			try {
				Console.readLine.toInt match {
					case 1 => idx = index(root, Source.fromFile("data/exceptions.txt" ).getLines.toArray)
					case 2 => if (idx.isEmpty) println("Please run 'index' operation first.")
						else {
							val messages = analyze(root, idx, Source.fromFile("data/exceptions.txt" ).getLines.toArray)
							if (messages.isEmpty) println("All clear, nothing has changed!")
							else messages.foreach(println)
						}
					case 3 => running = false
					case _ => println("Invalid input")
				}
				
			} catch {
				case e:Exception => println("Invalid input\n" + e.getStackTraceString)
			}
		}
		println("\nProgram closed.")
	}
	
	// indexing function
	// if there is already an index it will be overwritten
	def index(root: Path, ex: Array[String]): ArrayBuffer[(String, String, String, Boolean)] = {
		val index = createIndex(root, ex)
		return index
	}
	
	// analyze
	// will create a new index of the directory for comparison, but will not overwrite the old index
	def analyze(root: Path, idx_old: ArrayBuffer[(String, String, String, Boolean)], ex: Array[String]): ArrayBuffer[String] = {
		var errorMessages = ArrayBuffer[String]()
		
		val idx_new = createIndex(root, ex)
		
		// clones of indices for detecting files
		var idx_old_copy = idx_old.clone
		var idx_new_copy = idx_new.clone
		
		for ((n_old, p_old, h_old, isFolder_old) <- idx_old) {
			for ((n_new, p_new, h_new, isFolder) <- idx_new) {
				if (n_new == n_old) {
					if (h_old == h_new && p_old == p_new) { // unchanged
						idx_old_copy.remove(idx_old_copy.indexOf((n_old, p_old, h_old, isFolder_old))) 
						idx_new_copy.remove(idx_new_copy.indexOf((n_new, p_new, h_new, isFolder)))
					}
					else if (h_old != h_new && p_old == p_new) { // file modified or file inside folder
						if (isFolder) errorMessages += "Folder " + n_new + " or a file/folder inside it has been modified"
						else errorMessages += "File " + n_new + " has been modified"
						idx_old_copy.remove(idx_old_copy.indexOf((n_old, p_old, h_old, isFolder_old))) 
						idx_new_copy.remove(idx_new_copy.indexOf((n_new, p_new, h_new, isFolder)))
					}
					 
				}
			}
		}
		for ((n,_,_,_) <- idx_old_copy) errorMessages += n + " has been deleted"
		for ((n,_,_,_) <- idx_new_copy) errorMessages += n + " has been created"
		
		return errorMessages
	}
	
	
	// create an index from a path using a set of exceptions to be skipped
	def createIndex(root: Path, ex: Array[String]): ArrayBuffer[(String, String, String, Boolean)] = {
		var idx = ArrayBuffer[(String, String, String, Boolean)]()

		val visitor = new SimpleFileVisitor [Path] {
				// when visiting a file, check if it is not in the exceptions, then add to index
				override def visitFile(file: Path, attrs: attribute.BasicFileAttributes): FileVisitResult = {
					if (!ex.contains(file.getFileName.toString)) {
						idx.append((file.getFileName.toString, file.getParent.toString, createChecksum(file.toAbsolutePath.toString), false))
					}
					FileVisitResult.CONTINUE;
				}
				
				// before visiting a directory, check if it is in exceptions, in which case it will be skipped
				override def preVisitDirectory(dir: Path, attrs: attribute.BasicFileAttributes): FileVisitResult = {
					if (ex.contains(dir.getFileName.toString)) FileVisitResult.SKIP_SUBTREE
					else FileVisitResult.CONTINUE
				}
				
				// when all files in a directory have been visited (= added to index)
				override def postVisitDirectory(dir: Path, e: IOException): FileVisitResult = {
					// create hash value for folder from folder's name and contained file/folder hashes
					// for the folder, get all where the parent path is the path of the folder.
					// by adding the name, it is guaranteed that empty folders are also hashed
					var folderString = dir.getFileName.toString
					for ((_,path,h,_) <- idx) if (path == dir.toString) folderString += h
					val folderHash = DigestUtils.md5Hex(folderString)
					try {
						idx.append((dir.getFileName.toString, dir.getParent.toString, folderHash, true))
					} catch {
						// when dir.getParent fails
						case e: Exception => idx.append((dir.getFileName.toString, "", folderHash, true))
					}
					
					FileVisitResult.CONTINUE;
				}
			}
		
		Files.walkFileTree (root, visitor)
		
		return idx
	}
	
	def createChecksum(file: String): String = DigestUtils.md5Hex(new FileInputStream(file))
	
}