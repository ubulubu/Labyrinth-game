package game
import java.io.FileReader
import java.io.FileNotFoundException
import java.io.BufferedReader
import java.io.IOException
import java.io.FileWriter
import java.io.BufferedWriter
import java.io.BufferedReader
class FileOps {
  
  //writes to the given file
  def writeToFile(fileName: String, arr: Seq[String]) = {
    try {
      val fw = new FileWriter(fileName);
      val buffWriter = new BufferedWriter(fw)
      try {
        for(str <- arr) {
          buffWriter.write(str)
          buffWriter.newLine()
        }
      } finally {
        buffWriter.close()
      }
    } catch {
      case e: FileNotFoundException =>
        println("File not found"); Seq()
      case ioe: IOException => println("Issue with IO."); Seq()
      case _: Throwable => println("Unexpected Exception"); Seq()
    }
  
  }

  //reads from the given file
  def readFile(sourceFile: String): Seq[String] = {
    try {
      val fReader = new FileReader(sourceFile)
      val lineReader = new BufferedReader(fReader)
      try {
        val arr = {
          var resList = Seq[String]()
          var oneLine: String = null
          while ({oneLine = lineReader.readLine(); oneLine != null}) {
      resList = resList :+ oneLine
    }
    resList
        }
        arr
      } finally {
        fReader.close()
        lineReader.close()
      }
    } catch {
      case e: FileNotFoundException =>
        println("File not found"); Seq()
      case ioe: IOException => println("Issue with IO."); Seq()
      case _: Throwable => println("Unexpected Exception"); Seq()
   }
  }
  
}