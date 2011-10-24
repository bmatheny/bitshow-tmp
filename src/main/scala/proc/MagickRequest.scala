package bitshow.proc

import bitshow.{Item, Converter}
import java.io._
import io.Source
import java.lang.StringBuffer

object MagickRequest {
  import scala.collection.JavaConverters._
  private val environment = System.getenv().asScala
  val MAGICK_KEY = "MAGICK_KEY"
  val MAGICK_PATH = environment.getOrElse(MAGICK_KEY, "/usr/local/bin/convert")
}

object MagickResize
  extends MagickRequest("resize", "-resize 50%")

object MagickBlackAndWhite
  extends MagickRequest("blackwhite", "-black -white")

/**
 * Unused
 */
class ArgStack {
  val stringBuffer = new StringBuffer

  /** appends a -XXX type option
   */
  def option(a: String, b:Any) {
    stringBuffer.append("-").append(a).append(" ").append(b)
    newArg
  }

  /** adds a bit of space between arguments
   */
  def newArg {
    stringBuffer.append(" ")
  }

  def arg(a: String) {
    stringBuffer.append(a)
    newArg
  }

  def toCommandLine =
    stringBuffer.toString
}

class MagickRequest (
  val name: String,
  val args: String
) extends Converter {

  val BUFFER_SIZE = 1024 * 1024

  protected def getConvertPath =
    MagickRequest.MAGICK_PATH

  private def getTemporaryFile =
    File.createTempFile("bitshow", ".png")

  private def writeBytesToTemporaryFile(input: InputStream) = {
    val bufferedIn = new BufferedInputStream(input)
    val outFile = getTemporaryFile
    val bufferedOut = new BufferedOutputStream(new FileOutputStream(outFile))

    @annotation.tailrec()
    def readByte() {
      bufferedOut.write(bufferedIn.read)
      
      if(bufferedIn.available() > 0)
        readByte()
    }

    readByte()
    
    bufferedOut.close()
    bufferedIn.close()

    outFile
  }

  private def readBytesFromFile(file: File) =
    Source.fromFile(file)(scala.io.Codec.ISO8859).map(_.toByte).toArray

  def buildCommandLine(inputFile: File, outputFile: File) =
    getConvertPath+" "+inputFile.getAbsolutePath+" "+args+" "+outputFile.getAbsolutePath

  def execute(inputFile: File, outputFile: File): Int = {
    import scala.sys.process._
    (buildCommandLine(inputFile, outputFile)) !
  }

  def execute(inputFile: String, outputFile: String): Int =
    execute(new File(inputFile), new File(outputFile))

  def execute(input: InputStream): Array[Byte] = {
    val inputFile = writeBytesToTemporaryFile(input)
    val outputFile = getTemporaryFile

    val exitCode = execute(inputFile, outputFile)
    readBytesFromFile(outputFile)
  }

  def apply(item: Item) =
    item
}