package bitshow.proc

import java.io.File
import org.specs.Specification
import org.specs._

class MagickRequestSpecs extends Specification {
  val inFile = new File("/in")
  val outFile = new File("/out")

  "MagickRequest" should {
    "variable cli" in {
      System.setProperty(MagickRequest.MAGICK_KEY, "//convert")

      class TestableMagickRequest(n: String, a: String)
        extends MagickRequest(n, a)
      {
        override
        protected def getConvertPath = "//convert"
      }

      new TestableMagickRequest("x", "-dog").buildCommandLine(inFile, outFile) must_==
        "//convert /in -dog /out"

      new TestableMagickRequest("x", "-black").buildCommandLine(inFile, outFile) must_==
        "//convert /in -black /out"
    }
  }

  "MagickResize" should {
    "have the right CLI" >> {
      // BUG: a bug in must_== causes spec2 to throw an exception, so we use == instead
      val request = MagickRequest.MAGICK_PATH+" /in -resize 50% /out"
      (MagickResize.buildCommandLine(inFile, outFile) == request) must beTrue
    }
  }

}