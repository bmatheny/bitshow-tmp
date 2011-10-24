package bitshow

import unfiltered.request._
import unfiltered.response._

import org.clapper.avsl.Logger
import java.io.{ByteArrayOutputStream, InputStream}

object Pipe {
  def apply(out: ByteArrayOutputStream)(in: InputStream) = {
    @annotation.tailrec
    def consume(buffer: Array[Byte]): ByteArrayOutputStream = in.read(buffer, 0, buffer.length) match {
	  case -1 => 
	    out.flush()
	    out
	  case read =>
	    out.write(buffer, 0, read)
	    consume(buffer)
    }
    consume(new Array[Byte](1024*16))
  }
}

/** Browser front end, serves the page and handles multipart uploads */
object Browse extends unfiltered.filter.Plan {
  val logger = Logger(getClass)

  def intent = {
    case GET(Path("/")) =>
      logger.debug("GET /")
      Ok ~> view(
	<h1> _bitshow </h1>
	<form action='/upload' method='post' enctype='multipart/form-data'>
	  <fieldset>
	    <ol>
	      <li>
	        <label for='image'>file:</label>
	        <input type='file' name='image'></input>
	      </li>
	    </ol>
	    <input class='button' type='submit' value='upload' />
	  </fieldset>
	</form>
	)
	case POST(Path("/upload") & MultiPart(req)) => 
      logger.debug("POST /upload")
	  MultiPartParams.Streamed(req).files("image") match {
	    case Seq(f, _*) =>
	      val image = new ByteArrayOutputStream
	      f.stream(Pipe(image))
	      val image_array = image.toByteArray
	      val item = Item(f.contentType, image_array)
	      DefaultStore.put(item)
	      ResponseBytes(image_array)
	    case _ => 
		  view(<h3>upload error.</h3>)
	}
  }
  def view(body: scala.xml.NodeSeq) = {
    Html(
     <html>
      <head>
        <title>bitshow</title>
        <link rel="stylesheet" type="text/css" href="/assets/css/app.css"/>
        <script src="/assets/js/jquery-1.6.2.min.js" />
      </head>
      <body>
       <div id="container">
       { body }
       </div>
     </body>
    </html>
   )
  }
}
