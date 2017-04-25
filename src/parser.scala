import scala.io.Source

object parser extends App {
  
  val filename = "resources/program.txt"

  for (line <- Source.fromFile(filename).getLines()) {
    parse(line);
  }

  def parse(line:String) : Int = {
  	/*val aSplit = line.split(" ")
  	try{
  		val temp = aSplit[0].toInt
  	}
  	catch {
  		case _: println("not a number")
  	}
  	//println(line.length());*/
  	//Curent theory is that we will be able to use this regex match=>case to divide up the parsing into various methods that we can use
  	//to parse the individual vocab
  	line match {
  		case s if s.matches("\\A[0-9].") => println("list declaration")
  		case s if s.matches(".[:].") => println("assignment")
  		case s if s.matches("\\znext!") => println("endblock")
  		case _ => println("doesn't match any known line syntax: error")
  	}

  	0
  }

  /*def parseMatch(x:String) : String = x match {
  	//case ""
  }*/
}