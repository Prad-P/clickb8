import scala.io.Source

object parser extends App {
  
    //contains name of input file
  var input_file:String = null; 

  try{
    input_file = args(0);//"resources/program.txt"
  }
  catch{
    case noarg:ArrayIndexOutOfBoundsException=>{println("User failed to provide input file");System.exit(0);}
  }

  for (line <- Source.fromFile(input_file).getLines()) {
    //ignore comments
    if(!line.startsWith("//"))
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
  		case s if s.matches("\\z?") => println("if statement")
  		//still need to figure out what the syntax for assignment and while loops will be
  		case _ => println("doesn't match any known line syntax: error")
  	}

  	0
  }

  /*def parseMatch(x:String) : String = x match {
  	//case ""
  }*/
}