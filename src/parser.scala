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
  		case s if s.matches(".[:].") => println(parseAssignment(line))
  		case s if s.matches("\\znext!") => println(parseEndblock(line))
  		case s if s.matches("\\z?") => println(parseIfStatement(line))
  		case s if s.matches(".while.") => println(parseWhile(line))
  		case s if s.matches(".why.") => println(parseDeclaration(line))
  		case _ => println("doesn't match any known line syntax: error")
  	}

  	0
  }

  def parseDeclaration(line:String) : String = {
  	val lineSplit = line.split(" ")
  	val temp = 0
  	while (!lineSplit[temp].equals("why")) {
  		temp += 1
  	}
  	val varName = lineSplit[temp+1]
  	//gotta do something with the variable name but for now I'm just gonna return it as a string
  	("variable declared :: " + varName)
  }

  def parseAssignment(line:String) : String = {
  	val lineSplit = line.split(" ")
  	val postColon = line.split(":") // this will take the second half of the string, i.e. the portion that needs to be eval for assignment
  	val temp = 0
  	while(!lineSplit[temp].matches("\\z:")) {
  		temp += 1
  	}
  	val varName = lineSplit[temp].substring(0,-1)
  	//will need a call to a parser to parse the expression
  	varName
  }

  def parseEndblock(line:String) : String = {
  	//this doesn't really do anything yet because it's just an endblock
  	"endblock"
  }

  def parseIfStatement(line:String) : String = {
  	val lineSplit = line.split(" ")
  	val varName = lineSplit[-1].split("?")[0]
  	//will just have to pass the boolean variable name to the evaluator which will handle the rest
  	varName
  }

  def parseWhile(line:String) : String = {
  	val lineSplit = line.split(" ")
  	val temp = 0
  	while(!lineSplit[temp].equals("while")) {
  		temp += 1
  	}
  	val varName = lineSplit[temp+1]
  	//same as with If Statement
  	varName
  }

  /*def parseMatch(x:String) : String = x match {
  	//case ""
  }*/
}