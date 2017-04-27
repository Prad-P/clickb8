import scala.io.Source
import scala.util.matching.Regex

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

  	val LDPattern = "\\A[0-9].".r
  	val APattern = ".[:].".r
  	val EBPattern = "\\znext!".r
  	val IFPattern = "\\z?".r
  	val WPattern = ".while.".r
  	val DPattern = ".*Why.*".r

  	 if(line.matches("([0-9]).*"))
  	 	println(parseListDeclaration(line));

  	if(line.matches(".*:.*"))
  		println(parseAssignment(line));

  	if(line.matches(".*next!"))
  		println(parseEndblock(line));

  	if(line.matches(".*(\\?)"))
  		println(parseIfStatement(line));

  	if(line.matches(".*(W|w)hile.*"))
  		println(parseWhile(line));

  	if(line.matches(".*(W|w)hy.*"))
  		println(parseDeclaration(line));

  	// line match {
  	// 	case LDPattern(line) => println(parseListDeclaration(line))
  	// 	case APattern(line) => println(parseAssignment(line))
  	// 	case EBPattern(line) => println(parseEndblock(line))
  	// 	case IFPattern(line) => println(parseIfStatement(line))
  	// 	case WPattern(line) => println(parseWhile(line))
  	// 	case DPattern(line) => println(parseDeclaration(line))
  	// 	case _ => println("doesn't match any known line syntax: error")
  	// }

  	// line match {
  	// 	case r"\\A[0-9]." => println(parseListDeclaration(line))
  	// 	case r".[:]." => println(parseAssignment(line))
  	// 	case r"\\znext!" => println(parseEndblock(line))
  	// 	case r"\\z?" => println(parseIfStatement(line))
  	// 	case r".while." => println(parseWhile(line))
  	// 	case r".why." => println(parseDeclaration(line))
  	// 	case _ => println("doesn't match any known line syntax: error")
  	// }


  	0
  }

  def parseDeclaration(line:String) : String = {
  	val lineSplit = line.split(" ")
  	var temp = 0
  	while (!lineSplit(temp).matches("(W|w)hy")) {
  		temp += 1;
  	}
  	val varName = lineSplit(temp+1)
  	//gotta do something with the variable name but for now I'm just gonna return it as a string
  	("variable declared :: " + varName)
  }

  def parseAssignment(line:String) : String = {
  	val lineSplit = line.split(" ")
  	val postColon = line.split(":") // this will take the second half of the string, i.e. the portion that needs to be eval for assignment
  	var temp = 0
  	while(!lineSplit(temp).matches(".*:")) {
  		temp += 1;
  	}
  	val varName = lineSplit(temp).substring(0,lineSplit(temp).length-1)
  	//will need a call to a parser to parse the expression
  	varName
  }

  def parseEndblock(line:String) : String = {
  	//this doesn't really do anything yet because it's just an endblock
  	"endblock"
  }

  def parseIfStatement(line:String) : String = {
  	val lineSplit = line.split(" ")
  	val varName = lineSplit(lineSplit.length-1).split("\\?")(0)
  	//will just have to pass the boolean variable name to the evaluator which will handle the rest
  	varName
  }

  def parseWhile(line:String) : String = {
  	val lineSplit = line.split(" ")
  	var temp = 0
  	while(!lineSplit(temp).matches("(W|w)hile")) {
  		temp += 1;
  	}
  	val varName = lineSplit(temp+1)
  	//same as with If Statement
  	varName
  }

  def parseListDeclaration(line:String) : String = {
  	val lineSplit = line.split(" ")
  	val listLength = lineSplit(0).toInt
  	val listName = lineSplit(2)
  	(listName + ":: length = " + listLength)
  }

  /*def parseMatch(x:String) : String = x match {
  	//case ""
  }*/
}