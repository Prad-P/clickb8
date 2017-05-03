//package src;
import scala.io.Source
import scala.util.matching.Regex

object parser extends App {
  
    //contains name of input file
    var input_file:String = null;

    val varNameList = new Array[String](1000);
    var arrIter = 0;
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

    /*def startParse() {
    	for (line <- Source.fromFile(filename).getLines()) {
    		//ignore comments
    		if(!line.startsWith("//"))
    			parse(line);
    	}
    }*/

    def parse(line:String) : Int = {
    	//Possible TODO::
    	//Figure out how to use the match case in scala with regex so that this bit of code can look nicer
    	//but it works for now
    	if(line.matches("^(\\d).*$"))
    		println(parseListDeclaration(line));

    	if(line.matches(".*:.*"))
    		println(parseAssignment(line));

    	if(line.matches(".*next!"))
    		println(parseWEndblock(line));

    	if(line.matches(".*out!"))
    		println(parseEndblock(line));

    	if(line.matches(".*(\\?)"))
    		println(parseIfStatement(line));

    	if(line.matches(".*(\\.)"))
    		println(parseElseStatement(line));

    	if(line.matches(".*(W|w)hile.*"))
  			println(parseWhile(line));

  		if(line.matches(".*(W|w)hy.*"))
  			println(parseDeclaration(line));

  		if(line.matches(".*(S|s)ays.*"))
  			println(parseWriteVar(line));

  		if(line.matches(".*(T|t)hinks.*"))
  			println(parseReadLine(line));

  		0
  	}

  	def parseDeclaration(line:String) : String = {
  		val lineSplit = line.split(" ")
  		var vtype = ""
  		var temp = 0
  		while (!lineSplit(temp).matches("(W|w)hy")) {
  			temp += 1;
  		}
  		val varName = lineSplit(temp+1)
  		val upType = lineSplit(temp+2)
	  	//gotta do something with the variable name but for now I'm just gonna return it as a string
	  	varNameList(arrIter) = varName
	  	arrIter += 1
	  	upType match {
	  		case "Is" => vtype = "Int"
	  		case "Brings" => vtype = "Boolean"
	  		case "Still" => vtype = "String"
	  		case _ => vtype = "null"
	  	}
	  	("declare_var," + varName + "," + vtype)
	}

	def parseAssignment(line:String) : String = {
	  	val lineSplit = line.split(" ")
	  	val postColon = line.split(":") // this will take the second half of the string, i.e. the portion that needs to be eval for assignment
	  	var ind = -1
	  	var temp = 0
	  	while(!lineSplit(temp).matches(".*:")) {
	  		if(lineSplit(temp).matches("\\d+")) {
	  			ind = lineSplit(temp).toInt
	  		}
	  		temp += 1;
	  	}
	  	val varName = lineSplit(temp).substring(0,lineSplit(temp).length-1)
	  	//will need a call to a parser to parse the expression
	  	val pExpr = parseExpression(postColon(1))
	  	if(ind >= 0) {
	  		//("assign_list," + varName + "," + pExpr + "," + ind);
	  		(pExpr + "," + varName + "," + ind)
	  	}
	  	else {
	  		(pExpr + "," + varName)
	  	}
	}

	def parseExpression(expr:String) : String = {
	  	val exprSplit = expr.split(" ")
	  	var factorList = new Array[String](2)
	  	var factorLen = 0
	  	var operator:String = null
	  	var n = 0
	  	var token = ""
	  	//exprSplit.foreach { i:String =>
	  	if(!expr.matches(" .*\".*\".*")) {
	  		while (n < exprSplit.length) {
	  			val i:String = exprSplit(n)
		  		if((factorLen < 2) && varNameList.contains(i)) {
		  			factorList(factorLen) = i
		  			factorLen += 1
		  		}
		  		if((factorLen < 2) && i.matches("\\d+")) {
		  			if(exprSplit(n+1).matches("(T|t)imes")) {
		  				factorList(factorLen) = exprSplit(n+2) + ":" + exprSplit(n)
		  				factorLen += 1
		  			}
		  			else {
		  				factorList(factorLen) = i
		  				factorLen += 1
		  			}
		  		}
		  		if((factorLen < 2) && i.matches("LOVE(S)*")) {
		  			factorList(factorLen) = "true"
		  			factorLen += 1
		  		}
		  		if((factorLen < 2) && i.matches("HATE(S)*")) {
		  			factorList(factorLen) = "false"
		  			factorLen += 1
		  		}
		  		if(i.matches("Add")) {
		  			operator = "add"
		  		}
		  		if(i.matches("Stop")) {
		  			operator = "sub"
		  		}
		  		if(i.matches("Common")) {
		  			operator = "mult"
		  		}
		  		if(i.matches("Seperates")) {
		  			operator = "div"
		  		}
		  		if(i.matches("And")) {
		  			operator = "and"
		  		}
		  		if(i.matches("Or")) {
		  			operator = "or"
		  		}
		  		if(i.matches("Not")) {
		  			operator = "not"
		  		}
		  		if(i.matches("Greatest")) {
		  			operator = "greater_than"
		  		}
		  		if(i.matches("Worst")) {
		  			operator = "lesser_than"
		  		}
		  		if(i.matches("As")) {
		  			operator = "equal_to"
		  		}
		  		n+=1
		  	}
	  	}
	  	else {
	  		val stringExtract = expr.split("\"")
	  		token = "assign_var," + stringExtract(1)
	  	}
	  	if(operator != null && operator.matches("not")) {
	  		token = operator + "," + factorList(0)
	  	}
	  	else if(factorLen == 2) {
	  		token = operator + "," + factorList(0) + "," + factorList(1)
	  	}
	  	else if(factorList(0) != null) {
	  		token = "assign_var," + factorList(0)
	  	}
	  	token
	}

	def parseEndblock(line:String) : String = {
	  	//this doesn't really do anything yet because it's just an endblock
	  	"endif"
	}

	def parseWEndblock(line:String) : String = {
		"endwhile"
	}

	def parseIfStatement(line:String) : String = {
	  	val lineSplit = line.split(" ")
	  	val varName = lineSplit(lineSplit.length-1).split("\\?")(0)
	  	//will just have to pass the boolean variable name to the evaluator which will handle the rest
	  	("if," + varName)
	}

	def parseElseStatement(line:String) : String = {
		"else"
	}

	def parseWhile(line:String) : String = {
	  	val lineSplit = line.split(" ")
	  	var temp = 0
	  	while(!lineSplit(temp).matches("(W|w)hile")) {
	  		temp += 1;
	  	}
	  	val varName = lineSplit(temp+1)
	  	//same as with If Statement
	  	("while," + varName)
	}
	def parseListDeclaration(line:String) : String = {
	  	val lineSplit = line.split(" ")
	  	val listLength = lineSplit(0)
	  	val listName = lineSplit(2)
	  	val upType = lineSplit(3)
	  	var listType = ""
	  	upType match {
	  		case "Is" => listType = "Int"
	  		case "Brings" => listType = "Boolean"
	  		case "Still" => listType = "String"
	  	}
	  	("declare_list," + listName + "," + listType + "," + listLength)
	}

	def parseWriteVar(line:String) : String = {
	  	val lineSplit = line.split(" ")
	  	var temp = 0
	  	while (!lineSplit(temp).matches("(S|s)ays")) {
	  		temp += 1;
	  	}
	  	val varName = lineSplit(temp-1)
	  	//gotta do something with the variable name but for now I'm just gonna return it as a string
	  	("write_line," + varName)

	}

	def parseReadLine(line:String) : String = {
	  	val lineSplit = line.split(" ")
	  	var temp = 0
	  	while (!lineSplit(temp).matches("(T|t)hinks")) {
	  		temp += 1;
	  	}
	  	val varName = lineSplit(temp-1)
	  	//gotta do something with the variable name but for now I'm just gonna return it as a string
	  	("read_line," + varName)
	}
}