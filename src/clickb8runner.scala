//package src;
import scala.io.Source
import scala.util.matching.Regex
import scala.collection.mutable
import scala.util.parsing.combinator._
import scala.language.implicitConversions

object clickb8runner extends App {

	//contains name of input file
    var input_file:String = null;

    try{
    	input_file = args(0);//"resources/program.txt"
    }
    catch{
    	case noarg:ArrayIndexOutOfBoundsException=>{println("User failed to provide input file");System.exit(0);}
    }
    /*for (line <- Source.fromFile(input_file).getLines()) {
    	//ignore comments
    	if(!line.startsWith("//"))
    		parsero.parse(line);
    }*/

    val parserc = new parser(input_file)
    val parsedcalls = parserc.startParse()
    //println(parsedcalls)
    val evalc = new evaluator()
    evalc.startEval(parsedcalls)

}

class parser(filename:String) {
  
    //contains name of input file
    var input_file:String = filename;

    val varNameList = new Array[String](1000);
    var arrIter = 0;
    /*try{
    	input_file = args(0);//"resources/program.txt"
    }
    catch{
    	case noarg:ArrayIndexOutOfBoundsException=>{println("User failed to provide input file");System.exit(0);}
    }
    for (line <- Source.fromFile(input_file).getLines()) {
    	//ignore comments
    	if(!line.startsWith("//"))
    	parse(line);
    }*/

    def startParse() : String = {
    	var returnStr = ""
    	for (line <- Source.fromFile(filename).getLines()) {
    		//ignore comments
    		if(!line.startsWith("//"))
    			returnStr = returnStr + parse(line) + "\t";
    	}
    	returnStr
    }

    def parse(line:String) : String = {
    	//Possible TODO::
    	//Figure out how to use the match case in scala with regex so that this bit of code can look nicer
    	//but it works for now
    	var returnStr = ""
    	if(line.matches("^(\\d).*$"))
    		//println(parseListDeclaration(line));
    		returnStr = parseListDeclaration(line);

    	if(line.matches(".*:.*"))
    		//println(parseAssignment(line));
    		returnStr = parseAssignment(line);

    	if(line.matches(".*next!"))
    		//println(parseWEndblock(line));
    		returnStr = parseWEndblock(line);

    	if(line.matches(".*out!"))
    		//println(parseEndblock(line));
    		returnStr = parseEndblock(line);

    	if(line.matches(".*(\\?)"))
    		//println(parseIfStatement(line));
    		returnStr = parseIfStatement(line);

    	if(line.matches(".*(\\.)"))
    		//println(parseElseStatement(line));
    		returnStr = parseElseStatement(line);

    	if(line.matches(".*(W|w)hile.*"))
  			//println(parseWhile(line));
  			returnStr = parseWhile(line);

  		if(line.matches(".*(W|w)hy.*"))
  			//println(parseDeclaration(line));
  			returnStr = parseDeclaration(line);

  		if(line.matches(".*(S|s)ays.*"))
  			//println(parseWriteVar(line));
  			returnStr = parseWriteVar(line);

  		if(line.matches(".*(T|t)hinks.*"))
  			//println(parseReadLine(line));
  			returnStr = parseReadLine(line);

  		if(line.matches(".*Break(s)*.*"))
  			returnStr = parseTokenize(line);

  		if(line.matches(".*Long.*"))
  			returnStr = parseLength(line);

  		println(returnStr)
  		returnStr
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
	  	("declare_var`" + varName + "`" + vtype)
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
	  		(pExpr + "`" + varName + "`" + ind)
	  	}
	  	else {
	  		(pExpr + "`" + varName)
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
		  			if(n+1 < exprSplit.length && exprSplit(n+1).matches("(T|t)imes")) {
		  				factorList(factorLen) = exprSplit(n+2) + "[" + exprSplit(n) + "]"
		  				factorLen += 1
		  			}
		  			else {
		  				factorList(factorLen) = i
		  				factorLen += 1
		  			}
		  		}
		  		if((factorLen < 2) && i.matches("\\d+")) {
		  			if(exprSplit(n+1).matches("(T|t)imes")) {
		  				factorList(factorLen) = exprSplit(n+2) + "[" + exprSplit(n) + "]"
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
	  		token = "assign_var`" + stringExtract(1)
	  	}
	  	if(operator != null && operator.matches("not")) {
	  		token = operator + "`" + factorList(0)
	  	}
	  	else if(factorLen == 2) {
	  		token = operator + "`" + factorList(0) + "`" + factorList(1)
	  	}
	  	else if(factorList(0) != null) {
	  		token = "assign_var`" + factorList(0)
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
	  	("if`" + varName)
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
	  	("while`" + varName)
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
	  	("declare_list`" + listName + "`" + listType + "`" + listLength)
	}

	def parseWriteVar(line:String) : String = {
	  	val lineSplit = line.split(" ")
	  	var temp = 0
	  	while (!lineSplit(temp).matches("(S|s)ays")) {
	  		temp += 1;
	  	}
	  	val varName = lineSplit(temp-1)
	  	//gotta do something with the variable name but for now I'm just gonna return it as a string
	  	("write_line`" + varName)

	}

	def parseReadLine(line:String) : String = {
	  	val lineSplit = line.split(" ")
	  	var temp = 0
	  	while (!lineSplit(temp).matches("(T|t)hinks")) {
	  		temp += 1;
	  	}
	  	val varName = lineSplit(temp-1)
	  	//gotta do something with the variable name but for now I'm just gonna return it as a string
	  	("read_line`" + varName)
	}

	def parseTokenize(line:String) : String = {
		val lineSplit = line.split(" ")
		var varNames = new Array[String](2)
		var ind = 0
		var temp = 0
		while(temp < lineSplit.length) {
			if(varNameList.contains(lineSplit(temp))) {
				varNames(ind) = lineSplit(temp)
				ind += 1
			}
			temp += 1
		}
		//varName = lineSplit(temp)
		("tokenize`" + varNames(0) + "`" + varNames(1))
	}

	def parseLength(line:String) : String = {
		val lineSplit = line.split(" ")
		var varNames = new Array[String](2)
		var ind = 0
		var temp = 0
		while(temp < lineSplit.length) {
			if(varNameList.contains(lineSplit(temp))) {
				varNames(ind) = lineSplit(temp)
				ind += 1
			}
			temp += 1
		}
		//varName = lineSplit(temp)
		("length`" + varNames(0) + "`" + varNames(1))
	}
}

class evaluator{

	//State:
	val MAX_LINES = 1024;

	//maps var names to types
	var type_map = mutable.Map.empty[String, String];

	//variables
	var boolean_vars = mutable.Map.empty[String, Boolean];
	var string_vars = mutable.Map.empty[String, String];
	var integer_vars = mutable.Map.empty[String, Int];

	//lists
	var boolean_lists = mutable.Map.empty[String, Array[Boolean]];
	var string_lists = mutable.Map.empty[String, Array[String]];
	var integer_lists = mutable.Map.empty[String, Array[Int]];
	//End of state


	//var input_file:String = null;
	//var instructions: Array[String] = new Array(MAX_LINES);

	//control variable detects changes in control flow (loops,conditionals)
	var control:Int = 0;
	var loop_start:Int = 0;
	var while_condition:String = "";
	var if_condition:String = "";

	/*try{
    	input_file = args(0);
  	}
	catch{
		case noarg:ArrayIndexOutOfBoundsException=>{println("User failed to provide input file");System.exit(0);}
	}
	
	for (line <- Source.fromFile(input_file).getLines()){
 		instructions(control) = (line);
 		control+=1;

	}*/
	def startEval(commands:String) {


		val instructions = commands.split("\t")

		while(control < instructions.length && instructions(control)!=null){
			
			//println(instructions(control));
			var state_var = evaluate(instructions(control).split("`"));

			//1=loop,2=endloop
			if(state_var == 1)
				loop_start=control;
			else if(state_var == 2){
				if(boolean_vars(while_condition) == true)
					control = loop_start;
			}

			//3=if

			if(state_var == 3){

				if(boolean_vars(if_condition) == true){

					while(instructions(control).split("`")(0) != "endif" && instructions(control).split("`")(0) != "else"){
						
						println(instructions(control).split("`")(0));

						state_var = evaluate(instructions(control).split("`"));
						control+=1;
					}
				
					while(instructions(control).split("`")(0) != "endif"){

						control+=1;
					}
				}
				else{
					while(instructions(control).split("`")(0) != "endif" && instructions(control).split("`")(0) != "else"){

						control+=1;
					}
				}
				state_var = 0;

			}

			control+=1;
		}
	}

	//tokens(0) is the function
	def evaluate(tokens:Array[String]) : Int = tokens(0) match{
		case "add" => add(tokens);0;
		case "sub" => sub(tokens);0;
		case "mult" => mult(tokens);0;
		case "div" => div(tokens);0;
		case "declare_list" => declare_list(tokens);0;
		case "declare_var" => declare_var(tokens);0;
		case "assign_list" => assign_list(tokens);0;
		case "assign_var" => assign_var(tokens);0;
		case "read_line" => read_line(tokens);0;
		case "write_line" => write_line(tokens);0;
		case "while" => eval_while(tokens);1;
		case "endwhile" => 2;
		case "not" => eval_not(tokens);0;
		case "or" => eval_or(tokens);0;
		case "and" => eval_and(tokens);0;
		case "greater_than" => greater_than(tokens);0;
		case "lesser_than" => lesser_than(tokens);0;
		case "equal_to" => equal_to(tokens);0;
		case "if" => eval_if(tokens);3;
		case "else" => 0;
		case "endif" => eval_endif(tokens);0;
		case "tokenize" =>eval_tokenize(tokens);0;
		case "length" =>eval_length(tokens);0;

		case _ => println("no match :(");-1;
	}

	//length tokens(1)=string var o literal tokens(2)=int storage
	def eval_length(tokens:Array[String]) : Int ={

		integer_vars(tokens(2)) = getStr(tokens(1)).length();
		0;
	}

	//tokenizer tokens(1)=string var or literals tokens(2)=list of strings that will store our chars
	def eval_tokenize(tokens:Array[String]) : Int ={

		var char_array:Array[String] = getStr(tokens(1)).sliding(1).toArray;

		string_lists(tokens(2)) = char_array;

		0;
	}

	//conditionals tokens(1) = bolean var, else,endif have no args

	def eval_endif(tokens:Array[String]) : Int ={
		if_condition = "";
		0;
	}

	def eval_if(tokens:Array[String]) : Int ={

		if_condition = tokens(1);
		0;
	}

	
	//comparators tokens(1),tokens(2) = 1 compare 2, tokens(3) storage boolean
	def equal_to(tokens:Array[String]) : Int ={

		var boolean_str:String = "false";

		if(type_map(tokens(1))=="Int" && type_map(tokens(2))=="Int"){
			boolean_str = (getInt(tokens(1)) == getInt(tokens(2))).toString;
		}
		else if(type_map(tokens(1))=="Boolean" && type_map(tokens(2))=="Boolean"){
			boolean_str = (getBool(tokens(1)) == getBool(tokens(2))).toString;
		}
		else{
			boolean_str = (getStr(tokens(1)) == getStr(tokens(2))).toString;
		}

		var var_name:String = tokens(3);

		if(!var_name.contains('['))
			assign_var(Array(" ",boolean_str,var_name));
		else{
			var list_name:String = var_name.split('[')(0)
			var index:String = (var_name.substring(var_name.indexOf("[") + 1, var_name.indexOf("]")));
			assign_list(Array(" ",boolean_str,var_name.split('[')(0),index));
		}
		0;
		
	}

	def lesser_than(tokens:Array[String]) : Int ={

		var boolean_str:String = (getInt(tokens(1)) < getInt(tokens(2))).toString;
		var var_name:String = tokens(3);

		if(!var_name.contains('['))
			assign_var(Array(" ",boolean_str,var_name));
		else{
			var list_name:String = var_name.split('[')(0)
			var index:String = (var_name.substring(var_name.indexOf("[") + 1, var_name.indexOf("]")));
			assign_list(Array(" ",boolean_str,var_name.split('[')(0),index));
		}
		0;
		
	}

	def greater_than(tokens:Array[String]) : Int ={

		var boolean_str:String = (getInt(tokens(1)) > getInt(tokens(2))).toString;
		var var_name:String = tokens(3);

		if(!var_name.contains('['))
			assign_var(Array(" ",boolean_str,var_name));
		else{
			var list_name:String = var_name.split('[')(0)
			var index:String = (var_name.substring(var_name.indexOf("[") + 1, var_name.indexOf("]")));
			assign_list(Array(" ",boolean_str,var_name.split('[')(0),index));
		}
		0;
		
	}

	//conditionals tokens(1),tokens(2) bool vals, tokens(3) = storage / for not tokens(2) = storage
	def eval_or(tokens:Array[String]) : Int ={

		var boolean:Boolean = getBool(tokens(1)) || getBool(tokens(2));
		var var_name:String = tokens(3);

		var boolean_str:String = (boolean).toString;

		if(!var_name.contains('['))
			assign_var(Array(" ",boolean_str,var_name));
		else{
			var list_name:String = var_name.split('[')(0)
			var index:String = (var_name.substring(var_name.indexOf("[") + 1, var_name.indexOf("]")));
			assign_list(Array(" ",boolean_str,var_name.split('[')(0),index));
		}
		0;

	}

	def eval_and(tokens:Array[String]) : Int ={

		var boolean:Boolean = getBool(tokens(1)) && getBool(tokens(2));
		var var_name:String = tokens(3);

		var boolean_str:String = (boolean).toString;

		if(!var_name.contains('['))
			assign_var(Array(" ",boolean_str,var_name));
		else{
			var list_name:String = var_name.split('[')(0)
			var index:String = (var_name.substring(var_name.indexOf("[") + 1, var_name.indexOf("]")));
			assign_list(Array(" ",boolean_str,var_name.split('[')(0),index));
		}
		0;

	}

	def eval_not(tokens:Array[String]) : Int ={

		var boolean:Boolean = getBool(tokens(1));
		var var_name:String = tokens(2);

		var boolean_str:String = (!boolean).toString;

		if(!var_name.contains('['))
			assign_var(Array(" ",boolean_str,var_name));
		else{
			var list_name:String = var_name.split('[')(0)
			var index:String = (var_name.substring(var_name.indexOf("[") + 1, var_name.indexOf("]")));
			assign_list(Array(" ",boolean_str,var_name.split('[')(0),index));
		}
		0;

	}

	//tokens(1) == boolean var, if true loop
	def eval_while(tokens:Array[String]) : Int ={

		while_condition = tokens(1);

		//println("while cond is" + tokens(1));


		0;

	}

	//User IO functions token(1) : String var name
	def read_line(tokens:Array[String]) : Int = type_map(tokens(1)) match{

		case "Int" => integer_vars(tokens(1)) = scala.io.StdIn.readLine().toInt;0;
		case "Boolean" => boolean_vars(tokens(1)) = scala.io.StdIn.readLine().toBoolean;0;
		case "String" => string_vars(tokens(1)) = scala.io.StdIn.readLine();0;
		case _ => println("no match :(");-1;
	}

	//Cannot write literals directly! Must use a var)
	def write_line(tokens:Array[String]) : Int = type_map(tokens(1)) match{

		case "Int" => println(getInt(tokens(1)).toString);0;
		case "Boolean" => println(getBool(tokens(1)).toString);0;
		case "String" => println(getStr(tokens(1)));0;
		case _ => println("no match :(");-1;
	}

	//Declarations token(1): list/var name, token(2) = type, (lists only) token(3) = size
	def declare_list(tokens:Array[String]): Int = tokens(2) match{
		
		case "Int" => type_map(tokens(1))= "IntList"; integer_lists(tokens(1)) = new Array[Int](tokens(3).toInt);0;
		case "Boolean" => type_map(tokens(1))= "BooleanList"; boolean_lists(tokens(1)) = new Array[Boolean](tokens(3).toInt);0;
		case "String" => type_map(tokens(1))= "StringList"; string_lists(tokens(1)) = new Array[String](tokens(3).toInt);0;
		case _ => println("no match :(");-1;
	}

	def declare_var(tokens:Array[String]): Int = tokens(2) match{

		case "Int" => type_map(tokens(1))= "Int"; integer_vars(tokens(1)) = 0;0;
		case "Boolean" => type_map(tokens(1))= "Boolean"; boolean_vars(tokens(1)) = false;0;
		case "String" => type_map(tokens(1))= "String"; string_vars(tokens(1)) = "";0;
		case _ => println("no match :(");-1;
	}

	//Direct assignments: tokens(1) = value, tokens(2) = istname tokens(3) = index
	def assign_list(tokens:Array[String]): Int = type_map(tokens(2)) match{

		case "IntList" => integer_lists(tokens(2))(tokens(3).toInt) = getInt(tokens(1));0;
		case "BooleanList" => (boolean_lists(tokens(2)))(tokens(3).toInt) = getBool(tokens(1));0;
		case "StringList" => (string_lists(tokens(2)))(tokens(3).toInt) = getStr(tokens(1));0;
		case _ => println("no match :(");-1;
	}

	def assign_var(tokens:Array[String]): Int = type_map(tokens(2)) match{

		case "Int" => integer_vars(tokens(2)) = getInt(tokens(1));0;
		case "Boolean" => boolean_vars(tokens(2)) = getBool(tokens(1));0;
		case "String" => string_vars(tokens(2)) = getStr(tokens(1));0;
		case _ => println("no match :(");-1;
	}


	//Arithmetic functions assignments: token(0) = function, token(1),token(2) = 1 op 2, token(3)=storage variable
	def add(tokens:Array[String]) : Int = {
		
		var var_name:String = tokens(3)

		var sum:String = (getInt(tokens(1)) + getInt(tokens(2))).toString;

		if(!var_name.contains('['))
			assign_var(Array(" ",sum,var_name));
		else{
			var list_name:String = var_name.split('[')(0)
			var index:String = (var_name.substring(var_name.indexOf("[") + 1, var_name.indexOf("]")));
			assign_list(Array(" ",sum,var_name.split('[')(0),index));
		}
		sum.toInt;
	}

	def sub(tokens:Array[String]) : Int = {
		
		var var_name:String = tokens(3)

		var diff:String = (getInt(tokens(1)) - getInt(tokens(2))).toString;

		if(!var_name.contains('['))
			assign_var(Array(" ",diff,var_name));
		else{
			var list_name:String = var_name.split('[')(0)
			var index:String = (var_name.substring(var_name.indexOf("[") + 1, var_name.indexOf("]")));
			assign_list(Array(" ",diff,var_name.split('[')(0),index));
		}
		diff.toInt;
	}

	def mult(tokens:Array[String]) : Int = {
		
		var var_name:String = tokens(3)
		
		var product:String = (getInt(tokens(1)) * getInt(tokens(2))).toString;

		if(!var_name.contains('['))
			assign_var(Array(" ",product,var_name));
		else{
			var list_name:String = var_name.split('[')(0)
			var index:String = (var_name.substring(var_name.indexOf("[") + 1, var_name.indexOf("]")));
			assign_list(Array(" ",product,var_name.split('[')(0),index));
		}
		product.toInt;
	}

	def div(tokens:Array[String]) : Int = {

		var var_name:String = tokens(3)
		
		var quotient:String = (getInt(tokens(1)) / getInt(tokens(2))).toString;

		if(!var_name.contains('['))
			assign_var(Array(" ",quotient,var_name));
		else{
			var list_name:String = var_name.split('[')(0)
			var index:String = (var_name.substring(var_name.indexOf("[") + 1, var_name.indexOf("]")));
			assign_list(Array(" ",quotient,var_name.split('[')(0),index));
		}
		quotient.toInt;
	}

	def isAllDigits(x: String) = x forall Character.isDigit

	def getInt(x: String) : Int = {

		var ret:Int = -1;


		if(isAllDigits(x) || x(0)=='-' && isAllDigits(x.substring(1)))
			ret = x.toInt;
		else if(x.contains('[')){
			var list_name:String = x.split('[')(0)
			var index:String = (x.substring(x.indexOf("[") + 1, x.indexOf("]")));
			if(integer_vars.contains(index)){
				ret = integer_vars(index);
			}
			else{
				ret = (integer_lists(list_name))(index.toInt);
			}
		}
		else
			ret = integer_vars(x);
		ret;
	}

	def getStr(x: String) : String = {
	
		var ret:String = "";

		if(x.contains('[')){
			var list_name:String = x.split('[')(0)
			var index:String = (x.substring(x.indexOf("[") + 1, x.indexOf("]")));
			if(integer_vars.contains(index)){
				ret = string_vars(index);
			}
			else{
				ret = (string_lists(list_name))(index.toInt);
			}
		}
		else if(string_vars.contains(x))
			ret = string_vars(x);
		else
			ret = x;

		ret;
	}

	def getBool(x: String) : Boolean = {
	
		var ret:Boolean = false;

		if(x.contains('[')){
			var list_name:String = x.split('[')(0)
			var index:String = (x.substring(x.indexOf("[") + 1, x.indexOf("]")));
			if(integer_vars.contains(index)){
				ret = boolean_vars(index);
			}
			else{
				ret = (boolean_lists(list_name))(index.toInt);
			}
		}
		else if(boolean_vars.contains(x))
			ret = boolean_vars(x);
		else
			ret = x.toBoolean;

		ret;
	}

}
