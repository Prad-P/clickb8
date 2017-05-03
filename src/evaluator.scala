import scala.io.Source
import scala.collection.mutable
import scala.util.parsing.combinator._
import scala.language.implicitConversions

object evaluator extends App{

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


	var input_file:String = null;
	var instructions: Array[String] = new Array(MAX_LINES);

	//control variable detects changes in control flow (loops,conditionals)
	var control:Int = 0;
	var loop_start:Int = 0;
	var while_condition:String = "";
	var if_condition:String = "";

	try{
    	input_file = args(0);
  	}
	catch{
		case noarg:ArrayIndexOutOfBoundsException=>{println("User failed to provide input file");System.exit(0);}
	}
	
	for (line <- Source.fromFile(input_file).getLines()){
 		instructions(control) = (line);
 		control+=1;

	}

	control = 0;

	while(control < instructions.length && instructions(control)!=null){
		
		var state_var = evaluate(instructions(control).split(","));

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

				while(instructions(control).split(",")(0) != "endif" && instructions(control).split(",")(0) != "else"){
					
					println(instructions(control).split(",")(0));

					state_var = evaluate(instructions(control).split(","));
					control+=1;
				}
			
				while(instructions(control).split(",")(0) != "endif"){

					control+=1;
				}
			}
			else{
				while(instructions(control).split(",")(0) != "endif" && instructions(control).split(",")(0) != "else"){

					control+=1;
				}
			}
			state_var = 0;

		}

		control+=1;
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

		case _ => println("no match :(");-1;
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

		var boolean_str:String = (getInt(tokens(1)) == getInt(tokens(2))).toString;
		var var_name:String = tokens(3);

		if(!var_name.contains('['))
			assign_var(Array(" ",var_name,boolean_str));
		else{
			var list_name:String = var_name.split('[')(0)
			var index:String = (var_name.substring(var_name.indexOf("[") + 1, var_name.indexOf("]")));
			assign_list(Array(" ",var_name.split('[')(0),boolean_str,index));
		}
		0;
		
	}

	def lesser_than(tokens:Array[String]) : Int ={

		var boolean_str:String = (getInt(tokens(1)) < getInt(tokens(2))).toString;
		var var_name:String = tokens(3);

		if(!var_name.contains('['))
			assign_var(Array(" ",var_name,boolean_str));
		else{
			var list_name:String = var_name.split('[')(0)
			var index:String = (var_name.substring(var_name.indexOf("[") + 1, var_name.indexOf("]")));
			assign_list(Array(" ",var_name.split('[')(0),boolean_str,index));
		}
		0;
		
	}

	def greater_than(tokens:Array[String]) : Int ={

		var boolean_str:String = (getInt(tokens(1)) > getInt(tokens(2))).toString;
		var var_name:String = tokens(3);

		if(!var_name.contains('['))
			assign_var(Array(" ",var_name,boolean_str));
		else{
			var list_name:String = var_name.split('[')(0)
			var index:String = (var_name.substring(var_name.indexOf("[") + 1, var_name.indexOf("]")));
			assign_list(Array(" ",var_name.split('[')(0),boolean_str,index));
		}
		0;
		
	}

	//conditionals tokens(1),tokens(2) bool vals, tokens(3) = storage / for not tokens(2) = storage
	def eval_or(tokens:Array[String]) : Int ={

		var boolean:Boolean = getBool(tokens(1)) || getBool(tokens(2));
		var var_name:String = tokens(3);

		var boolean_str:String = (boolean).toString;

		if(!var_name.contains('['))
			assign_var(Array(" ",var_name,boolean_str));
		else{
			var list_name:String = var_name.split('[')(0)
			var index:String = (var_name.substring(var_name.indexOf("[") + 1, var_name.indexOf("]")));
			assign_list(Array(" ",var_name.split('[')(0),boolean_str,index));
		}
		0;

	}

	def eval_and(tokens:Array[String]) : Int ={

		var boolean:Boolean = getBool(tokens(1)) && getBool(tokens(2));
		var var_name:String = tokens(3);

		var boolean_str:String = (boolean).toString;

		if(!var_name.contains('['))
			assign_var(Array(" ",var_name,boolean_str));
		else{
			var list_name:String = var_name.split('[')(0)
			var index:String = (var_name.substring(var_name.indexOf("[") + 1, var_name.indexOf("]")));
			assign_list(Array(" ",var_name.split('[')(0),boolean_str,index));
		}
		0;

	}

	def eval_not(tokens:Array[String]) : Int ={

		var boolean:Boolean = getBool(tokens(1));
		var var_name:String = tokens(2);

		var boolean_str:String = (!boolean).toString;

		if(!var_name.contains('['))
			assign_var(Array(" ",var_name,boolean_str));
		else{
			var list_name:String = var_name.split('[')(0)
			var index:String = (var_name.substring(var_name.indexOf("[") + 1, var_name.indexOf("]")));
			assign_list(Array(" ",var_name.split('[')(0),boolean_str,index));
		}
		0;

	}

	//tokens(1) == boolean var, if true loop
	def eval_while(tokens:Array[String]) : Int ={

		while_condition = tokens(1);


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
			assign_var(Array(" ",var_name,sum));
		else{
			var list_name:String = var_name.split('[')(0)
			var index:String = (var_name.substring(var_name.indexOf("[") + 1, var_name.indexOf("]")));
			assign_list(Array(" ",var_name.split('[')(0),sum,index));
		}
		sum.toInt;
	}

	def sub(tokens:Array[String]) : Int = {
		
		var var_name:String = tokens(3)

		var diff:String = (getInt(tokens(1)) - getInt(tokens(2))).toString;

		if(!var_name.contains('['))
			assign_var(Array(" ",var_name,diff));
		else{
			var list_name:String = var_name.split('[')(0)
			var index:String = (var_name.substring(var_name.indexOf("[") + 1, var_name.indexOf("]")));
			assign_list(Array(" ",var_name.split('[')(0),diff,index));
		}
		diff.toInt;
	}

	def mult(tokens:Array[String]) : Int = {
		
		var var_name:String = tokens(3)
		
		var product:String = (getInt(tokens(1)) * getInt(tokens(2))).toString;

		if(!var_name.contains('['))
			assign_var(Array(" ",var_name,product));
		else{
			var list_name:String = var_name.split('[')(0)
			var index:String = (var_name.substring(var_name.indexOf("[") + 1, var_name.indexOf("]")));
			assign_list(Array(" ",var_name.split('[')(0),product,index));
		}
		product.toInt;
	}

	def div(tokens:Array[String]) : Int = {

		var var_name:String = tokens(3)
		
		var quotient:String = (getInt(tokens(1)) / getInt(tokens(2))).toString;

		if(!var_name.contains('['))
			assign_var(Array(" ",var_name,quotient));
		else{
			var list_name:String = var_name.split('[')(0)
			var index:String = (var_name.substring(var_name.indexOf("[") + 1, var_name.indexOf("]")));
			assign_list(Array(" ",var_name.split('[')(0),quotient,index));
		}
		quotient.toInt;
	}

	def isAllDigits(x: String) = x forall Character.isDigit

	def getInt(x: String) : Int = {

		var ret:Int = -1;

		if(isAllDigits(x))
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
