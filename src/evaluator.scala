import scala.io.Source
import scala.collection.mutable
import scala.util.parsing.combinator._
import scala.language.implicitConversions

object evaluator extends App{

	//State:

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

	try{
    	input_file = args(0);
  	}
	catch{
		case noarg:ArrayIndexOutOfBoundsException=>{println("User failed to provide input file");System.exit(0);}
	}
	
	for (line <- Source.fromFile(input_file).getLines()){
		var tokens:Array[String] = line.split(",");
		evaluate(tokens);
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
		case _ => println("no match :(");-1;
	}

	//User IO functions token(1) : String var name
	def read_line(tokens:Array[String]) : String = {

		var var_name:String = tokens(1);
		var input:String = scala.io.StdIn.readLine();


		if(!var_name.contains('['))
			assign_var(Array(" ",var_name,input));
		else{
			var list_name:String = var_name.split('[')(0)
			var index:String = (var_name.substring(var_name.indexOf("[") + 1, var_name.indexOf("]")));
			assign_list(Array(" ",var_name.split('[')(0),input,index));
		}

		input
	}

	//Cannot write literals directly! Must use a var)
	def write_line(tokens:Array[String]) : Int = type_map(tokens(1)) match{

		case "Int" => println(getInt(tokens(1)).toString);0;
		case "Boolean" => println(getBool(tokens(1)));0;
		case "String" => println(getStr(tokens(1)));0;
		case _ => println(type_map(tokens(1)));-1;
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

	//Direct assignments: tokens(1) = listname, tokens(2) = value tokens(3) = index
	def assign_list(tokens:Array[String]): Int = type_map(tokens(1)) match{

		case "IntList" => integer_lists(tokens(1))(tokens(3).toInt) = tokens(2).toInt;0;
		case "BooleanList" => (boolean_lists(tokens(1)))(tokens(3).toInt) = tokens(2).toBoolean;0;
		case "StringList" => (string_lists(tokens(1)))(tokens(3).toInt) = tokens(2);0;
		case _ => println("no match :(");-1;
	}

	def assign_var(tokens:Array[String]): Int = type_map(tokens(1)) match{

		case "Int" => integer_vars(tokens(1)) = tokens(2).toInt;0;
		case "Boolean" => boolean_vars(tokens(1)) = tokens(2).toBoolean;0;
		case "String" => string_vars(tokens(1)) = tokens(2);0;
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
			var index:Int = (x.substring(x.indexOf("[") + 1, x.indexOf("]"))).toInt;
			ret = (integer_lists(list_name))(index);
		}
		else
			ret = integer_vars(x);
		ret;
	}

	def getStr(x: String) : String = {
	
		var ret:String = "";

		if(x.contains('[')){
			var list_name:String = x.split('[')(0)
			var index:Int = (x.substring(x.indexOf("[") + 1, x.indexOf("]"))).toInt;
			ret = (string_lists(list_name))(index);
		}
		else
			ret = string_vars(x);

		ret;
	}

	def getBool(x: String) : Boolean = {
	
		var ret:Boolean = false;

		if(x.contains('[')){
			var list_name:String = x.split('[')(0)
			var index:Int = (x.substring(x.indexOf("[") + 1, x.indexOf("]"))).toInt;
			ret = (boolean_lists(list_name))(index);
		}
		else
			ret = boolean_vars(x);

		ret;
	}

}
