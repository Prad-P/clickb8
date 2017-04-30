import scala.io.Source
import scala.collection.mutable
import scala.util.parsing.combinator._
import scala.language.implicitConversions

object evaluator extends App{

	//State:
	//var float_vars:Map[String,Float] = Map();
	var boolean_vars = mutable.Map.empty[String, Boolean]
	var string_vars = mutable.Map.empty[String, String]
	var integer_vars = mutable.Map.empty[String, Int]

	//var float_lists:Map[String,Array[Float]] = Map();
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
		var tokens:Array[String] = line.split(" ");
		evaluate(tokens);
	}

	def evaluate(tokens:Array[String]) : Int = tokens(0) match{
		case "add" => println(add(tokens));0;
		case "sub" => println(sub(tokens));0;
		case "mult" => println(mult(tokens));0;
		case "div" => println(div(tokens));0;
		case "declare_list" => println(declare_list(tokens));0;
		case _ => println("no match :(");-1;
	}

	//Declarations token(1)= list/var name, token(2) = type, (lists only) token(3) = size
	def declare_list(tokens:Array[String]): Int = tokens(2) match{
		
		case "Int" => integer_lists(tokens(1)) = new Array[Int](tokens(3).toInt);0;
		case "Boolean" => boolean_lists(tokens(1)) = new Array[Boolean](tokens(3).toInt);0;
		case "String" => string_lists(tokens(1)) = new Array[String](tokens(3).toInt);0;
		case _ => println("no match :(");-1;
	}

	def declare_var(tokens:Array[String]): Int = tokens(2) match{

		case "Int" => integer_vars(tokens(1)) = 0;0;
		case "Boolean" => boolean_vars(tokens(1)) = false;0;
		case "String" => string_vars(tokens(1)) = "";0;
		case _ => println("no match :(");-1;
	}

	//Arithmetic functions: token(0) = function, token(1),token(2) = 1 op 2, token(3)=storage variable
	def add(tokens:Array[String]) : Int = {
		
		var var_name:String = tokens(3)
		var sum:Int = (tokens(1).toInt + tokens(2).toInt);

		if(!var_name.contains('['))
			integer_vars(var_name) = sum;
		else{
			var list_name:String = var_name.split('[')(0)
			var index:Int = (var_name.substring(var_name.indexOf("[") + 1, var_name.indexOf("]"))).toInt;
			(integer_lists(list_name))(index) = sum;
		}
		sum;
	}

	def sub(tokens:Array[String]) : Int = {
		
		var var_name:String = tokens(3)
		var diff:Int = (tokens(1).toInt - tokens(2).toInt);

		if(!var_name.contains('['))
			integer_vars(var_name) = diff;
		else{
			var list_name:String = var_name.split('[')(0)
			var index:Int = (var_name.substring(var_name.indexOf("[") + 1, var_name.indexOf("]"))).toInt;
			(integer_lists(list_name))(index) = diff;
		}
		diff;
	}

	def mult(tokens:Array[String]) : Int = {
		
		var var_name:String = tokens(3)
		var product:Int = (tokens(1).toInt * tokens(2).toInt);

		if(!var_name.contains('['))
			integer_vars(var_name) = product;
		else{
			var list_name:String = var_name.split('[')(0)
			var index:Int = (var_name.substring(var_name.indexOf("[") + 1, var_name.indexOf("]"))).toInt;
			(integer_lists(list_name))(index) = product;
		}
		product;
	}

	def div(tokens:Array[String]) : Int = {

		var var_name:String = tokens(3)
		var quotient:Int = (tokens(1).toInt / tokens(2).toInt);

		if(!var_name.contains('['))
			integer_vars(var_name) = quotient;
		else{
			var list_name:String = var_name.split('[')(0)
			var index:Int = (var_name.substring(var_name.indexOf("[") + 1, var_name.indexOf("]"))).toInt;
			(integer_lists(list_name))(index) = quotient;
		}
		quotient;
	}

}