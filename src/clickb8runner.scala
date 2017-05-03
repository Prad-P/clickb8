//package src;
import scala.io.Source
import parser._

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

    val parsero = new parser(input_file)
    parsero.startParse

}