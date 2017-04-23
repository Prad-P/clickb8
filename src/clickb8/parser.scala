import scala.io.Source

object parser extends App {
  
  val filename = "resources/program.txt"

  for (line <- Source.fromFile(filename).getLines()) {
    parse(line);
  }

  def parse(line:String) : Int = {
  	println(line.length());
  	0
  }
}