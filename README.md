the most incredible article about programming language you'll ever read

Clickb8 a disruptive programming language that millennial can resist.

A parser that reads from our proprietary .cb8 source files
An evaluator that allows for loops, conditionals, arithmetic, logical operators and much more!
Employs a type system
An external Scala DSL and utilizes Scala’s pattern matching mechanism

How to run a Clickb8 Program:
First, compile the file clickb8runner.scala using the scalac command
Next, run your .cb8 file with the command "scala clickb8runner FILE_NAME"
That's it!

Writing in Clickb8:
Clickb8 is an incredibly easy to comprehend language with syntax that reads just like plain English!

Variable Declaration: “Why”, following word will be the variable name, then followed by type
	Variable types:
		“Is” -> Int
		“Brings” -> Boolean
		“Still” -> String
Example: “Here's Why Coke Is the greatest soda of all time”

Variable Assignment: Variable Name, followed by “:” then the assignment expression
Expression Forms:
	Factor
		Factor can be:
		A Number
		A Variable Name
		An index call to an array
			Takes the form Index + “Times” + Array Name
	Factor + operator + Factor
		Operators:
			“Add” -> +
			“Stop” -> -
			“Common” -> *
			“Seperates” -> /
	Factor + logical operator + Factor
		Logical Operators:
			“And”
			“Or”
			“Not”
				Takes the form operator + Factor
	Factor + comparator + Factor
		Comparators:
			“Greatest” -> >
			“Worst” -> <
			“As” -> ==
	String in quotes
	Note about operator order:
		In the "English" form of Clickb8, it is recommended that the operator be placed between the two factors to avoid confusion, howeve the parser can handle the operator being placed anywhere in the statement. The factors WILL be read in order.
Example: “Diet Coke: The 10 Best Ways to Incorporate it into your Cooking”

Array Declaration: Starts with Number(Array Length) + filler word + Variable Name + Type
Example: “10 Pictures Sprite Is Trying To Remove From The Internet”

Array Index Assignment: Index number must appear anywhere after the first word and before the Variable + Colon.
Example: “The 5 Commandments of the CEO of  Sprite: Number 5 will shock you!

If Statement: Boolean Name + “?”
Can have an else in the form of any line ending in a period
Has an endblock that ends with “out!”
Example:
Can Triple Stuf save Oreo?
…
We asked 10 Doctors what toothpaste they actually recommend.
…
Does toothpaste cause Cancer? Click here to find out!

While Loop: Contains “while” + Boolean Name
Terminated with endblock ending in “next!”
Example:
Ten ways to make money while Walmart destroys your local economy
…
This little boy smoked three packs of cigarettes a day, you won’t believe what happens next!

String operations:
	Length: If the phrase "Long" is found in the statement, the parser will find two variable names in the statement. It will assume that the first one it finds is the String to find the length and the second is the the integer variable that the length is to be stored in.
		Example: "We've waited so Long but it's finally here, Oreo is coming out with a Hotdog flavor"
			"Oreo" is the string that we are finding the length of, and Hotdog is the Int variable that we are using to store the length.
	Tokenize: If the phrase "Breaks" is found in the statement, then the parser will find two variable names in the statement. It will assume that the first one it finds is the String to be tokenized, and that the second one is a list to be assigned the Array of characters.
		Note: The list to be assigned must be declared beforehand, but the length of said Array doesn't matter as the Tokenize assignment will reset the length of the List.
		Example: "Whole Foods Breaks with tradition and decides it will make an Oreo flavored Pringles"
			"Oreo" is the string to tokenized, "Pringles" is the list that it will be assigned to.