import scala.collection.immutable.{ Map, Set }
import scala.util.parsing.combinator.{ JavaTokenParsers, Parsers}
import scala.util.parsing.input._
import scala.language.implicitConversions

object SearchOperatorsParser extends JavaTokenParsers {
	type Operators = Map[String, Set[String]]
	
	def Operators() = Map[String, Set[String]]()

	def Operators(a: String, b: Set[String]) = {
		Map[String, Set[String]](a -> b)
	}	
	def merge(a : Operators, b : Operators) : Operators = {
		a ++ b.map{case (k, v) => k -> (v ++ a.getOrElse(k, Set()))}	
	}

	def values : Parser[Set[String]] = stringLiteral ~ rep( "," ~ stringLiteral ) ^^ {
		case value ~ list => Set(value) /: list {
			case (values, "," ~ value) => values + value
		} 		
	}
	
	def kvPair : Parser[Operators] = stringLiteral ~ ":" ~ values ^^ {
		case key ~ ":" ~ values => 
			Operators(key, values)
		
	}


	def operators : Parser[Operators] = rep( stringLiteral | kvPair ) ^^ {
		case list => Operators() /: list {
			case (operators, plaintext: String) => merge(operators, Operators("plaintext", Set(plaintext)))
			case (operators, kvPair: Operators) => merge(operators, kvPair)
		} 
	}

     def apply(input: String): Operators = parseAll(operators, input) match {
       case Success(result, _) => result
       case failure : NoSuccess => scala.sys.error(failure.msg)
     }
}
