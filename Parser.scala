import scala.collection.immutable.{ Map, Set }
import scala.util.parsing.combinator.{ RegexParsers, Parsers }
import scala.util.parsing.input._
import scala.language.implicitConversions

object SearchOperatorsParser extends RegexParsers {
	type Operators = Map[String, Set[String]]

	def Operators() = Map[String, Set[String]]()

	def Operators(a: String, b: Set[String]) = {
		Map[String, Set[String]](a -> b)
	}
	def merge(a: Operators, b: Operators): Operators = {
		a ++ b.map { case (k, v) => k -> (a.getOrElse(k, Set()) ++ v) }
	}

	def ident = """[^\s,:，：]+""".r

	def values: Parser[Set[String]] = ident ~ rep(",|，".r ~> ident) ^^ {
		//Question: why `Set(value) /: list` would fail?
		//Answer: The parenthethis are needed due to rules of precedence.
		case value ~ list => (Set(value) /: list) {
			case (values, value) => values + value
		}
	}

	def kvPair: Parser[Operators] = (ident <~ ":|：".r) ~ values ^^ {
		case key ~ values =>
			Operators(key, values)

	}

	//Question: why `ident | kvPair` would fail ?
	def operators: Parser[Operators] = rep(kvPair | ident) ^^ {
		case list => (Operators() /: list) {
			case (operators, plaintext: String) => merge(operators, Operators("plaintext", Set(plaintext)))
			case (operators, kvPair: Operators) => merge(operators, kvPair)
		}
	}

	def apply(input: String): Operators = parseAll(operators, input) match {
		case Success(result, _) => result
		case failure: NoSuccess => scala.sys.error(failure.msg)
	}
}
