package amyc
package parsing

import utils._
import java.io.File

import silex._

import amyc.utils.Position

// The lexer for Amy.
object Lexer extends Pipeline[List[File], Iterator[Token]]
                with Lexers with CharRegExps {

  /** Tiny Scallion-lexer reference:
    * ==============================
    * Scallion's lexer essentially allows you to define a list of regular expressions
    * in their order of priority. To tokenize a given input stream of characters, each
    * individual regular expression is applied in turn. If a given expression matches, it
    * is used to produce a token of maximal length. Whenever a regular expression does not
    * match, the expression of next-highest priority is tried.
    * The result is a stream of tokens.
    *
    * Regular expressions `r` can be built using the following operators:
    *   - `word("abc")`  matches the sequence "abc" exactly
    *   - `r1 | r2`      matches either expression `r1` or expression `r2`
    *   - `r1 ~ r2`      matches `r1` followed by `r2`
    *   - `oneOf("xy")`  matches either "x" or "y"
    *                    (i.e., it is a shorthand of `word` and `|` for single characters)
    *   - `elem(c)`      matches character `c`
    *   - `elem(f)`      matches any character for which the boolean predicate `f` holds
    *   - `opt(r)`       matches `r` or nothing at all
    *   - `many(r)`      matches any number of repetitions of `r` (including none at all)
    *   - `many1(r)`     matches any non-zero number of repetitions of `r`
    *
    * To define the token that should be output for a given expression, one can use
    * the `|>` combinator with an expression on the left-hand side and a function
    * producing the token on the right. The function is given the sequence of matched
    * characters and the source-position range as arguments.
    *
    * For instance,
    *
    *   `elem(_.isDigit) ~ word("kg") |> {
    *     (cs, range) => WeightLiteralToken(cs.mkString).setPos(range._1)) }`
    *
    * will match a single digit followed by the characters "kg" and turn them into a
    * "WeightLiteralToken" whose value will be the full string matched (e.g. "1kg").
    */

  // Type of characters consumed.
  override type Character = Char

  // Type of positions.
  type Position = SourcePosition

  // Type of tokens produced.
  type Token = amyc.parsing.Token

  import Tokens._

  val lexer = Lexer(
    // Keywords
    word("abstract") | word("case") | word("class") |
    word("def") | word("else") | word("extends") |
    word("if") | word("match") | word("object") |
    word("val") | word("error") | word("_")
      |> { (cs, range) => KeywordToken(cs.mkString).setPos(range._1) },

    // Primitive type names
    word("Int") | word("String") | word("Boolean") |
    word("Unit")
      |> { (cs, range) => PrimTypeToken(cs.mkString).setPos(range._1) },

    // Boolean literals
    word("true") | word("false")
      |> { (cs, range) => BoolLitToken(cs.mkString.toBoolean).setPos(range._1) },

    // Operators
    word("<=") | word("&&") | word("||") |
    word("==") | word("++") | oneOf("+-*/%<!")
      |> { (cs, range) => OperatorToken(cs.mkString).setPos(range._1) },

    // Identifiers
    elem(_.isLetter) ~ many(elem(c => c.isLetterOrDigit || c == '_'))
    |> { (cs, range) => IdentifierToken(cs.mkString).setPos(range._1) },

    // Integer literals
    // NOTE: Make sure to handle invalid (e.g. overflowing) integer values safely by
    //       emitting an ErrorToken instead.
    // Overflow and underflows are correctly handled because Int is explicitly a 32-bit signed integer in Scala, as it is in Amy
    many1(digit)
      |> {(cs, range) => (try {
          IntLitToken(cs.mkString.toInt)
      } catch {
        case e: NumberFormatException => ErrorToken(cs.mkString)
      }).setPos(range._1)
    },

    // String literals
    elem('"') ~ many(elem(c => c != '\n' && c != '\"')) ~ elem('"')
      |> {(cs, range) => StringLitToken(cs.init.tail.mkString).setPos(range._1)},

    // Delimiters and whitespace
    many1(whiteSpace) |> { (cs, range) =>SpaceToken().setPos(range._1)},
    
    word("=>") | oneOf(".,:;(){}[]=")
      |> {(cs, range) => DelimiterToken(cs.mkString).setPos(range._1)},

    // Single line comments
    word("//") ~ many(elem(_ != '\n'))
      |> { (cs, range) => CommentToken(cs.mkString("")).setPos(range._1) },

    // Multiline comments
    // NOTE: Amy does not support nested multi-line comments (e.g. `/* foo /* bar */ */`).
    //       Make sure that unclosed multi-line comments result in an ErrorToken.

    //https://discord.com/channels/753221180856598530/758575246021754880/759060457813180468
    word("/*") ~ commentsContent ~ word("*/") 
      |> {(cs, range) => CommentToken(cs.mkString).setPos(range._1)},

    word("/*") ~ commentsContent // Un finished comments
      |> {(cs, range) => ErrorToken(cs.mkString).setPos(range._1)},

  ) onError {
    // We also emit ErrorTokens for Scallion-handled errors.
    (cs, range) => ErrorToken(cs.mkString).setPos(range._1)
  } onEnd {
    // Once all the input has been consumed, we emit one EOFToken.
    pos => EOFToken().setPos(pos)
  }

  lazy val commentsContent = many(elem(c => c != '*' && c != '/') | elem('*') ~ opt(elem(_ != '/')))
  
  override def run(ctx: Context)(files: List[File]): Iterator[Token] = {
    var it = Seq[Token]().toIterator

    for (file <- files) {
      val source = Source.fromFile(file, SourcePositioner(file))
      // Remove all whitespace and comment tokens
      it ++= lexer.spawn(source).filter {
          case SpaceToken() => false
          case CommentToken(c) => false
          case _ => true
      }.map {
        case token@ErrorToken(error) => ctx.reporter.fatal("Unknown token at " + token.position + ": " + error)
        case token => token
      }
    }
    it
  }
}

/** Extracts all tokens from input and displays them */
object DisplayTokens extends Pipeline[Iterator[Token], Unit] {
  override def run(ctx: Context)(tokens: Iterator[Token]): Unit = {
    tokens.foreach(println(_))
  }
}
