package amyc
package parsing

import scala.language.implicitConversions

import amyc.ast.NominalTreeModule._
import amyc.utils._
import Tokens._
import TokenKinds._

import scallion._

// The parser for Amy
object Parser extends Pipeline[Iterator[Token], Program] with Parsers {

  type Token = amyc.parsing.Token
  type Kind = amyc.parsing.TokenKind

  import Implicits._

  override def getKind(token: Token): TokenKind = TokenKind.of(token)

  val eof: Syntax[Token] = elem(EOFKind)
  def op(string: String): Syntax[String] = accept(OperatorKind(string)) {
    case OperatorToken(name) => name
  }
  def kw(string: String): Syntax[Token] = elem(KeywordKind(string))

  implicit def delimiter(string: String): Syntax[Token] = elem(
    DelimiterKind(string)
  )

  // An entire program (the starting rule for any Amy file).
  lazy val program: Syntax[Program] = many1(many1(module) ~<~ eof).map(ms =>
    Program(ms.flatten.toList).setPos(ms.head.head)
  )

  // A module (i.e., a collection of definitions and an initializer expression)
  lazy val module: Syntax[ModuleDef] =
    (kw("object") ~ identifier ~ "{".skip ~ many(definition) ~ opt(
      expr
    ) ~ "}".skip).map { case obj ~ id ~ defs ~ body =>
      ModuleDef(id, defs.toList, body).setPos(obj)
    }

  // An identifier.
  val identifier: Syntax[String] = accept(IdentifierKind) {
    case IdentifierToken(name) => name
  }

  // An identifier along with its position.
  val identifierPos: Syntax[(String, Position)] = accept(IdentifierKind) {
    case id @ IdentifierToken(name) => (name, id.position)
  }

  // A definition within a module.
  lazy val definition: Syntax[ClassOrFunDef] =
    abstractClassDefinition | caseClassDefinition | functionDefinition

  lazy val abstractClassDefinition: Syntax[ClassOrFunDef] =
    (kw("abstract") ~ kw("class") ~ identifier).map { case kw ~ _ ~ id =>
      AbstractClassDef(id).setPos(kw)
    }

  lazy val caseClassDefinition: Syntax[ClassOrFunDef] =
    (kw("case") ~ kw(
      "class"
    ) ~ identifier ~ "(".skip ~ parameters ~ ")".skip ~ kw(
      "extends"
    ) ~ identifier).map { case kw ~ _ ~ id ~ params ~ ext ~ idP =>
      CaseClassDef(id, params.map(_.tt), idP).setPos(kw)
    }

  lazy val functionDefinition: Syntax[ClassOrFunDef] =
    (kw("def") ~ identifier ~ delimiter("(").skip ~ parameters ~ delimiter(
      ")"
    ).skip ~ ":".skip
      ~ typeTree ~ "=".skip ~ "{".skip ~ expr ~ "}".skip).map {
      case kw ~ id ~ params ~ tree ~ exp =>
        FunDef(id, params, tree, exp).setPos(kw)
    }

  // A list of parameter definitions.
  lazy val parameters: Syntax[List[ParamDef]] =
    repsep(parameter, ",").map(_.toList)

  // A parameter definition, i.e., an identifier along with the expected type.
  lazy val parameter: Syntax[ParamDef] =
    (identifierPos ~ ":".skip ~ typeTree).map { case id ~ tree =>
      ParamDef(id._1, tree).setPos(id._2)
    }

  // A type expression.
  lazy val typeTree: Syntax[TypeTree] = recursive {
    primitiveType | identifierType | tupleType
  }

  // A built-in type (such as `Int`).
  val primitiveType: Syntax[TypeTree] = accept(PrimTypeKind) {
    case tk @ PrimTypeToken(name) =>
      TypeTree(name match {
        case "Unit"    => UnitType
        case "Boolean" => BooleanType
        case "Int"     => IntType
        case "String"  => StringType
        case _ =>
          throw new java.lang.Error("Unexpected primitive type name: " + name)
      }).setPos(tk)
  }

  // A user-defined type (such as `List`).
  lazy val identifierType: Syntax[TypeTree] =
    (identifierPos ~ opt(".".skip ~ identifier)).map {
      case id1 ~ Some(id2) =>
        TypeTree(ClassType(QualifiedName(Some(id1._1), id2))).setPos(id1._2)
      case id ~ None =>
        TypeTree(ClassType(QualifiedName(None, id._1))).setPos(id._2)
    }
  
  lazy val tupleType: Syntax[TypeTree] = ("(".skip ~ rep1sep(typeTree, ",").map(_.toList) ~ ")".skip).map{
    case types => TypeTree(TupleType(types))
  }

  lazy val simpleExpr: Syntax[Expr] = recursive {
    literalBase.up[Expr] | variableOrCall | parenthesizedExpr | error
  }

  lazy val unaryExpr: Syntax[Expr] =
    ((op("-") | op("!")) ~ simpleExpr).map {
      case "-" ~ exp => Neg(exp)
      // We can't have something else than ! and we hence avoid a warning
      case _ ~ exp => Not(exp)
    }

  val plusOp = op("+")
  val minusOp = op("-")
  val timesOp = op("*")
  val divOp = op("/")
  val binExpr = operators(simpleExpr | unaryExpr)(
    //val binOps = operators(expr)(
    // Indicates the various operators and their associativity.
    // First level in multiplication and division, which are
    // left associative and bind the strongest.
    op("%") | timesOp | divOp is LeftAssociative,
    // On the next priority level are addition and subtraction,
    // which are also left associative.
    op("++") | plusOp | minusOp is LeftAssociative,
    op("<") | op("<=") is LeftAssociative,
    op("==") is LeftAssociative,
    op("&&") is LeftAssociative,
    op("||") is LeftAssociative
  )({
    // Indicates how to apply a binary operator.
    case (l, op, r) =>
      op match {
        case "-"  => Minus(l, r)
        case "+"  => Plus(l, r)
        case "*"  => Times(l, r)
        case "/"  => Div(l, r)
        case "%"  => Mod(l, r)
        case "++" => Concat(l, r)
        case "<"  => LessThan(l, r)
        case "<=" => LessEquals(l, r)
        case "==" => Equals(l, r)
        case "&&" => And(l, r)
        case "||" => Or(l, r)
      }
  })

  lazy val exprWithoutSeqLet = binExpr | ite 
  lazy val exprWithoutSeq = 
    (((exprWithoutSeqLet) ~ opt(matchExpr)).map {
      case e ~ None           => e
      case e1 ~ Some(kw ~ mc) => Match(e1, mc).setPos(kw)
    })

  lazy val expr: Syntax[Expr] = recursive {
    (
      (
        exprWithoutSeq ~ opt(seq)
      ).map {
        case e ~ None              => e
        case e1 ~ Some(token ~ e2) => Sequence(e1, e2).setPos(token)
      }
    ) | let

  }

  lazy val seq: Syntax[Token ~ Expr] =
    ";" ~ expr

  lazy val let: Syntax[Expr] = recursive {
    (kw(
      "val"
    ) ~ parameter ~ "=".skip ~ exprWithoutSeqLet ~ ";".skip ~ (expr)).map {
      case kw ~ param ~ e1 ~ e2 => Let(param, e1, e2).setPos(kw)
    }
  }

  lazy val parenthesizedExpr: Syntax[Expr] =
    ("(".skip ~ repsep(expr, ",").map(_.toList) ~ ")".skip).map {
      case exprs => 
        if (exprs.length == 0)
          UnitLiteral()
        else if (exprs.length == 1)
          exprs(0)
        else
          TupleExpr(exprs)
    }

  lazy val ite: Syntax[Expr] =
    (kw("if") ~ "(".skip ~ expr ~ ")".skip ~
      "{".skip ~ expr ~ "}".skip ~
      kw("else").skip ~
      "{".skip ~ expr ~ "}".skip).map { case kw ~ cond ~ thenn ~ elze =>
      Ite(cond, thenn, elze).setPos(kw)
    }

  lazy val error: Syntax[Expr] =
    (kw("error") ~ "(".skip ~ expr ~ ")".skip).map { case kw ~ msg =>
      Error(msg).setPos(kw)
    }

  // A literal expression.
  lazy val unitLiteral: Syntax[Literal[_]] = ("(" ~ ")").map { _ =>
    UnitLiteral()
  } /// No position...  ¯\_(ツ)_/¯

  lazy val literalBase: Syntax[Literal[_]] = accept(LiteralKind) {
    case tk @ BoolLitToken(value)   => BooleanLiteral(value).setPos(tk)
    case tk @ IntLitToken(value)    => IntLiteral(value).setPos(tk)
    case tk @ StringLitToken(value) => StringLiteral(value).setPos(tk)
  }

  lazy val literal = literalBase | unitLiteral

  lazy val matchExpr =
    (kw("match") ~ "{".skip ~ matchCases ~ "}".skip).map { case kw ~ mC =>
      kw ~ mC.toList
    }

  lazy val matchCases: Syntax[Seq[MatchCase]] =
    many1((kw("case") ~ pattern ~ "=>".skip ~ expr).map {
      case kw ~ pat ~ expr => MatchCase(pat, expr).setPos(kw)
    })

  // A pattern as part of a match case.
  lazy val pattern: Syntax[Pattern] = recursive {
    literalPattern | wildPattern | caseClassPattern | tuplePattern
  }

  lazy val patterns: Syntax[List[Pattern]] = repsep(pattern, ",").map(_.toList)

  lazy val literalPattern: Syntax[Pattern] = literalBase.map { lit =>
    LiteralPattern(lit).setPos(lit)
  }
   
  lazy val tuplePattern: Syntax[Pattern] = ("(".skip ~ rep1sep(pattern, ",").map(_.toList) ~ ")".skip).map{
    case pats => TuplePattern(pats)
  }

  lazy val wildPattern: Syntax[Pattern] = kw("_").map { wcp =>
    WildcardPattern().setPos(wcp)
  }
 
  lazy val caseClassPattern: Syntax[Pattern] =
    (identifierPos ~
      opt(
        opt(".".skip ~ identifier) ~
          "(".skip ~ patterns ~ ")".skip
      )).map {
      case (id, pos) ~ None => IdPattern(id).setPos(pos)
      case (id, pos) ~ Some(None ~ args) =>
        CaseClassPattern(QualifiedName(None, id), args).setPos(pos)
      case (id, pos) ~ Some(Some(id2) ~ args) =>
        CaseClassPattern(QualifiedName(Some(id), id2), args).setPos(pos)
    }

  lazy val variableOrCall: Syntax[Expr] =
    (identifierPos ~
      opt(
        opt(".".skip ~ identifier) ~
          "(".skip ~ args ~ ")".skip
      )).map {
      case (id, pos) ~ None => Variable(id).setPos(pos)
      case (id, pos) ~ Some(None ~ args) =>
        Call(QualifiedName(None, id), args).setPos(pos)
      case (id, pos) ~ Some(Some(id2) ~ args) =>
        Call(QualifiedName(Some(id), id2), args).setPos(pos)
    }

  // A list of args definitions.
  lazy val args: Syntax[List[Expr]] =
    repsep(
      expr,
      delimiter(",")
    ).map(_.toList)

  // Ensures the grammar is in LL(1)
  lazy val checkLL1: Boolean = {
    if (program.isLL1) {
      true
    } else {
      // Set `showTrails` to true to make Scallion generate some counterexamples for you.
      // Depending on your grammar, this may be very slow.
      val showTrails = false//true
      debug(program, showTrails)
      false
    }
  }

  override def run(ctx: Context)(tokens: Iterator[Token]): Program = {
    import ctx.reporter._
    if (!checkLL1) {
      ctx.reporter.fatal("Program grammar is not LL1!")
    }

    val parser = Parser(program)

    parser(tokens) match {
      case Parsed(result, rest) => result
      case UnexpectedEnd(rest)  => fatal("Unexpected end of input.")
      case UnexpectedToken(token, rest) =>
        fatal(
          "Unexpected token: " + token + ", possible kinds: " + rest.first
            .map(_.toString)
            .mkString(", ")
        )
    }
  }
}
