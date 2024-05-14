package amyc
package analyzer

import utils._
import ast.{Identifier, SymbolicTreeModule => S}

import scala.collection.mutable.HashMap

// Name analyzer for Amy
// Takes a nominal program (names are plain string, qualified names are string pairs)
// and returns a symbolic program, where all names have been resolved to unique Identifiers.
// Rejects programs that violate the Amy naming rules.
// Also populates symbol table.
object Tuples extends Pipeline[(S.Program, SymbolTable), (S.Program, SymbolTable)] {
  private val moduleName = "$amycTuples"

  def run(ctx: Context)(v: (S.Program, SymbolTable)): (S.Program, SymbolTable) = {
    import ctx.reporter._

    val (p, table) = v

    // Step 1: Add module "Tuple"
    table.addModule(moduleName)
    def tupleName(size: Integer) = (moduleName, size.toString)
    val tuplesTypesMap = HashMap[Int, Identifier]()
    def tupleType(size: Int): Identifier = {
      val (owner, name) = tupleName(size)
      tuplesTypesMap.getOrElseUpdate(
        size, 
        table.addType(owner, name)
      )
    }

    val tuplesConstructorsMap = HashMap[Int, Identifier]()
    def tupleConstructor(size: Int): Identifier = {
      val parent = tupleType(size)
      val (owner, name) = tupleName(size)
      tuplesConstructorsMap.getOrElseUpdate(
        size, 
        // We don't care what type it is, we already typed checked
        table.addConstructor(owner, name, List.fill(size)(S.IntType), parent)
      )
    }

    def transformType(t: S.Type): S.Type = {
      t match {
        case S.TupleType(types) => 
          S.ClassType(tupleType(types.size)) 
        case rest => rest
      }
    }
    def transformTypeTree(tt: S.TypeTree): S.TypeTree = {
      tt match {
        case S.TypeTree(tpe) => S.TypeTree(transformType(tpe))
        case rest => rest
      }
    }

    // Step 6: We now know all definitions in the program.
    //         Reconstruct modules and analyse function bodies/ expressions

    def transformDef(df: S.ClassOrFunDef, module: Identifier): S.ClassOrFunDef = {
      val newDf: S.ClassOrFunDef = df match {
        case S.CaseClassDef(name, fields, parent) =>
          S.CaseClassDef(name, fields map transformTypeTree, parent)
        case fd: S.FunDef =>
          transformFunDef(fd, module)
        case rest => rest
      }
      newDf.setPos(df)
    }
    def transformFunDef(fd: S.FunDef, module: Identifier): S.FunDef = {
      val S.FunDef(name, params, retType, body) = fd

      val paramNames = params.map(_.name)

      val newParams = params.map{
        case S.ParamDef(name, tt) => S.ParamDef(name, tt)
      }

      val paramsMap = paramNames.zip(newParams.map(_.name)).toMap

      S.FunDef(
        name,
        newParams,
        transformTypeTree(retType),
        transformExpr(body)
      ).setPos(fd)
    }

    def transformExpr(expr: S.Expr): S.Expr = {
      val res = expr match {
        case S.Plus(lhs, rhs) =>
          S.Plus(transformExpr(lhs), transformExpr(rhs))
        case S.Minus(lhs, rhs) =>
          S.Minus(transformExpr(lhs), transformExpr(rhs))
        case S.Times(lhs, rhs) =>
          S.Times(transformExpr(lhs), transformExpr(rhs))
        case S.Div(lhs, rhs) =>
          S.Div(transformExpr(lhs), transformExpr(rhs))
        case S.Mod(lhs, rhs) =>
          S.Mod(transformExpr(lhs), transformExpr(rhs))
        case S.LessThan(lhs, rhs) =>
          S.LessThan(transformExpr(lhs), transformExpr(rhs))
        case S.LessEquals(lhs, rhs) =>
          S.LessEquals(transformExpr(lhs), transformExpr(rhs))
        case S.And(lhs, rhs) =>
          S.And(transformExpr(lhs), transformExpr(rhs))
        case S.Or(lhs, rhs) =>
          S.Or(transformExpr(lhs), transformExpr(rhs))
        case S.Equals(lhs, rhs) =>
          S.Equals(transformExpr(lhs), transformExpr(rhs))
        case S.Concat(lhs, rhs) =>
          S.Concat(transformExpr(lhs), transformExpr(rhs))
        case S.Not(e) =>
          S.Not(transformExpr(e))
        case S.Neg(e) =>
          S.Neg(transformExpr(e))
        case S.Call(qname, args) =>
          S.Call(qname, args map transformExpr)
        case S.Sequence(e1, e2) =>
          S.Sequence(transformExpr(e1), transformExpr(e2))
        case S.Let(vd, value, body) =>
          S.Let(
            S.ParamDef(vd.name, transformTypeTree(vd.tt)),
            transformExpr(value),
            transformExpr(body)
          )
        case S.Ite(cond, thenn, elze) =>
          S.Ite(transformExpr(cond), transformExpr(thenn), transformExpr(elze))
        case S.Match(scrut, cases) =>
          def transformCase(cse: S.MatchCase) = {
            val S.MatchCase(pat, rhs) = cse
            val newPat = transformPattern(pat)
            S.MatchCase(newPat, transformExpr(rhs).setPos(rhs)).setPos(cse)
          }

          def transformPattern(pat: S.Pattern): S.Pattern = {
            val newPat: S.Pattern = pat match {
              case S.TuplePattern(pats) =>
                val newPats = pats map transformPattern
                S.CaseClassPattern(tupleConstructor(newPats.size), newPats)
              // Other things doesnt modify anything
              case S.CaseClassPattern(constr, pats) =>
                val newPats = pats map transformPattern
                S.CaseClassPattern(constr, newPats)
              case rest => rest
            }
            newPat.setPos(pat)
          }

          S.Match(transformExpr(scrut), cases map transformCase)

        case S.TupleExpr(exprs) => 
          var retypedExprs = exprs.map(transformExpr(_))
          S.Call(tupleConstructor(retypedExprs.size), retypedExprs)
        case S.Error(msg) =>
          S.Error(transformExpr(msg))
        // Other things doesnt modify anything / are leaves
        case rest => rest
      }
      res.setPos(expr)
    }

    val newProgram = S.Program(
      p.modules map { case mod@S.ModuleDef(name, defs, optExpr) =>
        S.ModuleDef(
          name,
          defs map (transformDef(_, name)),
          optExpr map (transformExpr(_))
        ).setPos(mod)
      }
    ).setPos(p)

    (newProgram, table)

  }
}
