package amyc
package analyzer

import utils._
import ast.SymbolicTreeModule._
import ast.Identifier

// The type checker for Amy
// Takes a symbolic program and rejects it if it does not follow the Amy typing rules.
object TypeChecker extends Pipeline[(Program, SymbolTable), (Program, SymbolTable)] {

  def run(ctx: Context)(v: (Program, SymbolTable)): (Program, SymbolTable) = {
    import ctx.reporter._

    val (program, table) = v

    case class Constraint(found: Type, expected: Type, pos: Position)

    // Represents a type variable.
    // It extends Type, but it is meant only for internal type checker use,
    //  since no Amy value can have such type.
    case class TypeVariable private (id: Int) extends Type
    object TypeVariable {
      private val c = new UniqueCounter[Unit]
      def fresh(): TypeVariable = TypeVariable(c.next(()))
    }

    // Generates typing constraints for an expression `e` with a given expected type.
    // The environment `env` contains all currently available bindings (you will have to
    //  extend these, e.g., to account for local variables).
    // Returns a list of constraints among types. These will later be solved via unification.
    def genConstraints(e: Expr, expected: Type)(implicit env: Map[Identifier, Type]): List[Constraint] = {
      
      // This helper returns a list of a single constraint recording the type
      //  that we found (or generated) for the current expression `e`
      def topLevelConstraint(found: Type): List[Constraint] =
        List(Constraint(found, expected, e.position))
      
      e match {
        case Variable(id) => 
          topLevelConstraint(env(id))  
        case IntLiteral(_) =>
          topLevelConstraint(IntType)
        case BooleanLiteral(_) => 
          topLevelConstraint(BooleanType)
        case StringLiteral(_) => 
          topLevelConstraint(StringType)      
        case UnitLiteral() => 
          topLevelConstraint(UnitType)
        
        case Plus(lhs, rhs) => 
          topLevelConstraint(IntType) ++ genConstraints(lhs, IntType) ++ genConstraints(rhs, IntType)
        case Minus(lhs, rhs) => 
          topLevelConstraint(IntType) ++ genConstraints(lhs, IntType) ++ genConstraints(rhs, IntType)
        case Times(lhs, rhs) => 
          topLevelConstraint(IntType) ++ genConstraints(lhs, IntType) ++ genConstraints(rhs, IntType)
        case Div(lhs, rhs) => 
          topLevelConstraint(IntType) ++ genConstraints(lhs, IntType) ++ genConstraints(rhs, IntType)
        case Mod(lhs, rhs) => 
          topLevelConstraint(IntType) ++ genConstraints(lhs, IntType) ++ genConstraints(rhs, IntType)
        case LessThan(lhs, rhs) => 
          topLevelConstraint(BooleanType) ++ genConstraints(lhs, IntType) ++ genConstraints(rhs, IntType)
        case LessEquals(lhs, rhs) => 
          topLevelConstraint(BooleanType) ++ genConstraints(lhs, IntType) ++ genConstraints(rhs, IntType)
        case And(lhs, rhs) => 
          topLevelConstraint(BooleanType) ++ genConstraints(lhs, BooleanType) ++ genConstraints(rhs, BooleanType)
        case Or(lhs, rhs) => 
          topLevelConstraint(BooleanType) ++ genConstraints(lhs, BooleanType) ++ genConstraints(rhs, BooleanType)
        case Equals(lhs, rhs) =>
          val t = TypeVariable.fresh()
          topLevelConstraint(BooleanType) ++ genConstraints(lhs, t) ++ genConstraints(rhs, t)
        case Concat(lhs, rhs) => 
          topLevelConstraint(StringType) ++ genConstraints(lhs, StringType) ++ genConstraints(rhs, StringType)

        case Not(l) => 
          topLevelConstraint(BooleanType) ++ genConstraints(l, BooleanType)
        case Neg(l) => 
          topLevelConstraint(IntType) ++ genConstraints(l, IntType)

        case Call(qname, args) =>
          val consFunc = table.getConstructor(qname).orElse(table.getFunction(qname)).get
          topLevelConstraint(consFunc.retType) ++ (args.zip(consFunc.argTypes)).flatMap(a => genConstraints(a._1, a._2))       
        
        case Sequence(e1, e2) => 
          genConstraints(e1, TypeVariable.fresh()) ++ genConstraints(e2, expected)
        case Let(df, value, body) => 
          genConstraints(value, df.tt.tpe) ++ genConstraints(body, expected)(env + (df.name -> df.tt.tpe))
        case Ite(cond, thenn, elze) => 
          genConstraints(cond, BooleanType) ++ genConstraints(thenn, expected) ++ genConstraints(elze, expected)

        case Error(m) => 
          genConstraints(m, StringType)

        case TupleExpr(exprs) => 
          expected match {
            case TupleType(types) => 
              val list = exprs.zip(types)
              list.map{
                case (e, t) => genConstraints(e, t.tpe)
              }.flatten
            case _=> ctx.reporter.fatal(s"TupleType mismatch")
          }
          

        case Match(scrut, cases) =>
          // Returns additional constraints from within the pattern with all bindings
          // from identifiers to types for names bound in the pattern.
          // (This is analogous to `transformPattern` in NameAnalyzer.)
          def handlePattern(pat: Pattern, scrutExpected: Type):
            (List[Constraint], Map[Identifier, Type]) =
          {
            pat match {
              // Wildcard and identifier patterns follow any type
              // Wildcard is not bound to any value 
              case WildcardPattern() => (List(), Map())
              // Identifier is bound to the value
              case IdPattern(name) => (List(), Map(name -> scrutExpected))
              // Each literal pattern follows exactly the type of its literal
              case LiteralPattern(lit) =>
                (genConstraints(lit, scrutExpected), Map())
              case CaseClassPattern(constr, args) =>
                table.getConstructor(constr) match {
                  case Some(constrSig) =>
                    val scrutConstraint = Constraint(constrSig.retType, scrutExpected, e.position) 
                    (args zip constrSig.argTypes)
                      .foldLeft[(List[Constraint], Map[Identifier, Type])]((List(scrutConstraint), Map())){
                        case ((constraintsAcc, envAcc), (pat, t)) =>
                          val (constr, moreEnv) = handlePattern(pat, t)
                          (constraintsAcc ++ constr, envAcc ++ moreEnv)
                
                      }
                    case None => ctx.reporter.fatal(s"Contructor ${constr} not found")
                }
              case TuplePattern(patterns) =>
                scrutExpected match {
                  case TupleType(types) =>
                    val (constraints, typeVar, env) = (patterns zip types)
                      .foldLeft[(List[Constraint], List[Type], Map[Identifier, Type])]((List(), List(), Map())){
                        case ((constraintsAcc, typeVarAcc, envAcc), (pat, t)) =>
                          val (constr, moreEnv) = handlePattern(pat, t.tpe)
                          (constraintsAcc ++ constr, typeVarAcc :+ t.tpe, envAcc ++ moreEnv)
                      }
                    
                    val scrutConstraint = Constraint(
                      TupleType(typeVar.map(t=>TypeTree(t))), 
                      scrutExpected, 
                      e.position) 
                    (constraints :+ scrutConstraint, env)
                  case _ => (List(Constraint(TupleType(Nil), scrutExpected, e.position)), Map())
                }
            }
          }

          def handleCase(cse: MatchCase, scrutExpected: Type): List[Constraint] = {
            val (patConstraints, moreEnv) = handlePattern(cse.pat, scrutExpected)
            // Constraints to solve the pattern +
            // constraints on the return type of match constrained on all the possible
            // return types of all cases expr.
            // The exprs have additional parameters to generate the constraints
            // (for instance the case classes parameters)
            patConstraints ++ 
            genConstraints(cse.expr, expected)(env ++ moreEnv)
          }

          val st = TypeVariable.fresh()
          val c = genConstraints(scrut, st)
          // anyway the type will always be already set and we imperatively need the type tree
          val foundType = c.last.found
          c ++ cases.flatMap(cse => handleCase(cse, foundType))
      }


    }


    // Given a list of constraints `constraints`, replace every occurence of type variable
    //  with id `from` by type `to`.
    def subst_*(constraints: List[Constraint], from: Int, to: Type): List[Constraint] = {
      // Do a single substitution.
      def subst(tpe: Type, from: Int, to: Type): Type = {
        tpe match {
          case TypeVariable(`from`) => to
          case other => other
        }
      }

      constraints map { case Constraint(found, expected, pos) =>
        Constraint(subst(found, from, to), subst(expected, from, to), pos)
      }
    }

    // Solve the given set of typing constraints and report errors
    //  using `ctx.reporter.error` if they are not satisfiable.
    // We consider a set of constraints to be satisfiable exactly if they unify.
    def solveConstraints(constraints: List[Constraint]): Unit = {
      // HINT: You can use the `subst_*` helper above to replace a type variable
      //       by another type in your current set of constraints.
      constraints match {
        case Nil => ()
        case Constraint(found, expected, pos) :: more =>
          (found, expected) match {
            case (TypeVariable(id), type2) =>
              solveConstraints(subst_*(constraints, id, type2))
            case (type1, TypeVariable(id)) =>
              solveConstraints(subst_*(constraints, id, type1))
            case (type1, type2) =>  
              if(type1 == type2) {
                solveConstraints(more)
              } else {
                ctx.reporter.error("Error: Type mismatch")
              }
        } 
      } 
    }

    // Putting it all together to type-check each module's functions and main expression.
    program.modules.foreach { mod =>
      // Put function parameters to the symbol table, then typecheck them against the return type
      mod.defs.collect { case FunDef(_, params, retType, body) =>
        val env = params.map{ case ParamDef(name, tt) => name -> tt.tpe }.toMap
        solveConstraints(genConstraints(body, retType.tpe)(env))
      }

      // Type-check expression if present. We allow the result to be of an arbitrary type by
      // passing a fresh (and therefore unconstrained) type variable as the expected type.
      val tv = TypeVariable.fresh()
      mod.optExpr.foreach(e => solveConstraints(genConstraints(e, tv)(Map())))
    }

    v

  }
}
