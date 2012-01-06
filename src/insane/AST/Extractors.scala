package insane
package AST

import scala.tools.nsc._

/** Contains extractors to pull-out interesting parts of the Scala ASTs. */
trait Extractors {
  val global: Global

  import global._
  import global.definitions._

  object StructuralExtractors {
    object ExScalaPredef {
      /** Extracts method calls from scala.Predef. */
      def unapply(tree: Select): Option[String] = tree match {
        case Select(Select(This(scalaName),predefName),symName)
          if("scala".equals(scalaName.toString) && "Predef".equals(predefName.toString)) =>
            Some(symName.toString)
        case _ => None
      }
    }

    object ExEnsuredExpression {
      /** Extracts the 'ensuring' contract from an expression. */
      def unapply(tree: Apply): Option[(Tree,Symbol,Tree)] = tree match {
        case Apply(
          Select(
            Apply(
              TypeApply(
                ExScalaPredef("any2Ensuring"),
                TypeTree() :: Nil),
              body :: Nil),
            ensuringName),
          (Function((vd @ ValDef(_, _, _, EmptyTree)) :: Nil, contractBody)) :: Nil)
          if("ensuring".equals(ensuringName.toString)) => Some((body, vd.symbol, contractBody))
        case _ => None
      }
    }

    object ExAssertEQExpression {
      /** Extracts the 'assert' contract from an expression. */
      def unapply(tree: Tree): Option[(Tree, Tree)] = tree match {
        case Apply(ExScalaPredef("assert"), Apply(Select(lhs, methName), rhs :: Nil) :: omsg)
              if ("eq" == methName.toString) =>
            Some((lhs, rhs))
        case t =>
          None
      }
    }
    object ExAssertNEExpression {
      /** Extracts the 'assert' contract from an expression. */
      def unapply(tree: Tree): Option[(Tree, Tree)] = tree match {
        case Apply(ExScalaPredef("assert"), Apply(Select(lhs, methName), rhs :: Nil) :: omsg)
              if ("ne" == methName.toString) =>
            Some((lhs, rhs))
        case t =>
          None
      }
    }

    object ExRequiredExpression {
      /** Extracts the 'require' contract from an expression (only if it's the
       * first call in the block). */
      def unapply(tree: Tree): Option[(Tree,Tree)] = tree match {
        case Block(Apply(ExScalaPredef("require"), contractBody :: Nil) :: rest, body) =>
          if(rest.isEmpty)
            Some((body,contractBody))
          else
            Some((Block(rest,body),contractBody))

        case Apply(ExScalaPredef("require"), contractBody :: Nil) =>
          Some((Block(Literal(Constant(()))), contractBody))
        case t =>
          None
      }
    }

    object ExWhile {
      /** Extracts (cond, stmts) from a while loop */
      def unapply(tree: Tree): Option[(Tree, Seq[Tree])] = tree match {
        // do body while(cond) ==> LabelDef(L$, List(), if(cond) { body; L$() })
        case lab @ LabelDef(_ , List(), If(cond, Block(stats, Apply(fun: Ident, List())), _ ))
          if lab.symbol eq fun.symbol => Some(cond, stats)

        case lab @ LabelDef(_ , List(), If(cond, Apply(fun: Ident, List()), _ ))
          if lab.symbol eq fun.symbol => Some(cond, Nil)

        case _ => None

      }
    }


    object ExDoWhile {
      /** Extracts (cond, stmts) from a while loop */
      def unapply(tree: Tree): Option[(Tree, Seq[Tree])] = tree match {
        // do body while(cond) ==> LabelDef(L$, List(), { body; if (cond) L$() })
        case lab @ LabelDef(_ , List(), Block(stats, If(cond, Apply(fun: Ident, List()), _ )))
          if lab.symbol eq fun.symbol => Some(cond, stats)

        case lab @ LabelDef(_ , List(), If(cond, Apply(fun: Ident, List()), _ ))
          if lab.symbol eq fun.symbol => Some(cond, Nil)

        case _ => None

      }
    }
  }
  object ExpressionExtractors {
    object ExAnd {
      def unapply(tree: Apply): Option[(Tree,Tree)] = tree match {
        case Apply(s @ Select(lhs, _), List(rhs)) if (s.symbol == Boolean_and) =>
          Some((lhs,rhs))
        case _ => None
      }
    }

    object ExOr {
      def unapply(tree: Apply): Option[(Tree,Tree)] = tree match {
        case Apply(s @ Select(lhs, _), List(rhs)) if (s.symbol == Boolean_or) =>
          Some((lhs,rhs))
        case _ => None
      }
    }

    object ExNot {
      lazy val Boolean_not = getMember(BooleanClass, nme.UNARY_!)
      def unapply(tree: Apply): Option[Tree] = tree match {
        case Apply(s @ Select(t, _), List()) if (s.symbol == Boolean_not) =>
          Some(t)
        case _ => None
      }
    }

    object ExNew {
      def unapply(tree: Apply): Option[(Symbol,List[Tree])] = tree match {
        case a @ Apply(s @ Select(New(_), name), args) if (s.symbol.name == nme.CONSTRUCTOR) =>
          Some((s.symbol,args))
        case _ => None
      }
    }
  }
}
