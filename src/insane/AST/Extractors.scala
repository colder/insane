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
      def unapply(tree: Select): Option[Tree] = tree match {
        case Select(t, n) if (n == nme.UNARY_!) => Some(t)
        case _ => None
      }
    }
  }
}
