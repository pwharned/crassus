package org.pwharned.macros


// Inline routing table builder: routes must be provided as a literal List.
import org.pwharned.route.Router
import org.pwharned.route.Router.{HttpMethod, Route}

import scala.quoted.*

inline def buildRoutingTable(l: List[Route[HttpMethod]]): Map[String, Router.Route[Router.HttpMethod]] =
  ${ buildRoutingTableImpl('l) }




def buildRoutingTableImpl(
                           routesExpr: Expr[List[Router.Route[Router.HttpMethod]]]
                         )(using Quotes): Expr[Map[String, Router.Route[Router.HttpMethod]]] = {
  import quotes.reflect.*

  // Match on the structure of the List expression
  routesExpr match {
    case '{scala.collection.immutable.List.apply[Router.Route[Router.HttpMethod]](${Varargs(routeExprs)}*)} =>
      // Now routeExprs is a Seq[Expr[Router.Route[Router.HttpMethod]]]

      // Process each route expression
      val tupleExprs = routeExprs.map { routeExpr =>
        // Create an expression for the key
        val keyExpr = '{
          val route = $routeExpr
          route.method.toString + ":" + route.path
        }

        // Create a tuple expression
        '{ ($keyExpr, $routeExpr) }
      }

      // Convert to an expression of list of tuples
      val entriesListExpr = Expr.ofList(tupleExprs.toList)

      // Create the Map
      '{ Map.from($entriesListExpr) }

    case _ =>
      report.error("Routes must be a literal list", routesExpr)
      '{ Map.empty[String, Router.Route[Router.HttpMethod]] }
  }
}

