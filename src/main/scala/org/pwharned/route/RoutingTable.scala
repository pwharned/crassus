package org.pwharned.route

import org.pwharned.http.HttpMethod.HttpMethod
import org.pwharned.http.{HttpPath, Identifier, IdentifierOrSegment, Segment}
import org.pwharned.macros.buildRoutingTable
import org.pwharned.route.Router
import org.pwharned.http.HttpPath.*

// Our RoutingTable is now indexed by the HttpMethod.
// (Each HTTP method gets its own tree.)
object RoutingTable:

  // Define the type for our routing table,
  // which is a Map from HTTP method to a path-tree Node.
  type RoutingTableType = Map[HttpMethod, Node]

  // A tree node for routing.
  final case class Node(
                         // Stores a route if one exactly matches the path at this node.
                         route: Option[Router.Route[HttpMethod]] = None,
                         // Children for static segments (literal matches).
                         children: Map[IdentifierOrSegment, Node] = Map.empty,
                         // Wildcard branch for dynamic segments.
                         // The key is the Identifier (which carries the parameter name).
                         wildcard: Option[(Identifier, Node)] = None
                       ):

    // Insert a route into the tree given a list of path segments.
    def insert(
                segments: List[IdentifierOrSegment],
                route: Router.Route[HttpMethod]
              ): Node =
      segments match
        case Nil =>
          // At the end of the path, store the route.
          this.copy(route = Some(route))
        case head :: tail =>
          head match
            case id: Identifier =>
              // This segment is dynamic. Use the wildcard branch.
              wildcard match
                case Some((existingId, child)) =>
                  // Continue down the existing wildcard branch.
                  this.copy(wildcard = Some((existingId, child.insert(tail, route))))
                case None =>
                  // No existing wildcard; create one.
                  this.copy(wildcard = Some((id, Node().insert(tail, route))))
            case seg: Segment =>
              // Static segment: use the literal as the key.
              val child = children.getOrElse(seg, Node())
              val updatedChild = child.insert(tail, route)
              this.copy(children = children.updated(seg, updatedChild))

  end Node

  // Build a routing table from a list of routes.
  // We'll build a Map indexed by the route's method.
  def build(routes: List[Router.Route[HttpMethod]]): RoutingTableType =
    routes.foldLeft(Map.empty[HttpMethod, Node]) { (acc, route) =>
      // Fetch the current tree for this method (or start with a fresh Node).
      val currentTree = acc.getOrElse(route.method, Node())
      // Insert the route into the tree using the path's segments.
      val updatedTree = currentTree.insert(route.path.asList, route)
      // Update the map.
      acc.updated(route.method, updatedTree)
    }

  // Look up a route by both HTTP method and raw path (e.g. "/api/user/44").
  // If a match is found, return the route together with a Map of captured parameters.
  def lookup(
              table: RoutingTableType,
              method: HttpMethod,
              rawPath: String
            ): Option[(Router.Route[HttpMethod], Map[Identifier, IdentifierOrSegment])] =
    // Look up the tree for the given HTTP method.
    table.get(method).flatMap { root =>
      // Parse the raw path into segments using your HttpPath implementation.
      val segments: List[IdentifierOrSegment] = HttpPath(rawPath).asList

      // Recursive lookup function.
      def loop(
                node: Node,
                segs: List[IdentifierOrSegment],
                params: Map[Identifier, IdentifierOrSegment]
              ): Option[(Router.Route[HttpMethod], Map[Identifier, IdentifierOrSegment])] =
        segs match
          case Nil =>
            // At the end of the segments, if the node defines a route, return it.
            node.route.map(r => (r, params))
          case head :: tail =>
            // First, attempt a static match.
            node.children.get(head) match
              case Some(child) =>
                loop(child, tail, params) match
                  // Use the static match if found.
                  case some @ Some(_) => some
                  // Otherwise, try the wildcard branch.
                  case None =>
                    node.wildcard.flatMap { case (wildId, wildChild) =>
                      loop(wildChild, tail, params + (wildId -> head))
                    }
              case None =>
                // If no static match exists, try the wildcard branch.
                node.wildcard.flatMap { case (wildId, wildChild) =>
                  loop(wildChild, tail, params + (wildId -> head))
                }
      // Perform the lookup starting with an empty parameter map.
      loop(root, segments, Map.empty)
    }

end RoutingTable
