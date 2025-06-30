package org.pwharned.route

import org.pwharned.http.HttpMethod.{GET, HttpMethod}
import org.pwharned.http.HttpPath.{HttpPath}
import org.pwharned.http.HttpRequest.HttpRequest
import org.pwharned.http.asPath
import org.pwharned.http.Identifier.Identifier
import org.pwharned.http.{HttpPath, HttpRequest, HttpResponse, Identifier, Segment}
import org.pwharned.route.Router
import org.pwharned.route.Router.{Route, route}

import scala.annotation.tailrec
import scala.concurrent.Future

object RoutingTable:
  // The Node now explicitly creates its children as a Map of type Map[A, Node[A]]
  final case class Node[A <: Segment,P[_] <: Protocal[_]](
                                                   id: A,
                                                   route: Option[Route[P,HttpMethod]] = None,
                                                   var children: Branch[A,P] = Map.empty[A, Node[A,P]]
                                                 ):
    def insert(id: A, r: Route[P,HttpMethod]): Unit =
      children = children.updated(id, Node[A,P](id, Some(r)))

  // Define Branch and RoutingTable as opaque types that our code wraps around Maps.
  opaque type Branch[A <: Segment,P[_] <: Protocal[_]] = Map[A, Node[A,P]]
  opaque type RoutingTable[A<:Segment,P[_] <: Protocal[_]] = Map[HttpMethod, Branch[A,P]]

  extension[A <: Segment,P[_] <: Protocal[_]](b: Branch[A,P])
    private def lookup(id: A): Option[Node[A,P]] = b.get(id)

    @tailrec
    private def lookPath(path: List[A]): Branch[A,P] =
      path match
        case head :: next =>
          lookup(head) match
            case Some(node) => node.children.lookPath(next)
            case None       => b
        case Nil => b

    // Recursive insert (for intermediate segments)
    private def insert(path: List[A], route: Route[Protocal,HttpMethod]): Branch[A,P] =
      path match
        case head :: next =>
          val updatedNode: Node[A,P] = lookup(head) match
            case Some(existingNode) =>
              // Recursively update the children subtree
              existingNode.children = existingNode.children.insert(next, route)
              existingNode
            case None =>
              // We create a new node using the incoming head.
              val newNode: Node[A,P] = Node[A,P](head)
              newNode.children = newNode.children.insert(next, route)
              newNode
          b.updated(head, updatedNode)
        case Nil => b // Should not happen

    // Insert the final element in the path. We want to attach the route at the last node.
    private def insertFinal(path: List[A], route: Route[P,HttpMethod]): Branch[A,P] =
      path match
        case head :: Nil =>
          val updatedNode: Node[A,P] = lookup(head) match
            case Some(existingNode) => existingNode.copy(route = Some(route))
            case None               => Node[A,P](head, Some(route))
          b.updated(head, updatedNode)
        case head :: next =>
          val updatedNode: Node[A,P] = lookup(head) match
            case Some(existingNode) =>
              existingNode.children = existingNode.children.insertFinal(next, route)
              existingNode
            case None =>
              val newNode: Node[A,P] = Node[A,P](head)
              newNode.children = newNode.children.insertFinal(next, route)
              newNode
          b.updated(head, updatedNode)
        case Nil => b

  extension[A <: Segment,P[_] <: Protocal[_]](table: RoutingTable[A,P])
    // Here we traverse the tree to locate the matching route.
    def find(m: HttpMethod, p: HttpPath): Option[Node[A,P]] =
      // We assume that the path segments have type A. In our usage below A is IdentifierOrSegment.
      table.get(m).flatMap(branch => findNode(branch, p.segments.asInstanceOf[List[A]]  ))

    @tailrec
    private def findNode(branch: Branch[A,P], path: List[A]): Option[Node[A,P]] =
      path match
        case head :: next =>
          // Try an exact match first.
          branch.get(head) match {
            case someNode@Some(node) =>
              if (next.isEmpty) someNode else findNode(node.children, next)
            case None =>
              // If no exact match, see if there is a dynamic parameter match,
              // i.e. a key that is an Identifier.
              branch.collectFirst {
                case (Segment.Dynamic(key), node)  => node
              } match {
                case Some(node) =>
                  if (next.isEmpty) Some(node) else findNode(node.children, next)
                case None => None
              }
          }
        case Nil => None


  // We fix build so that it returns a RoutingTable specialized to IdentifierOrSegment.
  def build[A<:Segment,P[_] <: Protocal[_]](routes: List[Router.Route[P,HttpMethod]]): RoutingTable[A,P] =
    routes.foldLeft(Map.empty[HttpMethod, Branch[A,P]]) { (acc, route) =>
      val currentTree = acc.getOrElse(
        route.method,
        Map.empty[A, Node[A,P]]
      )
      val updatedTree = currentTree.insertFinal(route.path.segments.asInstanceOf[List[A]], route)
      acc.updated(route.method, updatedTree)
    }

end RoutingTable

// Our RoutingTable is now indexed by the HttpMethod (each HTTP method gets its own tree).

