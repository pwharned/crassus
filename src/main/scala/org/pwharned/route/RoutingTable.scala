package org.pwharned.route

import org.pwharned.http.HttpMethod.HttpMethod
import org.pwharned.http.HttpPath.HttpPath
import org.pwharned.http.{HttpPath, Protocal, Segment}
import org.pwharned.openapi.{Schema, schema}
import org.pwharned.route.Router
import org.pwharned.route.Router.Route
import org.pwharned.route.RoutingTable.RoutingTable
import org.pwharned.`lazy`.Lazy

import scala.annotation.tailrec
import scala.compiletime.summonInline
import scala.reflect.ClassTag


trait AnyNode[P[_] <: Protocal[_]] {
  def id: Segment

  var children: Map[Segment, AnyNode[P]]

  def route: Option[Route[P, HttpMethod, ?]]
  
}

object RoutingTable:
  // The Node now explicitly creates its children as a Map of type Map[A, Node[A]]
  final case class Node[P[_] <: Protocal[_], T](
                                                   id: Segment,
                                                   route: Option[Route[P,HttpMethod,T]] = None,
                                                   var children: Branch[P] = Map.empty[Segment, Node[P,?]]

                                               ) extends AnyNode[P]:
    inline def insert(id: Segment, r: Route[P,HttpMethod,T]): Unit =
      children = children.updated(id, Node[P,T](id = id, route = Some(r) ))

  opaque type Branch[P[_] <: Protocal[_]] =
    Map[Segment, AnyNode[P]]

  opaque type RoutingTable[H <: HttpMethod, P[_] <: Protocal[_]] =
    Map[H, Branch[P]]


  extension[P[_] <: Protocal[_]](b: Branch[P])
    private def lookup(id: Segment): Option[AnyNode[P]] = b.get(id)

    @tailrec
    private def lookPath(path: List[Segment]): Branch[P] =
      path match
        case head :: next =>
          lookup(head) match
            case Some(node) => node.children.lookPath(next)
            case None       => b
        case Nil => b

    // Recursive insert (for intermediate segments)
    def insert(path: List[Segment], route: Route[Protocal,HttpMethod,?]): Branch[P] =
      path match
        case head :: next =>
          val updatedNode: AnyNode[P] = lookup(head) match
            case Some(existingNode) =>
              // Recursively update the children subtree
              existingNode.children = existingNode.children.insert(next, route)
              existingNode
            case None =>
              // We create a new node using the incoming head.
              val newNode: Node[P,?] = Node(head)
              newNode.children = newNode.children.insert(next, route)
              newNode
          b.updated(head, updatedNode)
        case Nil => b // Should not happen

    // Insert the final element in the path. We want to attach the route at the last node.
    def insertFinal(path: List[Segment], route: Route[P,HttpMethod,?]): Branch[P] =
      path match
        case head :: Nil =>
          val updatedNode: AnyNode[P] = lookup(head) match
            case Some(existingNode) => existingNode
            case None               => Node(head)
          b.updated(head, updatedNode)
        case head :: next =>
          val updatedNode: AnyNode[P] = lookup(head) match
            case Some(existingNode) =>
              existingNode.children = existingNode.children.insertFinal(next, route)
              existingNode
            case None =>
              val newNode: Node[P, ?] = Node(head)
              newNode.children = newNode.children.insertFinal(next, route)
              newNode
          b.updated(head, updatedNode)
        case Nil => b

  def build[P[_] <: Protocal[_]](
                                  routes: List[Lazy[Router.Route[P, HttpMethod, _]]]
                                ): RoutingTable[HttpMethod, P] =
    routes.foldLeft(Map.empty[HttpMethod, Branch[P]]) { (acc, route) =>
      val currentTree = acc.getOrElse(route.value.method, Map.empty)
      val updatedTree = currentTree.insertFinal(route.value.path.segments, route.value)
      acc.updated(route.value.method, updatedTree)
    }
    
  extension[H<:HttpMethod, P[_] <: Protocal[_]](table: RoutingTable[H, P])
    // Here we traverse the tree to locate the matching route.
    def find(m: H, p: HttpPath): Option[AnyNode[P]] =
      // We assume that the path segments have type A. In our usage below A is IdentifierOrSegment.
      table.get(m).flatMap(branch => findNode(branch, p.segments  ))

    @tailrec
    private def findNode(branch: Branch[P], path: List[Segment] ): Option[AnyNode[P]] =
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




end RoutingTable



