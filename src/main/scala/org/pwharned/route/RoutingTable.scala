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




object RoutingTable:
  // The Node now explicitly creates its children as a Map of type Map[A, Node[A]]
  final case class Node[P[_] <: Protocal[_], Req, Res](
                                                   id: Segment,
                                                   route: Option[Route[P,HttpMethod,Req, Res]] = None,
                                                   var children: Branch[P] = Map.empty[Segment, Node[P,?, ?]]

                                               ):
    inline def insert(id: Segment, r: Route[P,HttpMethod,Req, Res]): Unit =
      children = children.updated(id, Node[P,Req, Res](id = id, route = Some(r) ))

  opaque type Branch[P[_] <: Protocal[_]] =
    Map[Segment, Node[P, ?,?]]

  opaque type RoutingTable[H <: HttpMethod, P[_] <: Protocal[_]] =
    Map[H, Branch[P]]


  extension[P[_] <: Protocal[_]](b: Branch[P])
    private def lookup(id: Segment): Option[Node[P, ?,?]] = b.get(id)

    @tailrec
    private def lookPath(path: List[Segment]): Branch[P] =
      path match
        case head :: next =>
          lookup(head) match
            case Some(node) => node.children.lookPath(next)
            case None       => b
        case Nil => b

    // Recursive insert (for intermediate segments)
    def insert(path: List[Segment], route: Route[Protocal,HttpMethod,?, ?]): Branch[P] =
      path match
        case head :: next =>
          val updatedNode: Node[P, ?,?] = lookup(head) match
            case Some(existingNode) =>
              // Recursively update the children subtree
              existingNode.children = existingNode.children.insert(next, route)
              existingNode
            case None =>
              // We create a new node using the incoming head.
              val newNode: Node[P,?, ?] = Node(head)
              newNode.children = newNode.children.insert(next, route)
              newNode
          b.updated(head, updatedNode)
        case Nil => b // Should not happen

    // Insert the final element in the path. We want to attach the route at the last node.
    def insertFinal(path: List[Segment], route: Route[P,HttpMethod,?, ?]): Branch[P] =
      path match
        case head :: Nil =>
          val updatedNode: Node[P, ?,?] = lookup(head) match
            case Some(existingNode) => existingNode.copy(route = Some(route))
            case _               =>           Node(
              id       = head,
              route    = Some(route),
              children = Map.empty
            )
          b.updated(head, updatedNode)
        case head :: next =>
          val updatedNode: Node[P, ?,?] = lookup(head) match
            case Some(existingNode) =>
              existingNode.children = existingNode.children.insertFinal(next, route)
              existingNode
            case None =>
              val newNode: Node[P, ?, ?] = Node(head)
              newNode.children = newNode.children.insertFinal(next, route)
              newNode
          b.updated(head, updatedNode)
        case Nil => b

  def buildLazy[P[_] <: Protocal[_]](
                                  routes: List[Lazy[Router.Route[P, HttpMethod, _, _]]]
                                ): RoutingTable[HttpMethod, P] =
    routes.foldLeft(Map.empty[HttpMethod, Branch[P]]) { (acc, route) =>
      val currentTree = acc.getOrElse(route.value.method, Map.empty)
      val updatedTree = currentTree.insertFinal(route.value.path.segments, route.value)
      acc.updated(route.value.method, updatedTree)
    }
  def build[P[_] <: Protocal[_]](
                                  routes: List[Router.Route[P, HttpMethod, _, _]]
                                ): RoutingTable[HttpMethod, P] =
    routes.foldLeft(Map.empty[HttpMethod, Branch[P]]) { (acc, route) =>
      val currentTree = acc.getOrElse(route.method, Map.empty)
      val updatedTree = currentTree.insertFinal(route.path.segments, route)
      acc.updated(route.method, updatedTree)
    } 
  extension[H<:HttpMethod, P[_] <: Protocal[_]](table: RoutingTable[H, P])
    // Here we traverse the tree to locate the matching route.
    def find(m: H, p: HttpPath): Option[Node[P, ?,?]] =
      // We assume that the path segments have type A. In our usage below A is IdentifierOrSegment.
      table.get(m).flatMap(branch => findNode(branch, p.segments  ))

    @tailrec
    private def findNode(branch: Branch[P], path: List[Segment] ): Option[Node[P,?,?]] =
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



