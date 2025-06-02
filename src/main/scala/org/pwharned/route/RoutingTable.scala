package org.pwharned.route

import org.pwharned.http.HttpMethod.{GET, HttpMethod}
import org.pwharned.http.HttpPath.{HttpPath}
import org.pwharned.http.HttpRequest.HttpRequest
import org.pwharned.macros.asPath
import org.pwharned.http.Identifier.Identifier
import org.pwharned.http.{HttpPath, HttpRequest, HttpResponse, Identifier, Segment}
import org.pwharned.route.Router
import org.pwharned.route.Router.{Route, route}

import scala.annotation.tailrec
import scala.concurrent.Future

object RoutingTable:
  // The Node now explicitly creates its children as a Map of type Map[A, Node[A]]
  final case class Node[A <: Segment](
                                                   id: A,
                                                   route: Option[Route[HttpMethod]] = None,
                                                   var children: Branch[A] = Map.empty[A, Node[A]]
                                                 ):
    def insert(id: A, r: Route[HttpMethod]): Unit =
      children = children.updated(id, Node[A](id, Some(r)))

  // Define Branch and RoutingTable as opaque types that our code wraps around Maps.
  opaque type Branch[A <: Segment] = Map[A, Node[A]]
  opaque type RoutingTable = Map[HttpMethod, Branch[Segment]]

  extension[A <: Segment](b: Branch[A])
    private def lookup(id: A): Option[Node[A]] = b.get(id)

    @tailrec
    private def lookPath(path: List[A]): Branch[A] =
      path match
        case head :: next =>
          lookup(head) match
            case Some(node) => node.children.lookPath(next)
            case None       => b
        case Nil => b

    // Recursive insert (for intermediate segments)
    private def insert(path: List[A], route: Route[HttpMethod]): Branch[A] =
      path match
        case head :: next =>
          val updatedNode: Node[A] = lookup(head) match
            case Some(existingNode) =>
              // Recursively update the children subtree
              existingNode.children = existingNode.children.insert(next, route)
              existingNode
            case None =>
              // We create a new node using the incoming head.
              val newNode: Node[A] = Node[A](head)
              newNode.children = newNode.children.insert(next, route)
              newNode
          b.updated(head, updatedNode)
        case Nil => b // Should not happen

    // Insert the final element in the path. We want to attach the route at the last node.
    private def insertFinal(path: List[A], route: Route[HttpMethod]): Branch[A] =
      path match
        case head :: Nil =>
          val updatedNode: Node[A] = lookup(head) match
            case Some(existingNode) => existingNode.copy(route = Some(route))
            case None               => Node[A](head, Some(route))
          b.updated(head, updatedNode)
        case head :: next =>
          val updatedNode: Node[A] = lookup(head) match
            case Some(existingNode) =>
              existingNode.children = existingNode.children.insertFinal(next, route)
              existingNode
            case None =>
              val newNode: Node[A] = Node[A](head)
              newNode.children = newNode.children.insertFinal(next, route)
              newNode
          b.updated(head, updatedNode)
        case Nil => b

  extension(table: RoutingTable)
    // Here we traverse the tree to locate the matching route.
    def find(m: HttpMethod, p: HttpPath): Option[Node[Segment]] =
      // We assume that the path segments have type A. In our usage below A is IdentifierOrSegment.
      table.get(m).flatMap(branch => findNode(branch, p.segments))

    @tailrec
    private def findNode(branch: Branch[Segment], path: List[Segment]): Option[Node[Segment]] =
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
  def build(routes: List[Router.Route[HttpMethod]]): RoutingTable =
    routes.foldLeft(Map.empty[HttpMethod, Branch[Segment]]) { (acc, route) =>
      val currentTree = acc.getOrElse(
        route.method,
        Map.empty[Segment, Node[Segment]]
      )
      val updatedTree = currentTree.insertFinal(route.path.segments, route)
      acc.updated(route.method, updatedTree)
    }

end RoutingTable

// Our RoutingTable is now indexed by the HttpMethod (each HTTP method gets its own tree).

@main def testRoutingTable(): Unit =
  import scala.concurrent.ExecutionContext.Implicits.global

  // Define some example routes.
  val route1: Route[HttpMethod] = route(
    GET,
    "/users/profile/{user_id}".asPath,
    (req: HttpRequest) => Future(HttpResponse.ok("Profiles"))
  )
  val route2: Route[HttpMethod] = route(
    GET,
    "/users/ids".asPath,
    (req: HttpRequest) => Future(HttpResponse.ok("Ids"))
  )
  val route3: Route[HttpMethod] = route(
    GET,
    "/users/friends".asPath,
    (req: HttpRequest) => Future(HttpResponse.ok("friends"))
  )

  // Build the routing table. Note the explicit type parameter.
  val routes: List[Route[HttpMethod]] = List(route1, route2, route3)
  val routingTable: RoutingTable.RoutingTable = RoutingTable.build(routes)

  // Test lookup for existing paths.
  val foundUsersProfile = routingTable.find(GET, "/users/profile/4".asPath)
  val foundUserIds      = routingTable.find(GET, "/users/ids".asPath)

  println(s"Lookup users/profile (GET): ${foundUsersProfile}")
  println(s"Lookup users/ids (GET): ${foundUserIds.isDefined}")
