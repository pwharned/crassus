package org.pwharned.http

import org.pwharned.json.JsonSerializer

import scala.util.Try
import scala.concurrent.{ExecutionContext, Future}
import org.pwharned.database.HKD.~>.idToId
import org.pwharned.json.serialize

import scala.util.{Try, Success, Failure}

def handleTransaction[A <: Product](
                                     transaction: => Future[Try[Iterator[A]]]
                                   )(using ec: ExecutionContext, json: JsonSerializer[A]): Future[HttpResponse] =
  transaction.map {
    case Success(iterator) =>
      // Use the JsonSerializer instance explicitly to serialize each element.
      // This avoids ambiguities with any extension method.
      HttpResponse.ok(iterator.serialize, headers = Headers.empty.add("content-type", "application/json"))    
    case Failure(exception) =>
      HttpResponse.error(exception.getMessage)
  }.recover {
    case exception =>
      HttpResponse.error(exception.getMessage)
  }
