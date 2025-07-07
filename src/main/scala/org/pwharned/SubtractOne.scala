package org.pwharned

import org.pwharned.rpc.{RpcEndpoint, RpcSchema, RpcServer, listToCaseClass}

case class SubtractOne(args: List[Int])

case class SubtractOneArgs(a: Int, b: Int)

case class SubtractOneResult(r: Int)

inline given SubtractOneEndpoint: RpcEndpoint[SubtractOneArgs, SubtractOneResult]:
  val name = "subtractOne"

  def call(p: SubtractOneArgs): SubtractOneResult = SubtractOneResult(p.a - p.b)

  inline override def decodeParams(args: List[Int | String]): Either[String, SubtractOneArgs] =
    try Right(listToCaseClass[SubtractOneArgs](args))
    catch
      case e: Throwable =>
        Left(s"bad args for SubtractOneArgs: ${e.getMessage}")

  override def schemaP: RpcSchema[SubtractOneArgs] = RpcSchema[SubtractOneArgs]

  override def schemaR: RpcSchema[SubtractOneResult] = RpcSchema[SubtractOneResult]


inline def rpcServer = new RpcServer(endpoints = List(SubtractOneEndpoint))
