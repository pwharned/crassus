package org.pwharned.rpc

type RpcId = String | Int


// 2) The generic request wrapper
case class RpcRequest(
                               jsonrpc: String = "2.0",
                               method:   String,
                               params:   List[RpcId],
                               id:       Int
                             )


