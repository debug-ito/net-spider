# net-spider

**This is still work in progress.**

net-spider is a graph database middleware to maintain a time-varying graph. It stores history of local findings, and creates a snapshot graph at specific time in the past.

The architecture of net-spider is like:

    input
    (local findings)
      |
      |         output
      |         (snapshot graph)
      |            ^
      v            |
    +=================+
    |    net-spider   |
    +=================+
    +-----------------+
    |    Tinkerpop    |
    | Gremlin Server  |
    |                 |
    | (history graph) |
    +-----------------+

As input, net-spider takes local findings of the time-varying graph. A local finding is a local state of a node observed at specific time.

All local findings are stored in the graph database with timestamps. So the database stores the history of the time-varying graph (history graph).

The history graph can be queried via net-spider. When queried, it traverses the history graph to build a graph that should be the state of the time-varying graph at the specific time in the past.

Maintaining the graph database is delegated to [Tinkerpop Gremlin Server](http://tinkerpop.apache.org/). You have to set up a server instance seprately.


## Use case

Suppose you have a network of Cisco switches in your organization.

The network is a graph where the switches are the nodes and the cables between them are links. Some members in your organization are allowed to add new switches to the network. Sometimes some switches and cables get out of order. So the graph is a time-varying graph.

Now how can you keep track of the status of the network?

The switches can use Cisco Discovery Protocol (CDP) and/or Link Layer Discovery Protocol (LLDP), so each switch maintains the list of its current neighbors. Those neighbor information can be retrieved by SNMP.

The neighbor information retrieved by SNMP is a local finding. The information is obtained locally by a specific switch at specific time.

By putting the neighbor information to net-spider, it connects those information together to construct the history graph of your network of switches. The history graph tells you not just the latest status of the network, but also its status in the past.

## Basic usage

To use net-spider, first you have to set up Tinkerpop Gremlin Server and its underlying graph database.

For example if you use [JanusGraph](http://janusgraph.org/), you can start Gremlin Server with

    $ ./bin/janusgraph.sh start

By default, it accepts WebSocket connections at port 8182.

Then in your application, connect to the server and get `Spider` object.

```haskell basic
{-# LANGUAGE OverloadedStrings #-}
import Control.Exception.Safe (bracket)
import Data.Either (partitionEithers)
import Data.List (sort, sortOn)
import Data.Text (Text)
import Test.Hspec
import Test.Hspec.NeedEnv (needEnvHostPort, EnvMode(Need))

import NetSpider.Pair (Pair(..))
import NetSpider.Input
  ( Spider, connectWS, close,
    FoundNode(..), FoundLink(..), LinkState(LinkBidirectional),
    fromEpochSecond, addFoundNode, clearAll, getLatestSnapshot
  )
import NetSpider.Output
  ( nodeId, nodeTimestamp, linkNodePair, linkTimestamp )


main :: IO ()
main = hspec $ specify "basic" $ do
  (gremlin_server_host, gremlin_server_port) <- needEnvHostPort Need "NET_SPIDER_TEST"
  bracket (connectWS gremlin_server_host gremlin_server_port) close $ doWithSpider
```

Use `connectWS` function to get `Spider` object, and `close` function to close it. We recommend using `bracket`.

To input a local finding, use `addFoundNode` function.

```haskell basic
doWithSpider :: Spider Text () () () -> IO ()
doWithSpider spider = do
  let finding1 = FoundNode
                 { subjectNode = "switch1",
                   foundAt = fromEpochSecond 1534769618,
                   neighborLinks = links1,
                   nodeAttributes = ()
                 }
      links1 = [ FoundLink
                 { targetNode = "switch2",
                   linkState = LinkBidirectional,
                   linkAttributes = ()
                 },
                 FoundLink
                 { targetNode = "switch3",
                   linkState = LinkBidirectional,
                   linkAttributes = ()
                 }
               ]
  clearAll spider -- Delete all data from the databse for testing
  addFoundNode spider finding1
```

A local finding is expressed as `FoundNode` type. In the above example, we input a local finding observed at the switch named "switch1". `FoundNode` includes the timestamp (`foundAt`) at which the local finding was observed, and list of neighbors (`neighborLinks`) adjacent to this node. These are what we would get via SNMP + CDP/LLDP.

OK, let's observe the switch2 and input that local finding as well.

```haskell basic
  let finding2 = FoundNode
                 { subjectNode = "switch2",
                   foundAt = fromEpochSecond 1534770022,
                   neighborLinks = links2,
                   nodeAttributes = ()
                 }
      links2 = [ FoundLink
                 { targetNode = "switch4",
                   linkState = LinkBidirectional,
                   linkAttributes = ()
                 },
                 FoundLink
                 { targetNode = "switch1",
                   linkState = LinkBidirectional,
                   linkAttributes = ()
                 }
               ]
  addFoundNode spider finding2
```

So, by combining these local findings, we can infer the network topology is like:

```
[switch1]---[switch2]---[switch4]
    |
[switch3]
```

The above graph can be obtained by `getLatestSnapshot` function. This function retrieves the snapshot graph that is supposed to be the latest state of the network.

```haskell basic
  (raw_nodes, raw_links) <- getLatestSnapshot spider "switch1"
```

The snapshot graph is expressed as lists of `SnapshotNode`s and `SnapshotLink`s. They are independent of each other, so it is easy to render the graph using, for example, [graphviz](http://graphviz.org/).

```haskell basic
  let nodes = sort raw_nodes
      links = sortOn linkNodePair raw_links
  map nodeId nodes `shouldBe` [ "switch1",
                                "switch2",
                                "switch3",
                                "switch4"
                              ]
  map nodeTimestamp nodes `shouldBe` [ Just $ fromEpochSecond 1534769618,
                                       Just $ fromEpochSecond 1534770022,
                                       Nothing,
                                       Nothing
                                     ]
  map linkNodePair links `shouldBe` map Pair [ ("switch1", "switch2"),
                                               ("switch1", "switch3"),
                                               ("switch2", "switch4")
                                             ]
  map linkTimestamp links `shouldBe` [ fromEpochSecond 1534770022,
                                       fromEpochSecond 1534769618,
                                       fromEpochSecond 1534770022
                                     ]
```

## The Spider type

In the above example, maybe you noticed that the `Spider` type has a lot of type variables.

```haskell
Spider n na fla sla
```

These type variables determine the data model of your history graph and snapshot graph.

- Type `n`: the type of the node ID. For a simple graph we recommend using `Text`, because it should be supported by any Gremlin implementation.
- Type `na`: the type of node attributes. It's `()` if your node doesn't have any attribute.
- Type `fla`: the type of link attributes in local findings. It's `()` if your link doesn't have any attribute.
- Type `sla`: the type of link attributes in snapshot graphs. Similar to `fla`.

You are supposed to set these type variables based on your application. Because these types are unlikely to vary inside an application, it's a good idea to declare a type alias for `Spider`.

```haskell
type MySpider = Spider Text () () ()
```

## Node and link attributes

## Multiple links between a pair of nodes

## Merge local findings by end nodes of a link

## Author

Toshio Ito <debug.ito@gmail.com>
