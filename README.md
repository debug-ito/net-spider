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

## Prelude

Before we enter the detail, here we define the common imports. This is because this README is also a test script.

```haskell common
{-# LANGUAGE OverloadedStrings #-}
import Control.Applicative ((<$>), (<*>))
import Control.Category ((<<<))
import Control.Exception.Safe (bracket)
import Data.Either (partitionEithers)
import Data.List (sort, sortOn)
import Data.Text (Text)
import Test.Hspec
import Test.Hspec.NeedEnv (needEnvHostPort, EnvMode(Need))
```

## Basic usage

To use net-spider, first you have to set up Tinkerpop Gremlin Server and its underlying graph database.

For example if you use [JanusGraph](http://janusgraph.org/), you can start Gremlin Server with

    $ ./bin/janusgraph.sh start

By default, it accepts WebSocket connections at port 8182.

Then in your application, connect to the server and get `Spider` object.

```haskell basic
import NetSpider.Pair (Pair(..))
import NetSpider.Spider
  (Spider, connectWS, close, addFoundNode, clearAll, getSnapshotSimple)
import NetSpider.Found
  (FoundNode(..), FoundLink(..), LinkState(LinkBidirectional))
import NetSpider.Timestamp (fromEpochSecond)
import NetSpider.Snapshot
  (nodeId, nodeTimestamp, linkNodePair, linkTimestamp)


main :: IO ()
main = hspec $ specify "basic" $ do
  (gremlin_server_host, gremlin_server_port) <- needEnvHostPort Need "NET_SPIDER_TEST"
  bracket (connectWS gremlin_server_host gremlin_server_port) close $ doWithSpider
```

Use `connectWS` function to get `Spider` object, and `close` function to close it. We recommend using `bracket`.

To input a local finding, use `addFoundNode` function.

```haskell basic
doWithSpider :: Spider Text () () -> IO ()
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

The above graph can be obtained by `getSnapshotSimple` function. This function retrieves the snapshot graph that is supposed to be the latest state of the network.

```haskell basic
  (raw_nodes, raw_links) <- getSnapshotSimple spider "switch1"
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
Spider n na fla
```

These type variables determine the data model of your history graph and snapshot graph.

- Type `n`: the type of the node ID. For a simple graph we recommend using `Text`, because it should be supported by any Gremlin implementation.
- Type `na`: the type of node attributes. It's `()` if your node doesn't have any attribute.
- Type `fla`: the type of link attributes in local findings. It's `()` if your link doesn't have any attribute.

You are supposed to set these type variables based on your application. Because these types are unlikely to vary inside an application, it's a good idea to declare a type alias for `Spider`.

```haskell
type MySpider = Spider Text () ()
```

## Node and link attributes

Nodes and links in your history graph can have time-varying attributes. You can store those attributes in the history graph. In the snapshot graph, you can basically get the latest values of those attributes.

For example, let's monitor the total number of packets that the switch has transmitted and received. First, we define the data type for the node attributes.

```haskell attrs
import Data.Greskell (newBind, gProperty, parseOneValue)

import NetSpider.Spider
  (Spider, connectWS, close, clearAll, addFoundNode, getSnapshotSimple)
import NetSpider.Found (FoundNode(..), FoundLink(..))
import NetSpider.Graph (NodeAttributes(..))
import NetSpider.Timestamp (fromEpochSecond)
import NetSpider.Snapshot (nodeId, nodeTimestamp)
import qualified NetSpider.Snapshot as Sn


data PacketCount =
  PacketCount
  { transmitCount :: Int,
    receiveCount :: Int
  }
  deriving(Show,Eq,Ord)
```

Second, make it instance of `NodeAttributes` class and implement its methods. This is necessary for `Spider` to store/load the attributes to/from the history graph.

```haskell attrs
instance NodeAttributes PacketCount where
  writeNodeAttributes packet_count = do
    tx_count <- newBind $ transmitCount packet_count
    rx_count <- newBind $ receiveCount packet_count
    return (gProperty "tx_count" tx_count <<< gProperty "rx_count" rx_count)
  parseNodeAttributes props =
    PacketCount
    <$> parseOneValue "tx_count" props
    <*> parseOneValue "rx_count" props
```

`writeNodeAttributes` function is supposed to return Gremlin steps that store the given data in the graph database. `parseNodeAttributes` function is supposed to construct the attributes from the given property data. Those functions are based on [greskell package](https://github.com/debug-ito/greskell).

Once you implement `NodeAddtributes` instance, you can use `PacketCount` as the `na` type-variable.


```haskell attrs
main = hspec $ specify "node attributes" $ do
  (host, port) <- needEnvHostPort Need "NET_SPIDER_TEST"
  bracket (connectWS host port) close $ doWithSpider

doWithSpider :: Spider Text PacketCount () -> IO ()
doWithSpider spider = do
  clearAll spider
  let finding1 = FoundNode
                 { subjectNode = "switch1",
                   foundAt = fromEpochSecond 1536496858,
                   neighborLinks = [],
                   nodeAttributes = attrs1
                 }
      attrs1 = PacketCount
               { transmitCount = 15242,
                 receiveCount = 22301
               }
  addFoundNode spider finding1
```

Of course, you can retrieve the node attributes in the snapshot graph.

```haskell attrs
  (raw_nodes1, []) <- getSnapshotSimple spider "switch1"
  map nodeId raw_nodes1 `shouldBe` ["switch1"]
  map nodeTimestamp raw_nodes1 `shouldBe` [Just $ fromEpochSecond 1536496858]
  map Sn.nodeAttributes raw_nodes1 `shouldBe`
    [ Just PacketCount { transmitCount = 15242,
                         receiveCount = 22301
                       }
    ]
```

Now let's update the PacketCount. To do that, just add a local finding with a newer timestamp. `getSnapshotSimple` returns the latest state of the graph.

```haskell attrs
  let finding2 = FoundNode
                 { subjectNode = "switch1",
                   foundAt = fromEpochSecond 1536669543,
                   neighborLinks = [],
                   nodeAttributes = attrs2
                 }
      attrs2 = PacketCount
               { transmitCount = 20112,
                 receiveCount = 28544
               }
  addFoundNode spider finding2
  
  (raw_nodes2, []) <- getSnapshotSimple spider "switch1"
  map nodeId raw_nodes2 `shouldBe` ["switch1"]
  map nodeTimestamp raw_nodes2 `shouldBe` [Just $ fromEpochSecond 1536669543]
  map Sn.nodeAttributes raw_nodes2 `shouldBe`
    [ Just PacketCount { transmitCount = 20112,
                         receiveCount = 28544
                       }
    ]
```

Just like `FoundNode` has `nodeAttributes` field, `FoundLink` has `linkAttributes` field. To store your data type as link attributes, make that data type an instance of `LinkAttributes` class.

## Multiple links between a pair of nodes

By default, net-spider assumes there is at most one link between a pair of nodes. If it is possible in your application that there are more than one links between a pair of nodes, you have to tell `Spider` how to distinguish those links.

For example, if you use link aggregation, you often connect a pair of switches with more than one physical links. Those physical links can be distinguished by the port names of the switches.

So, let's define the data type for port names first.

```haskell multi-link
import Data.Greskell (newBind, gProperty, parseOneValue)

import NetSpider.Found (FoundNode(..), FoundLink(..), LinkState(..))
import NetSpider.Graph (LinkAttributes(..))
import NetSpider.Pair (Pair(..))
import NetSpider.Query (defQuery, unifyLinkSamples)
import NetSpider.Snapshot (linkNodeTuple)
import qualified NetSpider.Snapshot as Sn
import NetSpider.Spider
  (Spider, connectWS, close, clearAll, addFoundNode, getSnapshot)
import NetSpider.Timestamp (fromEpochSecond)
import NetSpider.Unify (LinkSample(..), unifyStd, defUnifyStdConfig, makeLinkSubId)

data Ports =
  Ports
  { subjectPort :: Text,
    targetPort :: Text
  }
  deriving (Show,Eq,Ord)

instance LinkAttributes Ports where
  writeLinkAttributes ports = do
    sp <- newBind $ subjectPort ports
    tp <- newBind $ targetPort ports
    return $ gProperty "sport" sp <<< gProperty "tport" tp
  parseLinkAttributes props =
    Ports <$> parseOneValue "sport" props <*> parseOneValue "tport" props
```

Then, put some local findings.

```haskell multi-link
main :: IO ()
main = hspec $ specify "multi-link" $ do
  (host, port) <- needEnvHostPort Need "NET_SPIDER_TEST"
  bracket (connectWS host port) close $ doWithSpider

doWithSpider :: Spider Text () Ports -> IO ()
doWithSpider spider = do
  clearAll spider
  let finding1 = FoundNode
                 { subjectNode = "switch1",
                   foundAt = fromEpochSecond 1536842590,
                   neighborLinks = links1,
                   nodeAttributes = ()
                 }
      links1 = [ FoundLink
                 { targetNode = "switch2",
                   linkState = LinkBidirectional,
                   linkAttributes = Ports "Gi0/0" "Gi0/12"
                 },
                 FoundLink
                 { targetNode = "switch2",
                   linkState = LinkBidirectional,
                   linkAttributes = Ports "Gi0/1" "Gi0/13"
                 }
               ]
  addFoundNode spider finding1
```

To get the correct snapshot graph, you have to tell `Spider` to distinguish the links by the port names as well as the switch names. To do that, use `getSnapshot` function instead of `getSnapshotSimple`, and pass an apropriate `Query` to it.

```haskell multi-link
  let linkSubIdWithPorts :: LinkSample Text Ports -> Pair (Text,Text)
      linkSubIdWithPorts ls = Pair ( (lsSubjectNode ls, subjectPort $ lsLinkAttributes ls),
                                     (lsTargetNode ls, targetPort $ lsLinkAttributes ls)
                                   )
      unifier = unifyStd $ defUnifyStdConfig { makeLinkSubId = linkSubIdWithPorts }
      query = (defQuery ["switch1"]) { unifyLinkSamples = unifier }
  (_, raw_links) <- getSnapshot spider query
```

The `linkSubIdWithPorts` function above defines the link sub-ID, an identifier for a link for a pair of switches. Here we include the `subjectPort` and `targetPort` in the link sub-ID. We also use the switch names and `Pair` type (from `NetSpider.Pair`) because the link's subject and target may be swapped.

The `linkSubIdWithPorts` function is used to construct the `unifier`, which is included in the `query`, which is passed to `getSnapshot`.

Now, let's check the result.

```haskell multi-link
  length raw_links `shouldBe` 2
  map linkNodeTuple raw_links `shouldBe`
    [("switch1","switch2"), ("switch1","switch2")]
  map Sn.linkAttributes raw_links `shouldMatchList`
    [Ports "Gi0/0" "Gi0/12", Ports "Gi0/1" "Gi0/13"]
```

If you didn't pass the `linkSubIdWithPorts` to `getSnapshot`, the result would contain just one link.

## Merge local findings by end nodes of a link

In some applications, each end node of a link can observe some independent attributes of the link. In that case, you may want to merge those attributes from the two end nodes into a single link attribute.

For example, when you use optical fibers to connect switches, it may be a good idea to monitor the signal strength to detect link failures early.

So, let's define the link attribute type for that.

```haskell merge-link-attrs
import Data.Greskell (newBind, gProperty, parseOneValue)
import Data.Scientific (Scientific)

import NetSpider.Found (FoundNode(..), FoundLink(..), LinkState(..))
import NetSpider.Graph (LinkAttributes(..))
import NetSpider.Spider
  (Spider, connectWS, close, clearAll, addFoundNode)
import NetSpider.Timestamp (fromEpochSecond)

-- | Received signal strength (dBm)
newtype RxSignal = RxSignal Scientific
                   deriving (Show,Eq,Ord)

instance LinkAttributes RxSignal where
  writeLinkAttributes (RxSignal s) = do
    s_var <- newBind s
    return $ gProperty "rx_signal" s_var
  parseLinkAttributes props =
    RxSignal <$> parseOneValue "rx_signal" props
```

Then, put local findings for a link. Note that each end node observes its own received signal strength.

```haskell merge-link-attrs
main :: IO ()
main = hspec $ specify "merge link attributes" $ do
  (host, port) <- needEnvHostPort Need "NET_SPIDER_TEST"
  bracket (connectWS host port) close $ doWithSpider

doWithSpider :: Spider Text () RxSignal -> IO ()
doWithSpider spider = do
  clearAll spider
  let finding1 = FoundNode
                 { subjectNode = "switch1",
                   foundAt = fromEpochSecond 1537189070,
                   nodeAttributes = (),
                   neighborLinks = [link1]
                 }
      link1 = FoundLink
              { targetNode = "switch2",
                linkState = LinkBidirectional,
                linkAttributes = RxSignal (-4.3)
              }
      finding2 = FoundNode
                 { subjectNode = "switch2",
                   foundAt = fromEpochSecond 1537189388,
                   nodeAttributes = (),
                   neighborLinks = [link2]
                 }
      link2 = FoundLink
              { targetNode = "switch1",
                linkState = LinkBidirectional,
                linkAttributes = RxSignal (-5.5)
              }
  addFoundNode spider finding1
  addFoundNode spider finding2
```

TODO: how to merge the two RxSignals? and what's the default behavior of getSnapshot?

## Author

Toshio Ito <debug.ito@gmail.com>
