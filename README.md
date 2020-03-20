# net-spider

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

Maintaining the graph database is delegated to [Tinkerpop Gremlin Server](http://tinkerpop.apache.org/). You have to set up a server instance separately.

Alternatively, net-spider provides a way to build a small-scale snapshot graph without using a graph database. See [The Weaver type](#the-weaver-type-to-build-a-snapshot-graph-on-memory) section for detail.

## Contents

* [Use case](#use-case)
* [Basic usage](#basic-usage)
* [The Spider type](#the-spider-type)
* [The Weaver type to build a snapshot graph on memory](#the-weaver-type-to-build-a-snapshot-graph-on-memory)
* [Node and link attributes](#node-and-link-attributes)
* [Snapshot graph for a specific time interval](#snapshot-graph-for-a-specific-time-interval)
* [Multiple links between a pair of nodes](#multiple-links-between-a-pair-of-nodes)
* [Merge local findings by end nodes of a link](#merge-local-findings-by-end-nodes-of-a-link)
* [Note on graph database servers](#note-on-graph-database-servers)
* [See also](#see-also)


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
import qualified Data.Text.Lazy.IO as TLIO
import NetSpider.Pair (Pair(..))
import NetSpider.Spider
  (Spider, connectWS, close, addFoundNode, clearAll, getSnapshotSimple)
import NetSpider.Found
  (FoundNode(..), FoundLink(..), LinkState(LinkBidirectional))
import NetSpider.GraphML.Writer (writeGraphML)
import NetSpider.Timestamp (fromS)
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
                   foundAt = fromS "2018-08-20T12:53:38",
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
  clearAll spider -- Delete all data from the database for testing
  addFoundNode spider finding1
```

A local finding is expressed as `FoundNode` type. In the above example, we input a local finding observed at the switch named "switch1". `FoundNode` includes the timestamp (`foundAt`) at which the local finding was observed, and list of neighbors (`neighborLinks`) adjacent to this node. These are what we would get via SNMP + CDP/LLDP.

OK, let's observe the switch2 and input that local finding as well.

```haskell basic
  let finding2 = FoundNode
                 { subjectNode = "switch2",
                   foundAt = fromS "2018-08-20T13:00:22",
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

The above graph can be obtained by `getSnapshotSimple` function. This function traverses the history graph from "switch1" node, and retrieves the snapshot graph that is supposed to be the latest state of the network.

```haskell basic
  got_graph <- getSnapshotSimple spider "switch1"
  let (raw_nodes, raw_links) = got_graph
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
  map nodeTimestamp nodes `shouldBe` [ Just $ fromS "2018-08-20T12:53:38",
                                       Just $ fromS "2018-08-20T13:00:22",
                                       Nothing,
                                       Nothing
                                     ]
  map linkNodePair links `shouldBe` map Pair [ ("switch1", "switch2"),
                                               ("switch1", "switch3"),
                                               ("switch2", "switch4")
                                             ]
  map linkTimestamp links `shouldBe` [ fromS "2018-08-20T13:00:22",
                                       fromS "2018-08-20T12:53:38",
                                       fromS "2018-08-20T13:00:22"
                                     ]
```

With [NetSpider.GraphML.Writer module](http://hackage.haskell.org/package/net-spider/docs/NetSpider-GraphML-Writer.html), you can format the snapshot graph into [GraphML](http://graphml.graphdrawing.org/).

```haskell basic
  TLIO.putStrLn $ writeGraphML got_graph
```

GraphML is a popular file format for attribute graphs. Graph visualizer/manipulator software, such as [Cytoscape](https://cytoscape.org/) and [Gephi](https://gephi.org/), can read it.


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

## The Weaver type to build a snapshot graph on memory

Similar to `Spider` type, net-spider has `Weaver` 


## Node and link attributes

Nodes and links in your history graph can have time-varying attributes. You can store those attributes in the history graph. In the snapshot graph, you can basically get the latest values of those attributes.

For example, let's monitor the total number of packets that the switch has transmitted and received. First, we define the data type for the node attributes.

```haskell attrs
import Data.Greskell (Key, lookupAs, pMapToFail)
import Data.Greskell.Extra (writeKeyValues, (<=:>))

import NetSpider.Spider
  (Spider, connectWS, close, clearAll, addFoundNode, getSnapshotSimple)
import NetSpider.Found (FoundNode(..), FoundLink(..))
import NetSpider.Graph (NodeAttributes(..), VFoundNode)
import NetSpider.Timestamp (fromS)
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

keyTxCount :: Key VFoundNode Int
keyTxCount = "tx_count"

keyRxCount :: Key VFoundNode Int
keyRxCount = "rx_count"

instance NodeAttributes PacketCount where
  writeNodeAttributes packet_count =
    fmap writeKeyValues $ sequence $
    [ keyTxCount <=:> transmitCount packet_count,
      keyRxCount <=:> receiveCount packet_count
    ]
  parseNodeAttributes props =
    pMapToFail (PacketCount <$> lookupAs keyTxCount props <*> lookupAs keyRxCount props)
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
                   foundAt = fromS "2018-09-09T12:40:58",
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
  map nodeTimestamp raw_nodes1 `shouldBe` [Just $ fromS "2018-09-09T12:40:58"]
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
                   foundAt = fromS "2018-09-11T12:39:03",
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
  map nodeTimestamp raw_nodes2 `shouldBe` [Just $ fromS "2018-09-11T12:39:03"]
  map Sn.nodeAttributes raw_nodes2 `shouldBe`
    [ Just PacketCount { transmitCount = 20112,
                         receiveCount = 28544
                       }
    ]
```

Just like `FoundNode` has `nodeAttributes` field, `FoundLink` has `linkAttributes` field. To store your data type as link attributes, make that data type an instance of `LinkAttributes` class.

## Snapshot graph for a specific time interval

In the above examples, net-spider creates a snapshot graph that expresses the latest state of the network. You can also get a snapshot graph at a specific time in past by specifying the time interval of your query.

```haskell time-interval
import NetSpider.Spider
  (Spider, connectWS, close, clearAll, addFoundNode, getSnapshot)
import NetSpider.Found (FoundNode(..), FoundLink(..), LinkState(LinkBidirectional))
import NetSpider.Query (defQuery, timeInterval, Extended(NegInf, Finite), (<=..<=))
import NetSpider.Snapshot (linkNodeTuple)
import NetSpider.Timestamp (fromS)

main = hspec $ specify "node attributes" $ do
  (host, port) <- needEnvHostPort Need "NET_SPIDER_TEST"
  bracket (connectWS host port) close $ doWithSpider

doWithSpider :: Spider Text () () -> IO ()
doWithSpider spider = do
  clearAll spider
  let finding1 = FoundNode
                 { subjectNode = "switch1",
                   foundAt = fromS "2018-11-30T00:16:40",
                   nodeAttributes = (),
                   neighborLinks = [link2]
                 }
      finding2 = FoundNode
                 { subjectNode = "switch1",
                   foundAt = fromS "2018-11-30T00:17:00",
                   nodeAttributes = (),
                   neighborLinks = [link2, link3]
                 }
      link2 = FoundLink
              { targetNode = "switch2",
                linkState = LinkBidirectional,
                linkAttributes = ()
              }
      link3 = FoundLink
              { targetNode = "switch3",
                linkState = LinkBidirectional,
                linkAttributes = ()
              }
  addFoundNode spider finding1
  addFoundNode spider finding2
```

In the above example, "switch1" had only one link to "switch2" at first, but 20 seconds later it also got a link to "switch3".

To get the snapshot graph of the old state of the network, use `getSnapshot` function instead of `getSnapshotSimple`. `getSnapshot` function takes a `Query` object, in which you can specify the time interval for the history graph.


```haskell time-interval
  let query = (defQuery ["switch1"]) { timeInterval = time_interval }
      time_interval = NegInf <=..<= Finite (fromS "2018-11-30T00:16:50")
  (_, raw_links) <- getSnapshot spider query
```

Above, we make a query for a time interval of (-∞ <= t <= 2018-11-30T00:16:50). This exludes the local finding that observes a link to "switch3".

```haskell time-interval
  map linkNodeTuple raw_links `shouldBe` [("switch1", "switch2")]
```

By limiting the upper bound of the time interval of the query, you can get a snapshot graph in past.


## Multiple links between a pair of nodes

By default, net-spider assumes there is at most one link between a pair of nodes. If it is possible in your application that there are more than one links between a pair of nodes, you have to tell `Spider` how to distinguish those links.

For example, if you use link aggregation, you often connect a pair of switches with more than one physical links. Those physical links can be distinguished by the port names of the switches.

So, let's define the data type for port names first.

```haskell multi-link
import Data.Greskell (lookupAs, Key, pMapToFail)
import Data.Greskell.Extra (writeKeyValues, (<=:>))

import NetSpider.Found (FoundNode(..), FoundLink(..), LinkState(..))
import NetSpider.Graph (LinkAttributes(..), EFinds)
import NetSpider.Pair (Pair(..))
import NetSpider.Query (defQuery, unifyLinkSamples)
import NetSpider.Snapshot (linkNodeTuple)
import qualified NetSpider.Snapshot as Sn
import NetSpider.Spider
  (Spider, connectWS, close, clearAll, addFoundNode, getSnapshot)
import NetSpider.Timestamp (fromS)
import NetSpider.Unify (LinkSample(..), unifyStd, defUnifyStdConfig, makeLinkSubId)

data Ports =
  Ports
  { subjectPort :: Text,
    targetPort :: Text
  }
  deriving (Show,Eq,Ord)

keySPort :: Key EFinds Text
keySPort = "sport"

keyTPort :: Key EFinds Text
keyTPort = "tport"

instance LinkAttributes Ports where
  writeLinkAttributes ports =
    fmap writeKeyValues $ sequence $
    [ keySPort <=:> subjectPort ports,
      keyTPort <=:> targetPort ports
    ]
  parseLinkAttributes props =
    pMapToFail (Ports <$> lookupAs keySPort props <*> lookupAs keyTPort props)
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
                   foundAt = fromS "2018-09-13T12:43:10",
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

To get the correct snapshot graph, you have to tell `Spider` to distinguish the links by the port names as well as the switch names. To do that, use `unifyLinkSamples` field in the `Query` object.

```haskell multi-link
  let linkSubIdWithPorts :: LinkSample Text Ports -> Pair (Text,Text)
      linkSubIdWithPorts ls = Pair ( (lsSubjectNode ls, subjectPort $ lsLinkAttributes ls),
                                     (lsTargetNode ls, targetPort $ lsLinkAttributes ls)
                                   )
      unifier = unifyStd $ defUnifyStdConfig { makeLinkSubId = linkSubIdWithPorts }
      query = (defQuery ["switch1"]) { unifyLinkSamples = unifier }
  (_, raw_links) <- getSnapshot spider query
```

The `linkSubIdWithPorts` function above defines the link sub-ID, an identifier for a link within the given pair of switches. Here we include the `subjectPort` and `targetPort` in the link sub-ID. We also use the switch names and `Pair` type (from `NetSpider.Pair`) because the link's subject and target may be swapped. `Pair` type's `Eq` and `Ord` instances are immune to swapping.

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
import Data.Greskell (newBind, gProperty, lookupAs, Key, pMapToFail)
import Data.Scientific (Scientific)

import NetSpider.Found (FoundNode(..), FoundLink(..), LinkState(..))
import NetSpider.Graph (LinkAttributes(..), EFinds)
import NetSpider.Query (defQuery, unifyLinkSamples)
import NetSpider.Snapshot
  (SnapshotLink, sourceNode, destinationNode, linkTimestamp, linkNodeTuple)
import qualified NetSpider.Snapshot as Sn
import NetSpider.Spider
  (Spider, connectWS, close, clearAll, addFoundNode, getSnapshot)
import NetSpider.Timestamp (fromS)
import NetSpider.Unify
  ( LinkSample(lsLinkAttributes),
    unifyStd, defUnifyStdConfig, mergeSamples, latestLinkSample,
    negatesLinkSample, defNegatesLinkSample
  )

-- | Received signal strength (dBm)
newtype RxSignal = RxSignal Scientific
                   deriving (Show,Eq,Ord)

instance LinkAttributes RxSignal where
  writeLinkAttributes (RxSignal s) = do
    s_var <- newBind s
    return $ gProperty "rx_signal" s_var
  parseLinkAttributes props =
    pMapToFail $ RxSignal <$> lookupAs ("rx_signal" :: Key EFinds Scientific) props
```

Then, put local findings for a link.

```haskell merge-link-attrs
main :: IO ()
main = hspec $ specify "merge link attributes" $ do
  (host, port) <- needEnvHostPort Need "NET_SPIDER_TEST"
  bracket (connectWS host port) close $ \spider -> do
    inputFindings spider
    inspectSnapshot spider

inputFindings :: Spider Text () RxSignal -> IO ()
inputFindings spider = do
  clearAll spider
  let finding1 = FoundNode
                 { subjectNode = "switch1",
                   foundAt = fromS "2018-09-17T12:57:50",
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
                   foundAt = fromS "2018-09-17T13:03:08",
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

The switch1 and switch2 observe their own signal strength, but both of them are on the same link. So, it's natural for the data model of a link to have BOTH of the signal strength values. Let's define another link attribute type that has two signal strengths.

```haskell merge-link-attrs
-- | Received signal strengths observed at ends of a link.
data SignalStrengths =
  SignalStrengths
  { atSource :: Maybe RxSignal,
    atDestination :: Maybe RxSignal
  }
  deriving (Show,Eq,Ord)
```

To merge the two `RxSignal`s into one `SignalStrengths`, you have to tell `Spider` how to merge them. To do that, define the merger function and pass it to the query to `getSnapshot`.

The merger function is like this:

```haskell merge-link-attrs
merger :: [LinkSample Text RxSignal]
       -> [LinkSample Text RxSignal]
       -> Maybe (LinkSample Text SignalStrengths)
merger llinks rlinks = do
  let llink = latestLinkSample llinks
      rlink = latestLinkSample rlinks
      lsignal = fmap lsLinkAttributes llink
      rsignal = fmap lsLinkAttributes rlink
  base_link <- latestLinkSample (llinks ++ rlinks)
  let ss = if Just base_link == llink
           then SignalStrengths lsignal rsignal
           else SignalStrengths rsignal lsignal
  return $ base_link { lsLinkAttributes = ss }
```

The above merger function takes link samples obtained from the history graph. The `llinks` is the link samples observed by one of the end nodes, whereas the `rlinks` is the ones observed by the other end node. It constructs the `SignalStrengths` attribute from the attributes from `llinks` and `rlinks`. Note that the subject node and target node of `llink` are swap of those of `rlink`. That's why we take care when we construct a `SignalStrengths` above.

Finally let's make a query.

```haskell merge-link-attrs
-- | a support getter
sourceNodeRxSignal :: SnapshotLink Text SignalStrengths -> (Text, Maybe RxSignal)
sourceNodeRxSignal l = (sourceNode l, atSource $ Sn.linkAttributes l)

-- | a support getter
destNodeRxSignal :: SnapshotLink Text SignalStrengths -> (Text, Maybe RxSignal)
destNodeRxSignal l = (destinationNode l, atDestination $ Sn.linkAttributes l)

inspectSnapshot :: Spider Text () RxSignal -> IO ()
inspectSnapshot spider = do
  let unify_conf = defUnifyStdConfig { mergeSamples = merger,
                                       negatesLinkSample = defNegatesLinkSample
                                     }
      unifier = unifyStd unify_conf
      query = (defQuery ["switch1"]) { unifyLinkSamples = unifier }
  (_, raw_links) <- getSnapshot spider query
  length raw_links `shouldBe` 1
  let [got_link] = raw_links
  linkNodeTuple got_link `shouldBe` ("switch2", "switch1")
  linkTimestamp got_link `shouldBe` fromS "2018-09-17T13:03:08"
  [sourceNodeRxSignal got_link, destNodeRxSignal got_link] `shouldMatchList`
    [("switch1", Just $ RxSignal (-4.3)), ("switch2", Just $ RxSignal (-5.5))]
```

The `merger` function passed to `getSnapshot` via `unifyStd`. Note that you need to set `negatesLinkSample` field in `UnifyStdConfig`, because setting `merger` to it is a polymorphic update. The result `SnapshotLink` has `SignalStrengths` type as its link attributes, which contains the signal strengths observed at both of the end nodes.

## Note on graph database servers

Sometimes you have to take care of configurations specific to the graph database server you use.

- [Configuration of JanusGraph](https://github.com/debug-ito/net-spider/wiki/Configuration-of-JanusGraph): Configuration guide for JanusGraph.


## See also

- [net-spider](http://hackage.haskell.org/package/net-spider): net-spider core package.
- [net-spider-cli](http://hackage.haskell.org/package/net-spider-cli): Composable CLI option parsers for NetSpider objects based on [optparse-applicative](http://hackage.haskell.org/package/optparse-applicative).
- [net-spider-rpl](http://hackage.haskell.org/package/net-spider-rpl): net-spider data model for RPL ([RFC 6550 - RPL: IPv6 Routing Protocol for Low-Power and Lossy Networks](https://tools.ietf.org/html/rfc6550))
- [net-spider-pangraph](http://hackage.haskell.org/package/net-spider-pangraph): Conversion between net-spider and [pangraph](https://hackage.haskell.org/package/pangraph).
- [net-spider-rpl-cli](https://github.com/debug-ito/net-spider/tree/master/net-spider-rpl-cli/): A working example of using net-spider. It uses net-spider-rpl and net-spider-cli.


## Author

Toshio Ito <debug.ito@gmail.com>
