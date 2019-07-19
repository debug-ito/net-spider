# Revision history for net-spider-rpl

## 0.2.1.0  -- 2019-07-19

### DIO module

* Export `TrickleInterval` type synonym. It should have been exported
  in the first place.


## 0.2.0.0  -- 2019-07-15

* [BREAKING CHANGE] Type class instances from net-spider-pangraph are
  removed. This is because

    1. `NetSpider.GraphML.Writer` from net-spider can be used to
       format SnapshotGraphs into GraphML
    2. net-spider-pangraph has relatively a heavy dependency set
    3. `NetSpider.Pangraph.ToAttributes` typeclass instance can be
       easily implemented by `NetSpider.Pangraph.attributesFromGraphML`.

* Add `NetSpider.GraphML.ToAttributes` instance to RPL data types so
  that SnapshotGraphs can be formatted to GraphML.

### Combined module

* Add `SnapshotGraphCombined` type synonym.
* `combineNodes` now prefers the input node with the latest timestamp
  as the basis of the result.

### ContikiNG module

* Bug fix for DIO entries including the infinite rank.
* Add detailed doc to `parseFile`.

### DAO module

* Add `SnapshotGraphDAO` type synonym.
* Add `daoDefQuery` function.
* Fix writer and parser of `dao_route_num` attribute.

### DIO module

* Add `SnapshotGraphDIO` type synonym.
* Add `dioDefQuery` function.

### FindingID module

* Add `NetSpider.GraphML.Writer.ToNodeID` intance to `FindingID` and
  `IPv6ID`.


## 0.1.0.0  -- 2019-05-03

* First version. Released on an unsuspecting world.
