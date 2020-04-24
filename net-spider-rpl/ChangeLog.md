# Revision history for net-spider-rpl

## 0.4.1.1  -- ?

* Confirm test with `greskell-1.1.0.0`

## 0.4.1.0  -- 2020-01-26

* Add `FromJSON` and `ToJSON` instances to the following
  types.
    * `DIONode`
    * `DIOLink`
    * `NeighborType`
    * `MergedDIOLink`
    * `DAONode`
    * `DAOLink`
    * `CombinedNode`
    * `CombinedLink`
    * `FindingType`
* Add `ToJSON` instance to `IPv6ID`.

## 0.4.0.1  -- 2019-12-29

* Confirm `ip-1.7.0` and `fast-logger-3.0.0`.

## 0.4.0.0  -- 2019-12-28

* [BREAKING CHANGE] Changes instances of `LinkAttributes` and
  `NodeAttributes` because their signature are changed in
  `net-spider-0.4.0.0`.
* This also leads to a bug fix. Before, it could not write or parse
  attribute values of 'Nothing'.
* Add `FromGraphSON` and `ToJSON` instances to `NeighborType`.


## 0.3.1.0  -- 2019-10-04

* Expose an internal module `NetSpider.RPL.IPv6`.
* Confirm test with `ip-1.5.1`, `ip-1.6.0`, `hashable-1.3.0.0`, `time-1.9.3`.

## 0.3.0.0  -- 2019-09-30

### ContikiNG module

* [BREAKING CHANGE] `parseStream`, `parserFoundNodeDIO` and
  `parserFoundNodeDAO` functions now require `MonadLogger` context.
* Add `parseFileHandleM` function.
* Now the parser functions can emit Warning logs when the parser fails
  in an unexpected context.


## 0.2.3.0  -- 2019-09-24

### ContikiNG module

* Add `parseStream`, `parserFoundNodeDIO`, `parserFoundNodeDAO` functions.


## 0.2.2.0  -- 2019-09-23

### RPL module

* document: change the link to net-spider-rpl-example into
  net-spider-rpl-cli.

### ContikiNG module

* Add `parseFileHandle` function.


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
