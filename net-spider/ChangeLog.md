# Revision history for net-spider

## 0.4.0.0

* Use `greskell-1.0.0.0`.

### Graph module

* [BREAKING CHANGE] Change signatures of `parseNodeAttributes` and
  `parseLinkAttributes` functions, because now `PropertMapList` and
  `PropertyMapSingle` are deprecated.

* [BREAKING CHANGE] Now `EID` is alias for `ElementID`. It's kind is
  now `* -> *`.

* [BREAKING CHANGE] Kind of `VFoundNode` and `EFinds` is now `*` (was
  `* -> *`).

* [BREAKING CHANGE] Remove `NodeAttributes PropertyMapList` instance
  and `LinkAttributes PropertyMapSingle` instance, because
  PropertyMaps are deprecated.
  
* Add `NodeAttributes` and `LinkAttributes` instances to `PMap`.


* (are they public?) Add `VFoundNodeData` and `EFindsData` types.



## 0.3.3.0  -- 2019-10-13

### Interval module

* Add `secSince` and `secUntil` functions.

## 0.3.2.1  -- 2019-10-04

* Confirm test with `hashable-1.3.0.0`.

## 0.3.2.0  -- 2019-08-04

* Add `Interval` module.

### Query module

* Add `Eq` instance to `FoundNodePolicy`.

### Spider module

* Add `withSpider` function.

## 0.3.1.1  -- 2019-07-19

* Add documentation about `GraphML.Writer` module.

## 0.3.1.0  -- 2019-07-15

* Add `GraphML.Writer` module.

### Snapshot module

* Add `SnapshotGraph` type synonym.

### Timestamp module

* Add `toTime`, `toSystemTime` and `showTimestamp` functions.


## 0.3.0.0  -- 2019-05-03

* Export `Snapshot.Internal` module. This module is only for internal
  use.

* **[BREAKING CHANGE]** Use StrictData extension by default. This changes the strictness of the following data types.

    * `Query` (from Query module)
    * `Config` (from Spider.Config module)
    * `UnifyStdConfig` (from Unify module)

### Found module

* Add `Functor` and `Bifunctor` instances to `FoundNode` and
  `FoundLink.`

### Graph module

* Add `NodeAttributes` instance to `PropertyMapList`.
* Add `LinkAttributes` instance to `PropertyMapSingle`.

### Query module

* **[BREAKING CHANGE]** `Query` data type is now strict.

### Snapshot module

* Add `Functor` and `Bifunctor` instances to `SnapshotNode` and
  `SnapshotLink.`

## 0.2.0.0  -- 2018-12-10

* Add `Log` module.
* Confirmed test with base-4.12, time-1.9, containers-0.6

### Query module

* Add `timeInterval` and `foundNodePolicy` fields to `Query` type.
* Add `FoundNodePolicy` type.
* Add `secUpTo`, `policyOverwrite` and `policyAppend` functions.
* Re-export symbols from Data.Interval.

### Spider module

* Fix bug that the Spider visits a node multiple times. The result was
  correct, but it took extra time.
* Now it may produce log messages.

### Spider.Config module

* [BREAKING CHANGE] Now `Spider` type's internal is hidden. It was
  just a mistake.
* [BREAKING CHANGE] Now `getSnapshot` and `getSnapshotSimple` requires
  `(Show n)` constraint for logging.
* Add `logThreshold` field.
* Export `LogLevel` type.

### Timestamp module

* [BREAKING CHANGE] Rename `fromEpochSecond` to
  `fromEpochMillisecond`. Now `Timestamp` represents milliseconds
  since the epoch.
* Add `now`, `addSec`, `parseTimestamp`, `fromS`, `fromZonedTime`,
  `fromUTCTime`, `fromSystemTime`, `fromLocalTime`, `showEpochTime`.

### Unify modulem

* [BREAKING CHANGE] Now `LinkSampleUnifier` returns `WriterLoggingM`
  monad.
* [BREAKING CHANGE] Now `unifyToOne`, `unifyToMany` and `unifyStd`
  require `(Show n)` constraint.


## 0.1.0.0  -- 2018-09-24

* First version. Released on an unsuspecting world.
