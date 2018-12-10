# Revision history for net-spider

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
