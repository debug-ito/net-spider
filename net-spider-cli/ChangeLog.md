# Revision history for net-spider-cli

## 0.2.0.6  -- 2021-02-11

* Confirm test with `doctest-0.18`.

## 0.2.0.5  -- 2020-12-30

* Confirm test with `optparse-applicative-0.16.0.0`.

## 0.2.0.4  -- 2020-06-21

* Confirm test with `base-4.13` and `base-4.14`

## 0.2.0.3  -- 2020-06-06

* Confirm test with `doctest-0.17`

## 0.2.0.2  -- 2020-05-30

* Confirm test with `aeson-1.5.0.0`.

## 0.2.0.1  -- 2019-12-28

* Allow `net-spider-0.4.0.0`.

## 0.2.0.0  -- 2019-10-13

* Add `--duration` option to `parserSnapshotQuery` function.
* [BREAKING CHANGE] `parserSnapshotQuery` function now returns a new type `CLISnapshotQuery`.
  This is because some combinations of `--time-from`, `--time-to` and `--duration` options
  are invalid. To express that case, users have to use `makeSnapshotQuery` function to get
  a `Query` object, and that function can fail.
* [BREAKING CHANGE] Remove `basisSnapshotQuery` field from `SnapshotConfig`.
  `makeSnapshotQuery` function now plays the same role.

## 0.1.0.2  -- 2019-10-04

* Confirm test with `hashable-1.3.0.0`.

## 0.1.0.1  -- 2019-08-04

* Fix version bound for net-spider. It requires net-spider-0.3.2.0,
  because this package uses `NetSpider.Interval`.

## 0.1.0.0  -- 2019-08-04

* First version. Released on an unsuspecting world.
