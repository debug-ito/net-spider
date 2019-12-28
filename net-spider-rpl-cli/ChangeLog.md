# Revision history for net-spider-rpl-cli

## 0.1.2.1  -- 2019-12-28

* docker-compose.yaml: Upgrade to janusgraph:0.4.0.
* Require `net-spider-0.4.0.0`, `net-spider-rpl-0.4.0.0` and
  `greskell-1.0.0.0` to support janusgraph:0.4.0.

## 0.1.2.0  -- 2019-10-13

* Add `--duration` option, thanks to `net-spider-cli-0.2.0.0`.
* Enable `-threaded` to build the executable.

## 0.1.1.1  -- 2019-10-04

* Confirm test with `net-spider-rpl-0.3.0.0`, `ip-1.5.1` and `ip-1.6.0`.

## 0.1.1.0  -- 2019-09-23

* First release.
* Improve README.
* Add `--filter` option to `input` command.
* Add `--year` option to `input` command.
* Improve debug messages.
* Now `--starts-from` option is mandatory for `snapshot` command.
* Now at least one input file is mandatory for `input` command.


## 0.1.0.0  -- 2019-09-16

* Preliminary version. Not released to hackage. Just to test Docker Hub.
