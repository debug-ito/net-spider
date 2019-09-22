# net-spider-rpl-cli

An executable of [net-spider](https://hackage.haskell.org/package/net-spider) specialized for analyzing [RPL](https://tools.ietf.org/html/rfc6550) networks.

## Synopsis

Here is an example of using the Docker images of this package. You have to install [docker](https://docs.docker.com/install/) and [docker-compose](https://docs.docker.com/compose/) first.

1. Start the graph database backend (i.e. Gremlin Server).

        $ docker-compose up -d

2. Clear the entire database.

        $ ./run-net-spider.sh clear

3. Input logs about RPL networks.

        $ ./run-net-spider.sh input < syslog

4. Get a snapshot graph of the RPL network in GraphML format.

        $ ./run-net-spider.sh snapshot --time-to '2019-09-20T10:43:08' --starts-from 'fd00::212:eeaa:0077:2f9c' > snapshot.graphml


## Description

net-spider-rpl-cli is a tool to analyze evolution of an RPL network. It collects "local findings" about the network topology from RPL nodes, and stores those data into a graph database. Then you can query the database to get a "snapshot graph", a network topology at the specified time.

For general description about net-spider and its terminology, see [net-spider README](https://github.com/debug-ito/net-spider).

For details on command-line options, run the tool with `--help` option.

## Build and install

net-spider-rpl-cli is a regular Haskell package with an executable. You can install it from [hackage repository](https://hackage.haskell.org/package/net-spider-rpl-cli).

Or, you can just use [the pre-built Docker image](#Docker-image).

## Graph database

net-spider-rpl-cli stores time-varying topology of an RPL network into a graph dabase via the [Gremlin Server](http://tinkerpop.apache.org/). You have to set up the server, and pass the server's hostname and port to `--host` and `--port` options, respectively.

## Commands

net-spider-rpl-cli has the folowing sub-commands.

### clear command

`clear` command clears the entire database. Use with care.

### input command

`input` command reads log files, extracts information about the RPL network topology with timestamps and stores them into the database.

Currently, the format of the log files must be **logs of rpl-lite module of [Contiki-NG](https://github.com/contiki-ng/contiki-ng) operating system, prefixed with the standard syslog header.** See [this test file](https://github.com/debug-ito/net-spider/blob/master/net-spider-rpl/test/data/syslog_root.log) for an example.

There are two kinds of RPL network topology: the DIO (upward) graph and the DAO (downward) graph. `input` command extracts and stores both kinds of information.

### snapshot command

`snapshot` command queries the database to make a "snapshot graph", an RPL network topology observed at the specified time. You can specify a time range with `--time-from` and `--time-to` options, and then it makes a snapshot graph using only those input information observed during the time range.

To run this command, you have to specify `--starts-from` option at least one time. This is the IPv6 address of the node from which it searches for a snapshot graph. Usually this should be the address of the DODAG root.

It prints the obtained snapshot graph to STDOUT in [GraphML](http://graphml.graphdrawing.org/) format. The graph includes both DIO and DAO graphs, but you can distinguish them by `link_type` attribute of edges.

### cis command

`cis` command is a short for "clear + input + snapshot". It clears the database, inputs the specified log files and returns a snapshot graph.

With this command, you don't have to specify `--starts-from` option (it's ignored.) Instead, all IPv6 addresses observed in the input log files are searched.


## Docker image

We provide pre-built Docker images of net-spider-rpl-cli and the graph database.

- The Docker image of the graph database is specified in `docker-compose.yml` in this directory.

    - You can start it with `docker-compose up` command.
    - The "db" directory stores persistent graph data.

- The Docker image of net-spider-rpl-cli is available at Docker Hub as [debugito/net-spider-rpl-cli](https://hub.docker.com/r/debugito/net-spider-rpl-cli).
- To run the net-spider-rpl-cli image with the graph database image, use "run-net-spider.sh" script in this directory.


## Author

Toshio Ito <debug.ito@gmail.com>
