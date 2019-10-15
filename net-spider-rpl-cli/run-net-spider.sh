#!/bin/bash

this_dir=`dirname "$0"`
cd "$this_dir"

image_name="debugito/net-spider-rpl-cli:0.1.2.0"

if ! ( which docker-compose > /dev/null ); then
    echo "docker-compose is not installed."
    exit 1
fi

db_service=`docker-compose ps -q db`
if [ "x$db_service" = "x" ]; then
    echo "JanusGraph DB service is not running. Run docker-compose up -d, and wait for the DB to open the port."
    exit 1
fi

docker run --rm --name net-spider --network netspiderrplcli_db-net -a stdin -a stdout -a stderr -i "$image_name" --host db $@
