#!/bin/bash

this_dir=`dirname "$0"`
cd "$this_dir"

image_name="1a1475122240"

docker-compose up -d && \
    docker run --rm --name net-spider --network netspiderrplcli_db-net -a stdin -a stdout -a stderr "$image_name" --host db $@
