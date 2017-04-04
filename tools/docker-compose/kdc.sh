#!/usr/bin/env bash

export kdc='docker-compose --file ./tools/docker-compose/secotrec-local.yml'

mkdir -p /tmp/secotrec-local-shared-temp
chmod -R 777 /tmp/secotrec-local-shared-temp

$kdc "$@"
