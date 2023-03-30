#!/bin/sh -e

docker-compose --profile delegate-server down
docker-compose --profile hydra-node down
docker-compose down
