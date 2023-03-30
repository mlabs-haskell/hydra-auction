#!/bin/sh -e

docker-compose down
docker-compose --profile hydra-node down
docker-compose --profile delegate-server down
