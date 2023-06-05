#!/bin/sh -e

docker-compose --profile delegate-server down
docker-compose --profile hydra-node down
docker-compose down --remove-orphans
# Network could stay by CLIs and mess up with restarting
docker network rm --force hydra-auction_hydra_net
