#!/usr/bin/env bash
set -eo pipefail

source ./scripts/setup-envs.sh

$COMPOSE_CMD --profile delegate-server down
$COMPOSE_CMD --profile hydra-node down
$COMPOSE_CMD down --remove-orphans
# Network could stay by CLIs and mess up with restarting
docker network rm --force hydra-auction_hydra_net
