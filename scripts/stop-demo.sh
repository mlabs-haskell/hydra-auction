#!/usr/bin/env bash
set -eo pipefail

source ./scripts/setup-envs.sh

# `down` does not respect profiles:
# https://github.com/docker/compose/issues/8139
$COMPOSE_CMD stop hydra-node-1 hydra-node-2 hydra-node-3
$COMPOSE_CMD stop delegate-server-1 delegate-server-2 delegate-server-3
