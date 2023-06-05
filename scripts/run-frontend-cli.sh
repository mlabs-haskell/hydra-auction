#!/usr/bin/env bash

# FIXME: parametrize for testnet

# Is runned inside container to prevent `node.socket` access bug on MacOS
# See issue #180

if [[ -z "${RUN_CLI_IN_NIX}" ]]; then
docker run --network hydra-auction_hydra_net --rm -it \
 -v ./devnet/node.socket:/node.socket \
 -v ./example:/example \
 -v ./auction-state:/auction-state hydra-auction-cli:latest \
 "hydra-auction" ${@}
else
  # FIXME: broken due to different delegate hosts
  cabal run hydra-auction -- ${@}
fi
