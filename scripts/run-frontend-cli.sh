#!/usr/bin/env bash -e
parent_path="$(realpath $(dirname $(realpath $0)))/.."
# FIXME: parametrize for testnet

# Is runned inside container to prevent `node.socket` access bug on MacOS
# See issue #180

if [[ -z "${RUN_CLI_IN_NIX}" ]]; then
docker run --network hydra-auction_hydra_net --rm -it \
 -v ${parent_path}/devnet/node.socket:/node.socket \
 -v ${parent_path}/example:/example \
 -v ${parent_path}/auction-state:/auction-state hydra-auction-cli:latest \
 "hydra-auction" ${@}
else
  # FIXME: broken due to different delegate hosts
  cabal run hydra-auction -- ${@}
fi
