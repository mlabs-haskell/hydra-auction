#!/usr/bin/env bash
set -eo pipefail

parent_path="$(realpath $(dirname $(realpath $0)))/.."

# Envs

CLUSTER_ENV=${CLUSTER_ENV:-devnet}

CARDANO_IMAGE="inputoutput/cardano-node:1.35.4"

if [ "${CLUSTER_ENV}" = "devnet" ];
then
    CONTESTATION_PERIOD="1"
    TESTNET_MAGIC="42"
    CLUSTER_WORKDIR="./devnet"
    COMPOSE_CMD="docker-compose -f docker-compose.yaml -f docker-compose.devnet.yaml"
else
    CONTESTATION_PERIOD="120"
    TESTNET_MAGIC="1"
    CLUSTER_WORKDIR="./testnet"
    COMPOSE_CMD="docker-compose -f docker-compose.yaml -f docker-compose.testnet.yaml"

fi;

CARDANO_NODE_SOCKET_PATH=${CLUSTER_WORKDIR}/node.socket

# Client actions

function ccli() {
  $COMPOSE_CMD exec cardano-node cardano-cli ${@} --testnet-magic ${TESTNET_MAGIC}
}

# Invoke hydra-node in a container or via provided executable
function hnode() {
    docker run --rm -it \
    -v ${CLUSTER_WORKDIR}:/devnet \
    hydra-node:latest -- ${@}
}

function frontend-cli() {
    # Is runned inside container to prevent `node.socket` access bug on MacOS
    # See issue #180

    local delegateNum="$1"
    shift
    local cmdArray=("$@")

    if [ "${RUN_CLI_IN_DOCKER:-0}" != "0" ]; then
        docker run --rm -it \
        -v ${CARDANO_NODE_SOCKET_PATH}:/devnet/node.socket \
        -v ${parent_path}/example:/example \
        -v ${parent_path}/auction-state:/auction-state hydra-auction-cli:latest \
        "hydra-auction" "${cmdArray[@]}" "-d" "delegate-server-${delegateNum}:8001"
    else
        cabal run hydra-auction -- "${cmdArray[@]}" "-d" "127.0.0.1:800${delegateNum}"
    fi;
}

CARDANO_PARAMS=("--cardano-node-socket" "$CARDANO_NODE_SOCKET_PATH" "--network-magic" "$TESTNET_MAGIC")

function frontend-cli-repl() {
    frontend-cli "${@}" "${CARDANO_PARAMS[@]}"
}

function frontend-cli-faucet-command(){
    frontend-cli-repl "1" "-a" "faucet" "-c" "$1"
}

function publishReferenceScripts() {
    echo >&2 "Publishing reference scripts ('νInitial' & 'νCommit')..."
    hnode publish-scripts \
        --testnet-magic "${TESTNET_MAGIC}" \
        --node-socket devnet/node.socket \
        --cardano-signing-key devnet/credentials/faucet.sk
}

ENVS="CONTESTATION_PERIOD=${CONTESTATION_PERIOD}
TESTNET_MAGIC=${TESTNET_MAGIC}
CLUSTER_WORKDIR=${CLUSTER_WORKDIR}
COMPOSE_CMD=${COMPOSE_CMD}
CARDANO_IMAGE=${CARDANO_IMAGE}"

echo "$ENVS" > .env
echo >&2 "Environment variables stored in '.env'"
