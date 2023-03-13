#!/usr/bin/env bash

# Seed a "devnet" by distributing some Ada to commit and also some marked as
# "fuel" for the Hydra Head.
set -eo pipefail

MARKER_DATUM_HASH="a654fb60d21c1fed48db2c320aa6df9737ec0204c0ba53b9b94a09fb40e757f3"
SCRIPT_DIR="$(realpath $(dirname $(realpath $0)))/.."
NETWORK_ID=42

CCLI_CMD=
DEVNET_DIR=/devnet
if [[ -n ${1} ]]; then
    echo >&2 "Using provided cardano-cli command: ${1}"
    $(${1} version > /dev/null)
    CCLI_CMD=${1}
    DEVNET_DIR=${SCRIPT_DIR}/devnet
fi

HYDRA_NODE_CMD=
if [[ -n ${2} ]]; then
    echo >&2 "Using provided hydra-node command: ${2}"
    ${2} --version > /dev/null
    HYDRA_NODE_CMD=${2}
fi

# Invoke cardano-cli in running cardano-node container or via provided cardano-cli
function ccli() {
  ccli_ ${@} --testnet-magic ${NETWORK_ID}
}
function ccli_() {
  if [[ -x ${CCLI_CMD} ]]; then
      ${CCLI_CMD} ${@}
  else
      docker-compose exec cardano-node cardano-cli ${@}
  fi
}

# Invoke hydra-node in a container or via provided executable
function hnode() {
  if [[ -n ${HYDRA_NODE_CMD} ]]; then
      ${HYDRA_NODE_CMD} ${@}
  else
      docker run --rm -it \
        -v ${SCRIPT_DIR}/devnet:/devnet \
        ghcr.io/input-output-hk/hydra-node:0.8.1 -- ${@}
  fi
}

# Retrieve some lovelace from faucet, marked as "fuel" if requested
function seedFaucet() {
    ACTOR=${1}
    AMOUNT=${2}
    MARKED=${3:-"normal"}
    echo >&2 "Seeding a UTXO from faucet to ${ACTOR} with ${AMOUNT}Ł (${MARKED})"

    # Determine faucet address and just the **first** txin addressed to it
    FAUCET_ADDR=$(ccli address build --payment-verification-key-file ${DEVNET_DIR}/credentials/faucet.vk)
    FAUCET_TXIN=$(ccli query utxo --address ${FAUCET_ADDR} --out-file /dev/stdout | jq -r 'keys[0]')

    ACTOR_ADDR=$(ccli address build --payment-verification-key-file ${DEVNET_DIR}/credentials/${ACTOR}.vk)

    # Optionally mark output
    MARKER=""
    if [[ "${MARKED}" == "fuel" ]]; then
        MARKER="--tx-out-datum-hash ${MARKER_DATUM_HASH}"
    fi
    echo >&2 "building"
    ccli transaction build --babbage-era --cardano-mode \
        --change-address ${FAUCET_ADDR} \
        --tx-in ${FAUCET_TXIN} \
        --tx-out ${ACTOR_ADDR}+${AMOUNT} \
        ${MARKER} \
        --out-file ${DEVNET_DIR}/seed-${ACTOR}.draft >&2
    ccli transaction sign \
        --tx-body-file ${DEVNET_DIR}/seed-${ACTOR}.draft \
        --signing-key-file ${DEVNET_DIR}/credentials/faucet.sk \
        --out-file ${DEVNET_DIR}/seed-${ACTOR}.signed >&2
    SEED_TXID=$(ccli_ transaction txid --tx-file ${DEVNET_DIR}/seed-${ACTOR}.signed | tr -d '\r')
    SEED_TXIN="${SEED_TXID}#0"
    ccli transaction submit --tx-file ${DEVNET_DIR}/seed-${ACTOR}.signed >&2

    echo -n >&2 "Waiting for utxo ${SEED_TXIN}.."

    while [[ "$(ccli query utxo --tx-in "${SEED_TXIN}" --out-file /dev/stdout | jq ".\"${SEED_TXIN}\"")" = "null" ]]; do
        sleep 1
        echo -n >&2 "."
    done
    echo >&2 "Done"
}

function publishReferenceScripts() {
  echo >&2 "Publishing reference scripts ('νInitial' & 'νCommit')..."
  hnode publish-scripts \
    --network-id ${NETWORK_ID} \
    --node-socket ${DEVNET_DIR}/node.socket \
    --cardano-signing-key devnet/credentials/faucet.sk
}

seedFaucet "oscar" 1000000000 # 1000 Ada to commit
seedFaucet "oscar" 100000000 "fuel" # 100 Ada marked as "fuel"
seedFaucet "patricia" 500000000 # 500 Ada to commit
seedFaucet "patricia" 100000000 "fuel" # 100 Ada marked as "fuel"
seedFaucet "rupert" 250000000 # 250 Ada to commit
seedFaucet "rupert" 100000000 "fuel" # 100 Ada marked as "fuel"
echo "HYDRA_SCRIPTS_TX_ID=$(publishReferenceScripts)" > .env
echo >&2 "Environment variable stored in '.env'"
echo >&2 -e "\n\t$(cat .env)\n"
