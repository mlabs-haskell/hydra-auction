#!/usr/bin/bash
parent_path=$( cd "$(dirname "${BASH_SOURCE[0]}")" ; pwd -P )
cd "$parent_path/.."

echo "Current CLUSTER_ENV is $CLUSTER_ENV"
echo "Change this env to devnet|testnet to change cluster network"

echo "Stopping demo in case anything is already running"
./scripts/stop-demo.sh

source ./scripts/setup-envs.sh

echo "Prepare devnet files"
source ./scripts/reset-devnet.sh

echo "Starting cardano node"
$COMPOSE_CMD up -d

export CARDANO_NODE_SOCKET_PATH="${CLUSTER_WORKDIR}/node.socket"

echo -n 'Waiting for the node socket ..'
while ! [ -S $CARDANO_NODE_SOCKET_PATH ]
do
  echo -n "."
  sleep 0.1
done
echo '. done'

echo "Changing node.socket owner to current user"
sudo chown $USER:$USER "$CARDANO_NODE_SOCKET_PATH"

echo 'Waiting for cardano node sync ..'

SYNC_PROGRESS=""
while ! [ "$SYNC_PROGRESS" = '"100.00"' ]
do
  SYNC_PROGRESS="$(cardano-cli query tip --testnet-magic $TESTNET_MAGIC | jq '.syncProgress')"
  echo "Sync progress is:" "$SYNC_PROGRESS"
  sleep 2
done
echo '. done'

echo "Seeding hydra node actors with fuel and publish reference scripts"

# We use custom Hydra version, so always publish
echo "HYDRA_SCRIPTS_TX_ID=$(publishReferenceScripts)" >> .env

echo "Faucet address is:"
frontend-cli-faucet-command "show-address"

frontend-cli-faucet-command "transfer-ada -a oscar -m normal -b 100"
frontend-cli-faucet-command "transfer-ada -a patricia -m normal -b 100"
frontend-cli-faucet-command "transfer-ada -a rupert -m normal -b 100"

frontend-cli-faucet-command "transfer-ada -a oscar -m fuel -b 100"
frontend-cli-faucet-command "transfer-ada -a patricia -m fuel -b 100"
frontend-cli-faucet-command "transfer-ada -a rupert -m fuel -b 100"

echo "Starting hydra nodes"
$COMPOSE_CMD --profile hydra-node up -d

start_delegate_servers="${1:-1}";

if [ "$start_delegate_servers" = "1" ];
then
  echo "Starting delegate servers"
  $COMPOSE_CMD --profile delegate-server up -d
fi;
