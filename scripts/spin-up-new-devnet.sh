#!/usr/bin/env bash
parent_path=$( cd "$(dirname "${BASH_SOURCE[0]}")" ; pwd -P )
cd "$parent_path/.."

echo "Stopping demo in case anything is already running"
./scripts/stop-demo.sh

echo "Prepare devnet files"
./scripts/reset-devnet.sh

echo "Starting cardano node"
docker-compose up cardano-node -d

echo -n 'Waiting for the node socket ..'
while ! [ -S devnet/node.socket ]
do
  echo -n "."
  sleep 0.1
done
echo '. done'

echo "Setup cardano-cli env to connect to started network"
export CARDANO_NODE_SOCKET_PATH=./devnet/node.socket

echo "Seeding hydra node actors with fuel and publish reference scripts"
./scripts/seed-devnet.sh

echo "Starting hydra nodes"
docker-compose --profile hydra-node up -d

start_delegate_servers="${1:-1}";

if [ "$start_delegate_servers" = "1" ];
then
  echo "Starting delegate servers"
  docker-compose --profile delegate-server up -d
fi;

echo "Changing node.socket owner to current user"
sudo chown $USER:$USER ./devnet/node.socket
