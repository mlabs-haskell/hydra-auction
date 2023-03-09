#!/bin/sh -e

echo "Stopping demo in case anything is already running"
./stop-demo.sh

echo "Prepare devnet files"
./reset-devnet.sh

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
./seed-devnet.sh

echo "Starting hydra nodes"
docker-compose --profile hydra-node up -d
