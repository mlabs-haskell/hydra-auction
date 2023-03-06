#!/bin/sh -e

mkdir -p devnet/ipc
docker-compose down
./reset-devnet.sh
docker-compose up -d
echo -n 'Waiting for the node socket ..'
while ! [ -S devnet/ipc/node.socket ]
do
  echo -n "."
  sleep 0.1
done
echo '. done'
echo 'Setting correct node socket permissions'
sudo chown -Rv $(id -u):$(id -g) devnet/ipc

echo "Setup cardano-cli env to connect to started network"
export CARDANO_NODE_SOCKET_PATH=./devnet/ipc/node.socket