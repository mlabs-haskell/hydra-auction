#!/usr/bin/env bash

# Prepare a "devnet" directory holding credentials, a dummy topology and
# "up-to-date" genesis files. If the directory exists, it is wiped out.
set -e

ROOTDIR="."
TARGETDIR=$CLUSTER_WORKDIR

[ -d "$TARGETDIR" ] && { echo "Cleaning up directory $TARGETDIR" ; sudo rm -r $TARGETDIR ; }

cp -af "$ROOTDIR/data/devnet/" "$TARGETDIR"
cp "$ROOTDIR/data/protocol-parameters.json" "$TARGETDIR/"
cp -af "$ROOTDIR/data/credentials" "$TARGETDIR"

echo '{"Producers": []}' > "$TARGETDIR/topology.json"
sed -i.bk "s/\"startTime\": [0-9]*/\"startTime\": $(date +%s)/" "$TARGETDIR/genesis-byron.json" && rm -f "$TARGETDIR/genesis-byron.json.bk" && \
sed -i.bk "s/\"systemStart\": \".*\"/\"systemStart\": \"$(date -u +%FT%TZ)\"/" "$TARGETDIR/genesis-shelley.json" && rm -f "$TARGETDIR/genesis-shelley.json.bk"

find $TARGETDIR -type f -exec chmod 0400 {} \;

echo "Prepared devnet, you can start the cluster now"
