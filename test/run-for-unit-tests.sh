#!/bin/bash
SCRIPT_DIRECTORY=$(dirname "${BASH_SOURCE[0]}")
DIR=$SCRIPT_DIRECTORY/..
chmod +x $DIR/bin/gossiper-run-internal.sh

screen -dmS gossiper bash -c "$DIR/bin/gossiper-run-internal.sh"

HOST=127.0.0.1
PORT=8080
REST_USER=$(cat $DIR/priv/settings.json | jq '.rest_user.username' -r)
REST_PASS=$(cat $DIR/priv/settings.json | jq '.rest_user.password' -r)

echo " -> Using $REST_USER:$REST_PASS credentials for overlay actions."

STATUS=nil
while true; do
  STATUS=$(curl -i -k -u $REST_USER:$REST_PASS https://$HOST:$PORT/overlays | head -n 1)
  echo " ---> Waiting for gossiperl to come up..."
  sleep 2
  if [[ $STATUS =~ .*200\ OK*. ]]; then
    break
  fi
done

echo " -> Gossiperl is up. Ensuring an overlay..."

sleep 5

curl -i -k -u $REST_USER:$REST_PASS \
     -X POST \
     -H 'Contetnt-Type: application/json; charset=utf-8' \
     -d "{ \"ip\": \"0.0.0.0\",
           \"port\": 6666,
           \"rack_name\": \"dev_rack1\",
           \"racks\": { \"dev_rack1\": [\"127.0.0.1\"] },
           \"symmetric_key\": \"v3JElaRswYgxOt4b\" }" \
     https://$HOST:$PORT/overlays/gossiper_overlay_remote

/bin/sleep 1

echo " -> Overlay started. Overlays:"

curl -k -u $REST_USER:$REST_PASS https://$HOST:$PORT/overlays | jq '.'