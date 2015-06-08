#!/bin/bash

SCRIPT_DIRECTORY=$(dirname "${BASH_SOURCE[0]}")
RELEASE=1.3.5

mkdir -p /var/log/gossiperl
mkdir -p /etc/gossiperl

if [ ! -f /etc/gossiperl/multicast ]; then
  echo "Enabling multicast"
  ifconfig eth1 multicast
  route add -net 224.0.0.0 netmask 224.0.0.0 eth1
  echo $(date) >> /etc/gossiperl/multicast
else
  echo "Multicast already enabled"
fi

apt-get update && apt-get install -y curl jq
dpkg -i $SCRIPT_DIRECTORY/releases/gossiperl-${RELEASE}_all.deb