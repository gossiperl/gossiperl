#!/bin/bash
mkdir -p .erlang
curl -O https://raw.githubusercontent.com/yrashk/kerl/master/kerl
chmod +x kerl
./kerl build 17.0 17.0
./kerl install 17.0 .erlang/17.0
. ./.erlang/17.0/activate