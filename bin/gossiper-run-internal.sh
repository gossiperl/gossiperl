#!/bin/bash

# ---------------------------------------------------- #
# The purpose of this executable is to start gossiperl #
# from sources in the development environment.         #
# ---------------------------------------------------- #

SCRIPT_DIRECTORY=$(dirname "${BASH_SOURCE[0]}")
cd $SCRIPT_DIRECTORY/../
rebar clean get-deps

chmod +x $SCRIPT_DIRECTORY/patches/raspberrypi-erlsha2-patch.sh
$SCRIPT_DIRECTORY/patches/raspberrypi-erlsha2-patch.sh

rebar compile
erlc -o ./ebin/
erl \
  -pa ./deps/*/ebin \
  -pa ./ebin/ \
  -eval "gossiperl_run:start()."