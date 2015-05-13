#!/bin/bash

SCRIPT_DIRECTORY=$(dirname "${BASH_SOURCE[0]}")
UNAME=$(uname -a)
if [[ $UNAME == *"raspberrypi"* ]]; then
        sed -i 's/-m32//' $SCRIPT_DIRECTORY/../../deps/erlsha2/rebar.config
        echo "-----------------------------------"
        echo "Raspberry Pi erlsha2 patch applied."
        cat $SCRIPT_DIRECTORY/../../deps/erlsha2/rebar.config
        echo "-----------------------------------"
fi