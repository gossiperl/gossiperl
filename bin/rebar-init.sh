#!/bin/bash

SCRIPT_DIRECTORY=$(dirname "${BASH_SOURCE[0]}")
cd $SCRIPT_DIRECTORY
rm -Rf rebar-src
mkdir -p rebar-src
cd rebar-src
git clone git://github.com/rebar/rebar.git .
/bin/sleep 5
./bootstrap
echo -e "Rebar bootstrapped."
while [ -z "$(ls -la . | grep ' rebar$')" ]; do
  echo " -> rebar build not found yet"
  /bin/sleep 1
done
echo -e "Rebar build found"
cp rebar ../../rebar
cd ..
rm -Rf rebar-src
echo -e "Rebar installed"