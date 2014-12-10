#!/bin/bash
SCRIPT_DIRECTORY=$(dirname "${BASH_SOURCE[0]}")
cd $SCRIPT_DIRECTORY/../

echo ""
echo "Lines of code:"
echo "------------------------------------------------------------"
echo "Files:"
find . -not \( -path "*deps*" -prune \) -not \( -path "*rel*" -prune \) -name "*.erl" -o -name "*.hrl" -o -name "*.thrift"
echo "Count excluding comments:"
find . -not \( -path "*deps*" -prune \) -not \( -path "*rel*" -prune \) -name "*.erl" -o -name "*.hrl" -o -name "*.thrift" \
  | xargs cat \
  | sed 's/ //g' \
  | grep '^[^%]' \
  | wc -l