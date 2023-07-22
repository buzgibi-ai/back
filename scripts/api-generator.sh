#!/bin/bash

file=$1
nm=$2
package=$3
dir=$4

abort()
{
    echo >&2 '
***************
*** ABORTED ***
***************
'
    echo "An error occurred. Exiting..." >&2
    exit 1
}

trap 'abort' 0

set -e

echo >&2 '
************
*** START *** 
************
'

openapi3-code-generator-exe \
  -f -o $dir/src/foreign/$nm \
  --package-name $package \
  --property-type-suffix $package \
  --convert-to-camel-case=true \
  --response-type-suffix $package \
  --generate-optional-empty-request-body=false $file && \
  python $dir/scripts/duplicate.py $dir/src/foreign/$nm/$package.cabal && \
  python $dir/scripts/duplicate.py $dir/src/foreign/$nm/src/OpenAPI.hs

trap : 0

echo >&2 '
************
*** DONE *** 
************
'