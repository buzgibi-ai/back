#!/bin/sh

dir="$(dirname "$0")"

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

front_commit=$(curl "https://api.github.com/repos/buzgibi-ai/front/commits/master?path=filename&page=1&per_page=1" \
     -H "Authorization: token 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" \
     -H "Accept: application/vnd.github+json" \
     -v | jq -r '.sha')


css_commit=$(curl "https://api.github.com/repos/buzgibi-ai/front-css/commits/master?path=filename&page=1&per_page=1" \
     -H "Authorization: token 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" \
     -H "Accept: application/vnd.github+json" \
     -v | jq -r '.sha')

cat <<EOT >> commits.yaml
  front: $front_commit
  css: $css_commit
EOT

trap : 0

echo >&2 '
************
*** DONE *** 
************
'