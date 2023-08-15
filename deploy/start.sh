#!/bin/bash

# gh_key, pg_pass, pg_master_pass, pg_admin email, pg_admin psss
declare -a keysmap

idx=0
while IFS= read -r line || [[ -n "$line" ]]; do
    keysmap[idx]=$line
    (( idx++ ))
done < "$1"

sha_front=$(curl -L \
  -H "Accept: application/vnd.github+json" \
  -H "Authorization: Bearer ${keysmap[0]}"\
  -H "X-GitHub-Api-Version: 2022-11-28" \
  https://api.github.com/repos/buzgibi-ai/front/commits/master \
  | jq -r '.sha')

sha_back=$(git log -n 1 --pretty=format:"%H")

echo "back sha --> $sha_back"
echo "front sha --> $sha_front"

cat <<EOT >> .env
  DBUSER=sonny
  DATABASE=buzgibi
  DBPASS=${keysmap[1]}
  DBPOSTGRESPASS=${keysmap[2]}
  BACK_TAG=master_${sha_back}
  FRONT_TAG=master_${sha_front}
  PGADMINEMAIL=${keysmap[3]}
  PGADMINPASS=${keysmap[4]}
EOT

cp ~/ssl/front/buzgibi.crt ./deploy/nginx/ssl/front/buzgibi.crt
cp ~/ssl/front/buzgibi.key ./deploy/nginx/ssl/front/buzgibi.key
cp ~/ssl/back/buzgibi.crt ./deploy/nginx/ssl/back/buzgibi.crt
cp ~/ssl/back/buzgibi.key ./deploy/nginx/ssl/back/buzgibi.key
cp ~/ssl/global.pass ./deploy/nginx/ssl/global.pass

exec docker-compose up -d