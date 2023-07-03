#!/bin/sh

commit=$1

sha=$(git rev-parse HEAD@{$commit})
branch=$(git rev-parse --abbrev-ref HEAD)

echo "go down along the commit tree to $commit commit(s)"
echo "sha ---> $sha"
echo "branch ---> $branch"

tag="${branch}_${sha}"

cat <<EOT >> .env
  DBUSER=sonny
  DATABASE=scaffold
  TAG=$tag
EOT

cp ~/ssl/buzgibi.crt ./deploy/nginx/ssl/buzgibi.crt
cp ~/ssl/buzgibi.key ./deploy/nginx/ssl/buzgibi.key

exec docker-compose up -d