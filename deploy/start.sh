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
  DATABASE=buzgibi
  TAG=$tag
EOT

cp ~/ssl/front/buzgibi.crt ./deploy/nginx/ssl/front/buzgibi.crt
cp ~/ssl/front/buzgibi.key ./deploy/nginx/ssl/front/buzgibi.key
cp ~/ssl/back/buzgibi.crt ./deploy/nginx/ssl/back/buzgibi.crt
cp ~/ssl/back/buzgibi.key ./deploy/nginx/ssl/back/buzgibi.key
cp ~/ssl/global.pass ./deploy/nginx/ssl/global.pass

exec docker-compose up -d