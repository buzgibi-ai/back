#!/bin/sh

sha=$(git rev-parse HEAD@{0})
branch=$(git rev-parse --abbrev-ref HEAD)

tag="${branch}_${sha}"

cat <<EOT >> .env
  DBUSER=sonny
  DATABASE=scaffold
  TAG=$tag
EOT

cp /ssl/buzgibi.crt nginx/ssl/buzgibi.crt
cp /ssl/buzgibi.key nginx/ssl/buzgibi.key

exec docker-compose up -d