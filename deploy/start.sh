#!/bin/sh

rm .env

sha=$(git rev-parse HEAD@{$1})
branch=$(git rev-parse --abbrev-ref HEAD)

tag="${branch}_${sha}"

cat <<EOT >> .env
  DBUSER=sonny
  DATABASE=scaffold
  TAG=$tag
EOT

exec docker-compose up