#!/bin/sh

sha=$(git rev-parse HEAD@{0})
branch=$(git rev-parse --abbrev-ref HEAD)

tag="${branch}_${sha}"

cat <<EOT >> .env
  DBUSER=sonny
  DATABASE=scaffold
  TAG=$tag
EOT

exec docker-compose up -d