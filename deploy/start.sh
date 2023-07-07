#!/bin/sh

sha=$(git log -n 1 --pretty=format:"%H")

cat <<EOT >> .env
  DBUSER=sonny
  DATABASE=buzgibi
  TAG=master_${sha}
EOT

cp ~/ssl/front/buzgibi.crt ./deploy/nginx/ssl/front/buzgibi.crt
cp ~/ssl/front/buzgibi.key ./deploy/nginx/ssl/front/buzgibi.key
cp ~/ssl/back/buzgibi.crt ./deploy/nginx/ssl/back/buzgibi.crt
cp ~/ssl/back/buzgibi.key ./deploy/nginx/ssl/back/buzgibi.key
cp ~/ssl/global.pass ./deploy/nginx/ssl/global.pass

exec docker-compose up -d