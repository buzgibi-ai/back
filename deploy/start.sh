#!/bin/sh

git clone https://github.com/buzgibi-ai/front.git

sha_front=$(cd front && git log -n 1 --pretty=format:"%H")

sha_back=$(git log -n 1 --pretty=format:"%H")

echo 'back sha --> ' + $sha_back
echo 'front sha --> ' + $sha_front

rm -rf front

cat <<EOT >> .env
  DBUSER=sonny
  DATABASE=buzgibi
  BACK_TAG=master_${sha_back}
  FRONT_TAG=master_${sha_front}
EOT

cp ~/ssl/front/buzgibi.crt ./deploy/nginx/ssl/front/buzgibi.crt
cp ~/ssl/front/buzgibi.key ./deploy/nginx/ssl/front/buzgibi.key
cp ~/ssl/back/buzgibi.crt ./deploy/nginx/ssl/back/buzgibi.crt
cp ~/ssl/back/buzgibi.key ./deploy/nginx/ssl/back/buzgibi.key
cp ~/ssl/global.pass ./deploy/nginx/ssl/global.pass

exec docker-compose up -d