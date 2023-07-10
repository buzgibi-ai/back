#!/bin/sh

sha_front=$(curl -H "Accept: application/vnd.github+json" \
  -H "Authorization: Bearer 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"\
  -H "X-GitHub-Api-Version: 2022-11-28" \
  https://api.github.com/repos/buzgibi-ai/front/commits/master | jq -r '.sha')

sha_back=$(git log -n 1 --pretty=format:"%H")

echo 'back sha --> ' + $sha_back
echo 'front sha --> ' + $sha_front

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