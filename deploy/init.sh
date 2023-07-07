#!/bin/sh

if [[ -z "${MUTE_500}" ]]; 
then mute500="${MUTE_500}"
else mute500='True'
fi

echo 'launch server..'
. /home/nix/.nix-profile/etc/profile.d/nix.sh && \
  nix-shell deploy.nix \
    --log-format bar-with-logs \
    --verbose \
    --command \
    "$PWD/bin/server \
        --cfg_path deploy/config.yaml \
        --path_to_katip deploy \
        --path_to_jwk deploy \
        --print_cfg y \
        --env_path $PWD/env.yaml \
        --mute500 $mute500"