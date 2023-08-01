#!/bin/sh

mute500="${MUTE_500:-True}"

env_file=$(realpath -s env.yaml)

echo 'launch server..'
. /home/nix/.nix-profile/etc/profile.d/nix.sh && \
  nix-shell deploy.nix \
    --log-format bar-with-logs \
    --verbose \
    --command \
    "$PWD/bin/buzgibi \
        --cfg_path deploy/config.yaml \
        --path_to_katip deploy \
        --path_to_jwk deploy/jwk \
        --print_cfg y \
        --env_path $env_file \
        --mute500 $mute500"