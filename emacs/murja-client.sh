#!/bin/env bash

source_dir=$(dirname "${0}")
cookie_jar=$source_dir/cookies

if [ ! -d $source_dir/murja-client/ ]; then
    echo "murja-client doesn't exist in $(pwd)"
    sh $source_dir/generate-client.sh
fi

sh $source_dir/murja-client/client.sh --cookie-jar "$cookie_jar" -b "$cookie_jar" "$@"

