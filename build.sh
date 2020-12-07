#!/bin/env bash

set -euo pipefail

pushd ./elm-frontti
elm make src/Main.elm --output murja.js
popd
if [[ ! -f ./elm-frontti/murja.js ]]; then
    echo "Compiling frontend seems to have failed!" 1>&2
    exit -1
else
    echo "Compiling frontend seems to have succeeded!"
fi

mv ./elm-frontti/murja.js ./blog/resources/murja.js
echo "Frontend is moved to backend's resources directory"

pushd blog
lein uberjar
