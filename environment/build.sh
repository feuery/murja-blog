#!/usr/bin/env bash

set -euo pipefail

if [[ $PWD =~ environment ]] ;
then
    pushd ..
fi

pushd ./elm-frontti
elm make src/Main.elm --output murja.js --optimize
uglifyjs murja.js --compress 'pure_funcs="F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9",pure_getters,keep_fargs=false,unsafe_comps,unsafe' | uglifyjs --mangle > murja.min.js

popd
if [[ ! -f ./elm-frontti/murja.min.js ]]; then
    echo "Compiling frontend seems to have failed!" 1>&2
    exit -1
else
    echo "Compiling frontend seems to have succeeded!"
fi

mv ./elm-frontti/murja.min.js ./murja/resources/murja.min.js
echo "Frontend is moved to backend's resources directory"

pushd murja
lein uberjar
