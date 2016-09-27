#!/bin/bash

set -e

ASSETS=./assets/public/js
mkdir -p $ASSETS

stack build

cp `stack path --local-install-root`/bin/client.jsexe/all.js $ASSETS/ghcjs.js
