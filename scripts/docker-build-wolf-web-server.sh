#!/bin/bash

set -e
set -x

stack build :wolf-web-server
binpathabs=$(stack path --local-install-root)/bin/wolf-web-server
mkdir -p deploy/dist
binpathrel="deploy/dist/wolf-web-server"
cp "${binpathabs}" "${binpathrel}"
docker build -t wolf-web-server deploy
