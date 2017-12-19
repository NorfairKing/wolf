#!/bin/bash

set -e
set -x

./scripts/docker-build-wolf-web-server.sh
docker run \
  --rm \
  --publish 8000:80 \
  --volume /tmp/shared-wolf:/www/wolf-data \
  wolf-web-server:latest

