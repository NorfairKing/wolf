#!/bin/bash
set -e
set -x

cd $HOME

killall wolf-web-server || true

PORT=8000
SHARED_DATA_DIR=/tmp/shared-wolf

wolf-web-server serve &
