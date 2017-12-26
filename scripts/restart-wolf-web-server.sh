#!/bin/bash
set -e
set -x

cd $HOME

killall wolf-web-server || true

export PORT=8000
export DATA_DIR=/tmp/shared-wolf

wolf-web-server serve &
