#!/bin/bash
set -e
set -x

cd $HOME

killall wolf-web-server || true
wolf-web-server serve --shared-data-dir="/tmp/shared-wolf" --port 8000 &
