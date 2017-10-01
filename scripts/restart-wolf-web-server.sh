#!/bin/bash
set -e
set -x

cd $HOME

killall wolf-web-server || true
wolf-web-server serve --personal-data-dir="$HOME/.wolf" --port 8000 &
