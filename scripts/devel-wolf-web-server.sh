#!/bin/bash

set -e
set -x

stack install :wolf-web-server --file-watch --exec='./scripts/restart-wolf-web-server.sh'
