#!/bin/bash

set -e
set -x

docker build --tag registry.gitlab.com/norfair/wolf --file ci/Dockerfile .
