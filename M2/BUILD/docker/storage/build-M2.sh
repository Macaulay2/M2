#!/bin/bash

set -x
set -e

mkdir -p /home/macaulay/M2/M2/BUILD/build-docker
cd /home/macaulay/M2/M2/BUILD/build-docker

cmake ../..
make build-libraries build-programs

cmake .
make install

yes | M2 -q --stop --silent -e "setupEmacs(); exit(0);"
