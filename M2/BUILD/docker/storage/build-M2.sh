#!/bin/bash

set -x
set -e

mkdir -p /home/macaulay/M2/M2/BUILD/docker-build
cd /home/macaulay/M2/M2/BUILD/docker-build

cmake ../..
make install-libraries install-programs

cmake .
make install

yes | M2 -q --stop --silent -e "setupEmacs(); exit(0);"
