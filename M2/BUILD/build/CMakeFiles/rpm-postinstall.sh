#! /bin/sh -e
cd /usr/local/share/info
for package in 
do install-info --quiet --info-dir=. $p.info || true
done