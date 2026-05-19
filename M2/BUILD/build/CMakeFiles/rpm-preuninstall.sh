#! /bin/sh -e
cd /usr/local/share/info
for p in 
do install-info --remove --quiet --info-dir=. $p.info || true
done