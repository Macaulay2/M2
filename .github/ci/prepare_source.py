#!/usr/bin/env python3
"""Overlay a source snapshot without making unchanged files look newer than outputs."""
import argparse
import json
from pathlib import Path
import shutil
import subprocess


def prepare(incoming, source, build):
    new = json.loads((incoming / '.ci-snapshot.json').read_text())
    marker = source / '.ci-snapshot.json'
    old = json.loads(marker.read_text()) if marker.exists() else None
    reason = None
    if old is None:
        reason = 'no previous source snapshot'
    elif old['key'] != new['key']:
        reason = 'build environment changed'
    elif any(new['files'].get(name) != kind for name, kind in old['files'].items()):
        # file(COPY) staging and ExternalProject stamps do not reliably remove
        # vanished inputs. A clean build avoids testing against stale files.
        reason = 'source files removed or changed type'
    if reason and build.exists():
        shutil.rmtree(build)
    source.mkdir(parents=True, exist_ok=True)
    # No -t: archive/checkout mtimes can predate cached outputs. Changed files
    # receive the current time; checksum-identical files keep their old times.
    subprocess.run(['rsync', '-rlpc', '--delete', str(incoming) + '/', str(source) + '/'], check=True)
    print('Clean build: ' + reason if reason else 'Reusing build outputs', flush=True)
    return reason


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('incoming', type=Path)
    parser.add_argument('source', type=Path)
    parser.add_argument('build', type=Path)
    args = parser.parse_args()
    prepare(args.incoming, args.source, args.build)
