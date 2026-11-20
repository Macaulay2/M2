#!/usr/bin/env python3
"""Export an exact Git revision (including submodules) and key its build environment."""
import argparse
import hashlib
import json
from pathlib import Path
import subprocess
import tarfile


def git(repo, *args):
    return subprocess.check_output(['git', '-C', str(repo), *args])


def export(repo, destination):
    """Export committed files only: no credentials, Git metadata, or local outputs."""
    destination.mkdir(parents=True, exist_ok=True)
    with subprocess.Popen(['git', '-C', str(repo), 'archive', 'HEAD'], stdout=subprocess.PIPE) as proc:
        with tarfile.open(fileobj=proc.stdout, mode='r|') as archive:
            archive.extractall(destination, filter='data')
        if proc.wait():
            raise RuntimeError('git archive failed')
    modules = {}
    for entry in git(repo, 'ls-tree', '-rz', 'HEAD').split(b'\0'):
        if not entry:
            continue
        metadata, name = entry.split(b'\t', 1)
        mode, _, revision = metadata.split()
        if mode != b'160000':
            continue
        relative = name.decode()
        checkout = repo / relative
        actual = git(checkout, 'rev-parse', 'HEAD').strip()
        if actual != revision:
            raise RuntimeError(f'Initialize the pinned submodule: {relative}')
        modules[relative] = revision.decode()
        for child, sha in export(checkout, destination / relative).items():
            modules[f'{relative}/{child}'] = sha
    return modules


def inventory(root):
    return {p.relative_to(root).as_posix(): 'link' if p.is_symlink() else 'file'
            for p in root.rglob('*') if p.is_symlink() or p.is_file()}


def environment_key(root, modules):
    # Key compiler/library compatibility, not build orchestration or package
    # policy. Bump cache-version for incompatible path/archive/script changes.
    paths = [root / 'M2/CMakeLists.txt', root / 'M2/VERSION', root / '.gitmodules']
    paths += [root / '.github/ci' / name for name in
              ('container-environment.sh', 'cache-version')]
    paths += [root / 'M2/BUILD/docker/incremental/Dockerfile']
    paths += [p for p in (root / 'M2/cmake').glob('*.cmake')
              if p.name != 'package-dependencies.cmake']
    # ExternalProject patch commands do not track patch contents themselves.
    paths += [p for p in (root / 'M2/libraries').rglob('*') if p.is_file()]
    digest = hashlib.sha256(json.dumps(modules, sort_keys=True).encode())
    for path in sorted(paths):
        if path.is_file():
            digest.update(path.relative_to(root).as_posix().encode() + b'\0')
            digest.update(path.read_bytes() + b'\0')
    return digest.hexdigest()[:20]


def snapshot(repo, destination):
    if destination.exists():
        raise ValueError('Snapshot destination must not already exist')
    modules = export(repo, destination)
    metadata = {'revision': git(repo, 'rev-parse', 'HEAD').decode().strip(),
                'key': environment_key(destination, modules),
                'files': inventory(destination)}
    (destination / '.ci-snapshot.json').write_text(json.dumps(metadata, sort_keys=True))
    return metadata


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('repository', type=Path)
    parser.add_argument('destination', type=Path)
    args = parser.parse_args()
    result = snapshot(args.repository.resolve(), args.destination.resolve())
    print('revision=' + result['revision'])
    print('key=' + result['key'])
