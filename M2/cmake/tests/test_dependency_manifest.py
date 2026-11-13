#!/usr/bin/env python3
"""Check manifest generation and failures with a real external M2."""
from pathlib import Path
import os
import shutil
import subprocess
import sys
import tempfile

source = Path(__file__).resolve().parents[2]
with tempfile.TemporaryDirectory(prefix="m2-manifest-test-") as tmp:
    root = Path(tmp)
    (root / "cmake").mkdir()
    packages = root / "Macaulay2/packages"
    packages.mkdir(parents=True)
    script = root / "cmake/package-dependencies.py"
    shutil.copy(source / "cmake/package-dependencies.py", script)
    shutil.copy(source / "cmake/package-headers.m2", root / "cmake")
    names = ["Style", "FirstPackage", "Macaulay2Doc", "Alpha", "Beta"]
    (packages / "=distributed-packages").write_text("\n".join(names))
    for name in names:
        (packages / (name + ".m2")).write_text(f'newPackage("{name}")\n')
    (packages / "Alpha.m2").write_text(
        'newPackage("Alpha", PackageImports => {"Beta", "Core", null}, '
        'PackageExports => {"Beta", "User", null})\n')
    (packages / "Beta.m2").write_text('newPackage("Beta")\nneedsPackage "Alpha"\n')

    def run(*args, succeeds=True):
        result = subprocess.run([sys.executable, str(script), "--m2", os.environ.get("M2", "M2"), *args],
                                capture_output=True, text=True)
        assert (result.returncode == 0) == succeeds, result.stdout + result.stderr
        return result.stdout + result.stderr

    run()
    manifest = root / "cmake/package-dependencies.cmake"
    assert 'set(M2_PACKAGE_IMPORTS_Alpha "Beta")' in manifest.read_text()
    assert 'set(M2_PACKAGE_SOURCES_Beta "Alpha")' in manifest.read_text()
    run("--check")
    (packages / "Alpha.m2").write_text('newPackage("Alpha", PackageImports => {"Style"})\n')
    assert "manifest is stale" in run("--check", succeeds=False)
    run()
    run("--check")
    before = manifest.read_text()

    def reject(message):
        assert message in run(succeeds=False)
        assert manifest.read_text() == before

    (packages / "Alpha.m2").write_text('newPackage("Alpha", PackageImports => {"Beta"})\n')
    (packages / "Beta.m2").write_text('newPackage("Beta", PackageImports => {"Alpha"})\n')
    reject("Cyclic installation dependencies: Alpha -> Beta -> Alpha")
    (packages / "Alpha.m2").write_text('newPackage("Alpha")\n')
    (packages / "Beta.m2").write_text('newPackage("Beta")\n')

    for header, message in [
        ('newPackage("Alpha", PackageImports => {"Alpha"})\n', "Alpha -> Alpha"),
        ('newPackage("Alpha", PackageExports => {"UnknownPackage"})\n',
         "Dependency outside distributed packages"),
        ('newPackage("Alpha", PackageImports => {42})\n', "expected a package name"),
        ('newPackage("DifferentName")\n', "Alpha.m2"),
        (None, "Alpha.m2"),
    ]:
        filename = packages / "Alpha.m2"
        if header is None:
            filename.unlink()
        else:
            filename.write_text(header)
        reject(message)
        filename.write_text('newPackage("Alpha")\n')

    (packages / "Macaulay2Doc.m2").write_text('newPackage("Macaulay2Doc", PackageImports => {"Alpha"})\n')
    reject("Cyclic installation dependencies")
print("PASS: external M2 generation, freshness, source cycles, installation cycles, and header errors")
