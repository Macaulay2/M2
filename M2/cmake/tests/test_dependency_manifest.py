#!/usr/bin/env python3
"""Check manifest freshness and cycle failures with a real external M2."""
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
    (root / "Macaulay2/m2").mkdir()
    script = root / "cmake/package-dependencies.py"
    shutil.copy(source / "cmake/package-dependencies.py", script)
    shutil.copy(source / "Macaulay2/m2/check-package-dependencies.m2", root / "Macaulay2/m2")
    names = ["Style", "FirstPackage", "Macaulay2Doc", "Alpha", "Beta"]
    (packages / "=distributed-packages").write_text("\n".join(names))
    for name in names:
        (packages / (name + ".m2")).write_text(f'newPackage("{name}")\n')
    (packages / "Alpha.m2").write_text('newPackage("Alpha", PackageImports => {"Beta"})\n')
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
    (packages / "Alpha.m2").write_text('newPackage("Alpha", PackageImports => {"Beta"})\n')
    (packages / "Beta.m2").write_text('newPackage("Beta", PackageImports => {"Alpha"})\n')
    assert "Cyclic components: 1" in run(succeeds=False)
    assert manifest.read_text() == before
    (packages / "Alpha.m2").write_text('newPackage("Alpha")\n')
    (packages / "Beta.m2").write_text('newPackage("Beta")\n')
    (packages / "Macaulay2Doc.m2").write_text('newPackage("Macaulay2Doc", PackageImports => {"Alpha"})\n')
    assert "Cyclic installation dependencies" in run(succeeds=False)
    assert manifest.read_text() == before
print("PASS: external M2 generation, freshness, source cycles, header cycles, and bootstrap cycles")
