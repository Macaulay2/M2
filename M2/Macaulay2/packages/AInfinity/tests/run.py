"""Run the local package's embedded and standalone M2 tests in fresh processes."""
from concurrent.futures import ThreadPoolExecutor
from pathlib import Path
from tempfile import TemporaryDirectory
import argparse
import json
import re
import subprocess
import sys

parser = argparse.ArgumentParser(description=__doc__)
parser.add_argument("--m2", default="M2", help="Macaulay2 executable")
parser.add_argument("--jobs", type=int, default=2)
parser.add_argument("--timeout", type=int, default=240, help="seconds per test process")
parser.add_argument("filter", nargs="*", help="optional test names or prefixes")
args = parser.parse_args()
if args.jobs < 1 or args.timeout < 1:
    parser.error("--jobs and --timeout must be positive")
testdir = Path(__file__).resolve().parent
root = testdir.parents[1]
main = root / "AInfinity.m2"
# Include each TEST file shipped with the package, including auxiliaries
# containing only new feature regressions. Run every body in a fresh M2.
sources = [("package", main)]
prefixes = {"typed-tests": "typed", "regressions": "regression"}
for source in sorted((root / "AInfinity").glob("*.m2")):
    sources.append((prefixes.get(source.stem, source.stem), source))
cases = {}
for prefix, source in sources:
    cases.update({f"{prefix}-{i:02d}": body for i, body in enumerate(
        re.findall(r"^TEST\s*///(.*?)^///", source.read_text(), re.MULTILINE | re.DOTALL))})
cases.update({p.stem: p.read_text() for p in sorted(testdir.glob("*.m2"))})
if args.filter:
    cases = {k: v for k, v in cases.items() if any(k.startswith(f) for f in args.filter)}
if not cases:
    parser.error("no matching tests")
preamble = (f"path = prepend({json.dumps(str(root) + '/')},path);\n"
            f"loadPackage(\"AInfinity\",FileName=>{json.dumps(str(main))});\n")

def run(item):
    name, body = item
    with TemporaryDirectory(prefix="ainfinity-test-") as work:
        script = Path(work) / (name + ".m2")
        script.write_text(preamble + body + "\n")
        try:
            result = subprocess.run([args.m2, "--script", str(script)], cwd=work,
                                    stdout=subprocess.PIPE, stderr=subprocess.STDOUT,
                                    text=True, timeout=args.timeout)
            return name, result.returncode == 0, result.stdout
        except subprocess.TimeoutExpired:
            return name, False, f"exceeded {args.timeout} seconds"

failed = 0
with ThreadPoolExecutor(max_workers=args.jobs) as pool:
    for name, ok, output in pool.map(run, cases.items()):
        print(f"{'PASS' if ok else 'FAIL'} {name}", flush=True)
        if not ok:
            failed += 1
            print(output)
print(f"{len(cases)-failed}/{len(cases)} passed")
sys.exit(bool(failed))
