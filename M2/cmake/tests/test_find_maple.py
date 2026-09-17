#!/usr/bin/env python3
"""Exercise Maple detection without requiring a proprietary installation."""
from pathlib import Path
import subprocess
import sys
import tempfile
import unittest

MODULES = Path(__file__).resolve().parents[1]


class MapleDetectionTests(unittest.TestCase):
    def setUp(self):
        self.tmp = tempfile.TemporaryDirectory(prefix='m2-maple-')
        self.addCleanup(self.tmp.cleanup)
        self.root = Path(self.tmp.name)
        self.executable = self.root / 'Maple with spaces'
        self.script = self.root / 'check.cmake'
        self.script.write_text(f'''cmake_minimum_required(VERSION 3.30)
list(PREPEND CMAKE_MODULE_PATH "{MODULES}")
find_package(Maple OPTIONAL_COMPONENTS Convex)
file(WRITE "${{CMAKE_CURRENT_BINARY_DIR}}/result" "${{Maple_FOUND}};${{Maple_Convex_FOUND}}")
''')

    def fake(self, body):
        self.executable.write_text('#!' + sys.executable + '\nimport sys, time\n' + body)
        self.executable.chmod(0o755)

    def probe(self, *args):
        subprocess.run(['cmake', f'-DMAPLE_EXECUTABLE={self.executable}',
                        '-DMAPLE_PROBE_TIMEOUT=0.2', *args,
                        '-P', str(self.script)], cwd=self.root, check=True,
                       capture_output=True, text=True)
        return (self.root / 'result').read_text()

    def test_absent(self):
        self.assertEqual(self.probe(), 'FALSE;FALSE')

    def test_license_failure_even_with_zero_exit(self):
        self.fake('print("License unavailable")\n')
        self.assertEqual(self.probe(), 'FALSE;FALSE')

    def test_echoed_input_is_not_success(self):
        self.fake('print(sys.stdin.read())\n')
        self.assertEqual(self.probe(), 'FALSE;FALSE')

    def test_failed_exit_is_not_success(self):
        self.fake('print("M2_MAPLE_42"); sys.exit(1)\n')
        self.assertEqual(self.probe(), 'FALSE;FALSE')

    def test_timeout(self):
        self.fake('time.sleep(10)\n')
        self.assertEqual(self.probe(), 'FALSE;FALSE')

    def test_maple_without_convex_and_recheck(self):
        self.fake('s = sys.stdin.read()\nif "with(convex)" not in s: print("M2_MAPLE_42")\n')
        self.assertEqual(self.probe(), 'TRUE;FALSE')
        self.fake('sys.exit(1)\n')
        self.assertEqual(self.probe(), 'FALSE;FALSE')

    def test_maple_and_convex_with_custom_directory(self):
        self.fake(f'''s = sys.stdin.read()
if "with(convex)" in s:
    assert 'libname := libname, "{self.root}/library with spaces"' in s
    print("M2_CONVEX_42")
else:
    print("M2_MAPLE_42")
''')
        self.assertEqual(self.probe(f'-DMAPLE_CONVEX_DIR={self.root}/library with spaces'), 'TRUE;TRUE')

    def test_cross_compilation_does_not_execute(self):
        self.fake('open("executed", "w").close()\n')
        self.assertEqual(self.probe('-DCMAKE_CROSSCOMPILING=TRUE'), 'FALSE;FALSE')
        self.assertFalse((self.root / 'executed').exists())


if __name__ == '__main__':
    unittest.main()
