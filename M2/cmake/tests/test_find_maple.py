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
        self.build = self.root / 'build'
        self.executable = self.root / 'Maple with spaces'
        self.script = self.root / 'CMakeLists.txt'
        self.script.write_text(f'''cmake_minimum_required(VERSION 3.30)
project(MapleDetection NONE)
list(PREPEND CMAKE_MODULE_PATH "{MODULES}")
find_package(Maple OPTIONAL_COMPONENTS Convex)
file(WRITE "${{CMAKE_CURRENT_BINARY_DIR}}/result" "${{Maple_FOUND}};${{Maple_Convex_FOUND}}")
file(WRITE "${{CMAKE_CURRENT_BINARY_DIR}}/reasons"
  "${{Maple_NOT_FOUND_MESSAGE}}\\n${{Maple_Convex_NOT_FOUND_MESSAGE}}")
''')

    def fake(self, body):
        self.executable.write_text('#!' + sys.executable + '\nimport sys, time\n' + body)
        self.executable.chmod(0o755)

    def probe(self, *args):
        result = subprocess.run(['cmake', '-S', str(self.root), '-B', str(self.build),
                        f'-DMAPLE_EXECUTABLE={self.executable}',
                        '-DMAPLE_PROBE_TIMEOUT=0.2', *args], cwd=self.root, check=True,
                       capture_output=True, text=True)
        self.output = result.stdout
        self.reasons = (self.build / 'reasons').read_text()
        return (self.build / 'result').read_text()

    def test_absent(self):
        self.assertEqual(self.probe(), 'FALSE;FALSE')
        self.assertIn(f'Maple executable not found: {self.executable}', self.output)
        self.assertNotIn('probe.log', self.reasons)

    def test_license_failure_even_with_zero_exit(self):
        self.fake('print("License unavailable", file=sys.stderr)\n')
        self.assertEqual(self.probe(), 'FALSE;FALSE')
        log = self.build / 'CMakeFiles/Maple/probe.log'
        self.assertIn(str(log), self.output)
        self.assertIn('License unavailable', log.read_text())

    def test_echoed_input_is_not_success(self):
        self.fake('print(sys.stdin.read())\n')
        self.assertEqual(self.probe(), 'FALSE;FALSE')

    def test_failed_exit_is_not_success(self):
        self.fake('print("M2_MAPLE_42"); sys.exit(1)\n')
        self.assertEqual(self.probe(), 'FALSE;FALSE')

    def test_timeout(self):
        self.fake('time.sleep(10)\n')
        self.assertEqual(self.probe(), 'FALSE;FALSE')
        self.assertIn('probe.log', self.reasons)

    def test_maple_without_convex(self):
        self.fake('s = sys.stdin.read()\nif "with(convex)" not in s: print("M2_MAPLE_42")\n'
                  'else: print("convex unavailable", file=sys.stderr)\n')
        self.assertEqual(self.probe(), 'TRUE;FALSE')
        log = self.build / 'CMakeFiles/Maple/convex.log'
        self.assertIn(str(log), self.reasons)
        self.assertIn('convex unavailable', log.read_text())

    def test_recheck_after_license_failure_and_recovery(self):
        working = 's = sys.stdin.read()\nprint("M2_CONVEX_42" if "with(convex)" in s else "M2_MAPLE_42")\n'
        self.fake(working)
        self.assertEqual(self.probe(), 'TRUE;TRUE')
        self.fake('print("License unavailable"); sys.exit(1)\n')
        self.assertEqual(self.probe(), 'FALSE;FALSE')
        self.assertIn('probe.log', self.reasons)
        self.fake(working)
        self.assertEqual(self.probe(), 'TRUE;TRUE')
        self.assertEqual(self.reasons, '\n')

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
        marker = self.root / 'executed'
        self.fake(f'open({str(marker)!r}, "w").close()\n')
        self.assertEqual(self.probe('-DCMAKE_SYSTEM_NAME=Generic'), 'FALSE;FALSE')
        self.assertFalse(marker.exists())
        self.assertIn('skipped while cross-compiling', self.reasons)


if __name__ == '__main__':
    unittest.main()
