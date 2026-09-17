"""Check that unavailable Maple imports cannot reenter the CI package selection."""
from pathlib import Path
import re
import subprocess
import tempfile
import unittest


class PackageSelectionTests(unittest.TestCase):
    def select(self, source, output):
        script = Path(__file__).with_name('select-packages.cmake')
        subprocess.run(['cmake', f'-DSOURCE={source}', f'-DOUTPUT={output}',
                        '-P', str(script)], check=True, capture_output=True)
        return set(output.read_text().split(';'))

    def test_transitive_exclusion_independent_of_manifest_order(self):
        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp)
            (root / 'M2/cmake').mkdir(parents=True)
            (root / 'M2/Macaulay2/packages').mkdir(parents=True)
            (root / 'M2/cmake/package-dependencies.cmake').write_text('''
set(M2_DEPENDENCY_PACKAGES Indirect Direct MapleInterface Independent)
set(M2_PACKAGE_IMPORTS_Indirect Direct)
set(M2_PACKAGE_IMPORTS_Direct MapleInterface)
''')
            (root / 'M2/Macaulay2/packages/=distributed-packages').write_text(
                'Indirect\nDirect\nMapleInterface\nIndependent\n')
            self.assertEqual(self.select(root, root / 'selected'), {'Independent'})

    def test_real_selection_is_closed_under_declared_imports(self):
        root = Path(__file__).resolve().parents[2]
        manifest = (root / 'M2/cmake/package-dependencies.cmake').read_text()
        imports = {p: set(deps.split(';')) - {''} for p, deps in re.findall(
            r'set\(M2_PACKAGE_IMPORTS_(\w+) "([^"]*)"\)', manifest)}
        with tempfile.TemporaryDirectory() as tmp:
            selected = self.select(root, Path(tmp) / 'selected')
        self.assertNotIn('MapleInterface', selected)
        self.assertIn('Macaulay2Doc', selected)
        for package in selected:
            self.assertLessEqual(imports[package], selected, package)


if __name__ == '__main__':
    unittest.main()
