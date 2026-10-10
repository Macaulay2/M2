#!/usr/bin/env python3
"""Verify actual generated install commands for optional Maple examples."""
from pathlib import Path
import re
import shutil
import subprocess
import tempfile
import unittest

ROOT = Path(__file__).resolve().parents[3]


class MapleExamplesTests(unittest.TestCase):
    def test_generated_install_commands(self):
        names = ['Style', 'FirstPackage', 'Macaulay2Doc', 'MapleInterface',
                 'AdjointIdeal', 'Parametrization', 'ConvexInterface', 'Independent']
        with tempfile.TemporaryDirectory(prefix='m2-maple-examples-') as tmp:
            root = Path(tmp)
            src = root / 'src'
            packages = src / 'packages'
            packages.mkdir(parents=True)
            shutil.copy(ROOT / 'M2/Macaulay2/packages/CMakeLists.txt', packages)
            (src / 'cmake').mkdir()
            shutil.copytree(ROOT / 'M2/cmake/package-install', src / 'cmake/package-install')
            (src / 'cmake/package-dependencies.cmake').write_text(
                'set(M2_DEPENDENCY_PACKAGES ' + ' '.join(names) + ')\n')
            for name in names:
                (packages / (name + '.m2')).write_text(name)
            (packages / 'LanguageServer').mkdir()
            (packages / 'LanguageServer/M2-language-server').touch()
            (src / 'latex.cmake').write_text('macro(_ADD_LATEX_TARGET)\nendmacro()\n')
            (src / 'CMakeLists.txt').write_text('''cmake_minimum_required(VERSION 3.30)
project(MapleExamples NONE)
set(CMAKE_MODULE_PATH "${CMAKE_CURRENT_SOURCE_DIR}")
set(M2_DIST_PREFIX "${CMAKE_BINARY_DIR}/dist")
set(M2_INSTALL_BINDIR bin)
set(M2_INSTALL_LIBDIR lib)
set(M2_INSTALL_DATADIR share)
set(M2_INSTALL_DOCDIR doc)
set(M2_INSTALL_INFODIR info)
set(BUILD_DOCS ON)
set(BUILD_TESTING OFF)
set(DISTRIBUTED_PACKAGES "''' + ';'.join(names) + '''")
file(MAKE_DIRECTORY "${M2_DIST_PREFIX}/share/Core")
file(WRITE "${M2_DIST_PREFIX}/share/Core/tvalues.m2" "")
add_custom_target(M2-binary)
add_custom_target(M2-core)
add_subdirectory(packages)
''')
            cases = [
                ('OFF', 'TRUE', 'TRUE', 'true', set(names[3:7])),
                ('ON', 'FALSE', 'FALSE', 'true', set(names[3:7])),
                ('ON', 'TRUE', 'FALSE', 'true', {'ConvexInterface'}),
                ('ON', 'TRUE', 'TRUE', 'true', set()),
                ('ON', 'TRUE', 'TRUE', 'false', set(names)),
            ]
            for enabled, maple, convex, rerun, cached in cases:
                with self.subTest(enabled=enabled, maple=maple, convex=convex, rerun=rerun):
                    build = root / 'build'
                    subprocess.run(['cmake', '-S', str(src), '-B', str(build), '-G', 'Ninja',
                                    f'-DWITH_MAPLE={enabled}', f'-DMaple_FOUND={maple}',
                                    f'-DMaple_Convex_FOUND={convex}', f'-DRerunExamples={rerun}'],
                                   check=True, capture_output=True)
                    commands = subprocess.check_output(
                        ['ninja', '-C', str(build), '-t', 'commands', 'install-packages'], text=True)
                    # Ninja's shell quoting escapes the embedded M2 quotes.
                    commands = commands.replace('\\"', '"')
                    actual = dict(re.findall(
                        r'installPackage\("([^"]+)"[^\n]*?RerunExamples => (true|false)', commands))
                    self.assertEqual(actual, {name: 'false' if name in cached else 'true'
                                              for name in names})


if __name__ == '__main__':
    unittest.main()
