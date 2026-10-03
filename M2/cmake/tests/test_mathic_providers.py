#!/usr/bin/env python3
"""Exercise provider selection with isolated CMake/pkg-config installations."""
import os
from pathlib import Path
import subprocess
import tempfile
import unittest

MODULES = Path(__file__).resolve().parents[1]
LIBS = {'memtailor': '1.4', 'mathic': '1.5', 'mathicgb': '1.4'}


class Providers(unittest.TestCase):
    def setUp(self):
        temp = tempfile.TemporaryDirectory()
        self.addCleanup(temp.cleanup)
        self.root = Path(temp.name)
        self.prefix = self.root / 'prefix'
        headers = self.prefix / 'include'
        headers.mkdir(parents=True)
        for lib, version in LIBS.items():
            api = (f'inline void lib{lib}IsPresent() {{}}' if lib != 'mathicgb' else
                   'namespace mgb { struct GroebnerConfiguration { '
                   'GroebnerConfiguration(int,int,int) {} }; }')
            (headers / f'{lib}.h').write_text(api)
            config = self.prefix / 'lib/cmake' / lib
            config.mkdir(parents=True)
            (config / f'{lib}Config.cmake').write_text(
                f'add_library({lib}::{lib} INTERFACE IMPORTED)\n'
                f'set_target_properties({lib}::{lib} PROPERTIES '
                f'INTERFACE_INCLUDE_DIRECTORIES "{headers}")\n')
            (config / f'{lib}ConfigVersion.cmake').write_text(
                f'set(PACKAGE_VERSION {version})\nset(PACKAGE_VERSION_COMPATIBLE TRUE)\n')
            pc = self.prefix / 'lib/pkgconfig'
            pc.mkdir(exist_ok=True)
            (pc / f'{lib}.pc').write_text(
                f'Name: {lib}\nDescription: Provider test fixture\nVersion: {version}\n'
                f'Cflags: -I{headers}\nLibs:\n')
        self.source = self.root / 'source'
        engine = self.source / 'e'
        engine.mkdir(parents=True)
        (self.source / 'CMakeLists.txt').write_text(
            'cmake_minimum_required(VERSION 3.25)\nproject(Providers LANGUAGES CXX)\n'
            f'list(PREPEND CMAKE_MODULE_PATH "{MODULES}")\n'
            'option(WITH_TBB "TBB" ON)\nadd_subdirectory(e)\n')
        (engine / 'CMakeLists.txt').write_text(
            'include(select-mathic-libraries)\n'
            'file(WRITE "${CMAKE_BINARY_DIR}/selection" '
            '"${_memtailor_external};${_mathic_external};${_mathicgb_external}")\n')
        for lib in LIBS:
            (engine / lib).mkdir()
            (engine / lib / 'dummy.cpp').write_text('int dummy;\n')
            (engine / lib / 'CMakeLists.txt').write_text(f'add_library({lib} STATIC dummy.cpp)\n')

    def configure(self, *options, error=None):
        env = dict(os.environ, PKG_CONFIG_LIBDIR=str(self.prefix / 'lib/pkgconfig'),
                   PKG_CONFIG_PATH='')
        result = subprocess.run(
            ['cmake', '-S', str(self.source), '-B', str(self.root / 'build'),
             f'-DCMAKE_PREFIX_PATH={self.prefix}', '-DCMAKE_BUILD_TYPE=Release', *options],
            env=env, text=True, stdout=subprocess.PIPE, stderr=subprocess.STDOUT)
        if error:
            self.assertNotEqual(result.returncode, 0, result.stdout)
            self.assertIn(error, ' '.join(result.stdout.split()))
        else:
            self.assertEqual(result.returncode, 0, result.stdout)
            return (self.root / 'build/selection').read_text()

    def test_system(self):
        self.assertEqual(self.configure(*[f'-D{x.upper()}_PROVIDER=SYSTEM' for x in LIBS]),
                         'TRUE;TRUE;TRUE')

    def test_release_libraries_in_debug_engine(self):
        self.assertEqual(self.configure('-DCMAKE_BUILD_TYPE=Debug'), 'TRUE;TRUE;TRUE')

    def test_bundled(self):
        self.assertEqual(self.configure(*[f'-D{x.upper()}_PROVIDER=BUNDLED' for x in LIBS]),
                         'FALSE;FALSE;FALSE')

    def test_dependency_fallback(self):
        self.assertEqual(self.configure('-DMEMTAILOR_PROVIDER=BUNDLED'), 'FALSE;FALSE;FALSE')

    def test_explicit_inconsistent_providers(self):
        self.configure('-DMEMTAILOR_PROVIDER=BUNDLED', '-DMATHIC_PROVIDER=SYSTEM',
                       error='system mathic requires system memtailor')

    def test_debug_bundled_dependency_layout(self):
        self.assertEqual(self.configure('-DCMAKE_BUILD_TYPE=Debug',
                                        '-DMATHICGB_PROVIDER=BUNDLED'), 'FALSE;FALSE;FALSE')

    def test_debug_system_dependency_layout_error(self):
        self.configure('-DCMAKE_BUILD_TYPE=Debug', '-DMATHICGB_PROVIDER=BUNDLED',
                       '-DMEMTAILOR_PROVIDER=SYSTEM',
                       error='bundled Debug mathic/mathicgb requires MEMT_DEBUG=ON')

    def test_tbb_mismatch_fallback(self):
        self.assertEqual(self.configure('-DWITH_TBB=OFF'), 'TRUE;TRUE;FALSE')

    def test_tbb_mismatch_required(self):
        self.configure('-DWITH_TBB=OFF', '-DMATHICGB_PROVIDER=SYSTEM',
                       error='disagrees with WITH_TBB=OFF')

    def test_transitive_abi_mismatch(self):
        p = self.prefix / 'lib/cmake/mathic/mathicConfig.cmake'
        with p.open('a') as out:
            out.write('set_property(TARGET mathic::mathic PROPERTY '
                      'INTERFACE_COMPILE_DEFINITIONS MEMT_DEBUG)\n')
        self.assertEqual(self.configure(), 'TRUE;FALSE;FALSE')

    def test_pkg_config(self):
        self.assertEqual(self.configure(*[f'-DCMAKE_DISABLE_FIND_PACKAGE_{x}=TRUE'
                                          for x in LIBS]), 'TRUE;TRUE;TRUE')

    def test_no_tbb_system_library(self):
        p = self.prefix / 'lib/cmake/mathicgb/mathicgbConfig.cmake'
        with p.open('a') as out:
            out.write('set_property(TARGET mathicgb::mathicgb PROPERTY '
                      'INTERFACE_COMPILE_DEFINITIONS MATHICGB_NO_TBB)\n')
        self.assertEqual(self.configure('-DWITH_TBB=OFF'), 'TRUE;TRUE;TRUE')

    def test_reconfigure_refreshes_abi(self):
        self.assertEqual(self.configure(), 'TRUE;TRUE;TRUE')
        p = self.prefix / 'lib/cmake/mathic/mathicConfig.cmake'
        with p.open('a') as out:
            out.write('set_property(TARGET mathic::mathic PROPERTY '
                      'INTERFACE_COMPILE_DEFINITIONS MEMT_DEBUG)\n')
        self.assertEqual(self.configure(), 'TRUE;FALSE;FALSE')

    def test_unusable_metadata(self):
        p = self.prefix / 'include/mathic.h'
        p.write_text('#error unusable installed header\n')
        self.configure('-DMATHIC_PROVIDER=SYSTEM', error='failed its compile/link check')

    def test_invalid_provider(self):
        self.configure('-DMATHIC_PROVIDER=TYPO', error='MATHIC_PROVIDER must be')


if __name__ == '__main__':
    unittest.main()
