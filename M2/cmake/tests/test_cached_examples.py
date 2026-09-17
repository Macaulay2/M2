#!/usr/bin/env python3
"""Evaluate CMake's install expression with real M2 package-option resolution."""
from pathlib import Path
import subprocess
import tempfile

repo = Path(__file__).resolve().parents[3]
rules = (repo / 'M2/Macaulay2/packages/CMakeLists.txt').read_text()
template = rules[rules.index('set(M2_INSTALL_TEMPLATE'):rules.index('set(M2_NEED_TEMPLATE')]
with tempfile.TemporaryDirectory(prefix='m2-example-policy-') as tmp:
    root = Path(tmp)
    cases = [
        ('Ordinary', '', True),
        ('BergmanAbsent', 'CacheExampleOutput => true, OptionalComponentsPresent => false,', False),
        ('BergmanPresent', 'CacheExampleOutput => true, OptionalComponentsPresent => true,', True),
        ('CachedDefault', 'CacheExampleOutput => true,', False),
        ('CachedExplicit', 'UseCachedExampleOutput => true, OptionalComponentsPresent => true,', False),
        ('UncachedExplicit', 'UseCachedExampleOutput => false, OptionalComponentsPresent => false,', True),
    ]
    for name, options, expected in cases:
        (root / (name + '.m2')).write_text(
            f'newPackage("{name}", {options} Headline => "CI fixture")\nend\n')
        script = root / 'render.cmake'
        script.write_text('''cmake_minimum_required(VERSION 3.30)
set(RespectCachedExampleOutput ON)
set(_package_rerun_examples true)
set(CheckDocumentation true)
set(IgnoreExampleErrors false)
set(RemakeAllDocumentation false)
set(M2_DIST_PREFIX "/unused")
set(package "''' + name + '''")
''' + template + '''
string(CONFIGURE "${M2_INSTALL_TEMPLATE}" rendered)
string(REPLACE "$<IF:$<BOOL:>,true,false>" "false" rendered "${rendered}")
file(WRITE "expression.m2" "${rendered}")
''')
        subprocess.run(['cmake', '-P', str(script)], cwd=root, check=True)
        expression = (root / 'expression.m2').read_text()
        # Intercept only installation, after loadPackage has resolved real
        # metadata defaults. No cached data or package installations are changed.
        program = '''installPackage Package := opts -> pkg -> (
    assert(opts.RerunExamples === ''' + str(expected).lower() + '''); pkg);
''' + expression + '\nexit 0\n'
        (root / 'test.m2').write_text(program)
        result = subprocess.run(['M2', '--script', 'test.m2'], cwd=root, text=True, capture_output=True)
        assert result.returncode == 0, (name, expression, result.stdout, result.stderr)
        print('PASS:', name, flush=True)
