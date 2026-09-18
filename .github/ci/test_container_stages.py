"""Exercise the stage handoff using real source preparation and PAX archives."""
import json
import os
from pathlib import Path
import subprocess
import tempfile
import unittest


class ContainerStagesTests(unittest.TestCase):
    def test_build_checkpoint_then_tests(self):
        ci = Path(__file__).resolve().parent
        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp)
            incoming = root / 'input'
            scripts = incoming / '.github/ci'
            scripts.mkdir(parents=True)
            for name in ('prepare_source.py', 'container-environment.sh'):
                (scripts / name).write_bytes((ci / name).read_bytes())
            (incoming / '.ci-snapshot.json').write_text(json.dumps(
                {'revision': 'fixture', 'key': 'fixture', 'files': {}}))
            state = root / 'opt'
            driver = root / 'driver.sh'
            driver.write_text((ci / 'container-build.sh').read_text()
                              .replace('/opt/m2', str(state)).replace('/input', str(incoming)))
            tools = root / 'bin'
            tools.mkdir()
            mock = tools / 'mock'
            mock.write_text('''#!/usr/bin/env python3
import json, os, pathlib, shutil, sys
name = pathlib.Path(sys.argv[0]).name
state = pathlib.Path(os.environ['TEST_STATE'])
args = sys.argv[1:]
with (state / 'calls').open('a') as out:
    out.write(json.dumps([name, args]) + '\\n')
if name == 'cmake' and '-S' in args:
    build = pathlib.Path(args[args.index('-B') + 1])
    build.mkdir(parents=True)
    shutil.copyfile(__file__, build / 'M2')
    (build / 'M2').chmod(0o755)
if name == 'cmake' and 'install-packages' in args:
    (state / 'build/installed').write_text('completed')
if name == 'cmake' and 'check-packages' in args:
    assert (state / 'build/installed').read_text() == 'completed'
if name == 'ccache':
    cache = pathlib.Path(os.environ['CCACHE_DIR'])
    cache.mkdir(exist_ok=True)
    (cache / 'object').write_text('cached')
if name == 'cpack':
    pathlib.Path('Macaulay2-fixture.deb').touch()
''')
            mock.chmod(0o755)
            for name in ['cmake', 'ccache', 'ctest', 'cpack']:
                (tools / name).symlink_to(mock)
            env = dict(os.environ, PATH=str(tools) + os.pathsep + os.environ['PATH'],
                       TEST_STATE=str(state), CMAKE_BUILD_PARALLEL_LEVEL='3')
            subprocess.run(['bash', str(driver), 'build'], env=env, check=True, capture_output=True)
            self.assertTrue((state / 'build-cache.tar').exists())
            self.assertFalse((state / 'build').exists())
            self.assertTrue((state / 'ccache/object').exists())
            calls = [json.loads(line) for line in (state / 'calls').read_text().splitlines()]
            self.assertFalse(any(name in ['M2', 'ctest', 'cpack'] for name, args in calls))
            (state / 'calls').write_text('')
            subprocess.run(['bash', str(driver), 'test'], env=env, check=True, capture_output=True)
            calls = [json.loads(line) for line in (state / 'calls').read_text().splitlines()]
            self.assertTrue(any('check-packages' in args for name, args in calls))
            self.assertFalse(any('install-packages' in args or '-S' in args for name, args in calls))
            self.assertEqual(sum(name == 'M2' for name, args in calls), 3)
            self.assertTrue((state / 'artifacts/Macaulay2-fixture.deb').exists())
            self.assertTrue((state / 'ccache/object').exists())


if __name__ == '__main__':
    unittest.main()
