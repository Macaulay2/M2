"""Exercise the source transfer that makes restored CMake builds incremental."""
import hashlib
import json
import os
import shutil
from pathlib import Path
import subprocess
import tempfile
import tarfile
import unittest

from prepare_source import prepare
from source_snapshot import environment_key, inventory, snapshot


class SnapshotTests(unittest.TestCase):
    def setUp(self):
        self.tmp = tempfile.TemporaryDirectory(prefix='m2-ci-test-')
        self.addCleanup(self.tmp.cleanup)
        self.root = Path(self.tmp.name)

    def git(self, repo, *args):
        return subprocess.check_output(['git', '-C', str(repo), *args], stderr=subprocess.PIPE)

    def repository(self, name):
        repo = self.root / name
        repo.mkdir()
        self.git(repo, 'init', '-q')
        self.git(repo, 'config', 'user.email', 'test@example.invalid')
        self.git(repo, 'config', 'user.name', 'CI fixture')
        return repo

    def commit(self, repo):
        self.git(repo, 'add', '.')
        self.git(repo, 'commit', '-qm', 'fixture')

    def test_export_includes_pinned_submodules_without_credentials_or_untracked_files(self):
        child = self.repository('child')
        (child / 'library.c').write_text('int library;\n')
        self.commit(child)
        repo = self.repository('repo')
        (repo / 'tracked').write_text('tracked\n')
        self.git(repo, '-c', 'protocol.file.allow=always', 'submodule', 'add', '-q', str(child), 'deps/library')
        self.commit(repo)
        (repo / 'untracked').write_text('not a build input')
        self.git(repo, 'config', 'http.extraheader', 'fake-credential')
        (repo / 'tracked').write_text('uncommitted change')
        out = self.root / 'snapshot'
        data = snapshot(repo, out)
        self.assertEqual((out / 'tracked').read_text(), 'tracked\n')
        self.assertEqual((out / 'deps/library/library.c').read_text(), 'int library;\n')
        self.assertFalse((out / 'untracked').exists())
        self.assertFalse(any(p.name == '.git' for p in out.rglob('*')))
        self.assertEqual(data['revision'], self.git(repo, 'rev-parse', 'HEAD').decode().strip())
        self.assertIn('deps/library/library.c', data['files'])

    def test_export_rejects_an_uninitialized_submodule(self):
        child = self.repository('child')
        (child / 'library.c').write_text('int library;\n')
        self.commit(child)
        repo = self.repository('repo')
        self.git(repo, '-c', 'protocol.file.allow=always', 'submodule', 'add', '-q', str(child), 'deps/library')
        self.commit(repo)
        self.git(repo, 'submodule', 'deinit', '-f', 'deps/library')
        with self.assertRaisesRegex(RuntimeError, 'Initialize the pinned submodule'):
            snapshot(repo, self.root / 'snapshot')

    def test_environment_key_changes_for_toolchain_but_not_package_metadata(self):
        root = self.root / 'source'
        (root / 'M2/cmake').mkdir(parents=True)
        (root / 'M2/VERSION').write_text('1.0')
        key = environment_key(root, {'deps/library': 'abc'})
        (root / 'M2/cmake/package-dependencies.cmake').write_text('changed imports')
        self.assertEqual(key, environment_key(root, {'deps/library': 'abc'}))
        (root / 'M2/VERSION').write_text('2.0')
        self.assertNotEqual(key, environment_key(root, {'deps/library': 'abc'}))
        self.assertNotEqual(key, environment_key(root, {'deps/library': 'def'}))

    def test_package_policy_and_orchestration_do_not_invalidate_environment(self):
        root = self.root / 'source'
        scripts = root / '.github/ci'
        scripts.mkdir(parents=True)
        environment = scripts / 'container-environment.sh'
        environment.write_text('cmake_environment_args=(-DCMAKE_BUILD_TYPE=Release)')
        version = scripts / 'cache-version'
        version.write_text('2')
        key = environment_key(root, {})
        for name in ('container-build.sh', 'prepare_source.py', 'source_snapshot.py'):
            (scripts / name).write_text('changed package policy or orchestration')
            self.assertEqual(key, environment_key(root, {}))
        environment.write_text('cmake_environment_args=(-DCMAKE_BUILD_TYPE=Debug)')
        self.assertNotEqual(key, environment_key(root, {}))
        key = environment_key(root, {})
        version.write_text('3')
        self.assertNotEqual(key, environment_key(root, {}))
        key = environment_key(root, {})
        dockerfile = root / 'M2/BUILD/docker/incremental/Dockerfile'
        dockerfile.parent.mkdir(parents=True)
        dockerfile.write_text('FROM changed-toolchain')
        self.assertNotEqual(key, environment_key(root, {}))
        key = environment_key(root, {})
        patch = root / 'M2/libraries/factory/patch-4.4.1'
        patch.parent.mkdir(parents=True)
        patch.write_text('changed library patch')
        self.assertNotEqual(key, environment_key(root, {}))

    def test_package_policy_change_preserves_external_project(self):
        incoming = self.incoming()
        scripts = incoming / '.github/ci'
        scripts.mkdir(parents=True)
        policy = scripts / 'container-build.sh'
        policy.write_text('CacheExampleOutput=null')
        library = self.root / 'factory-source'
        library.mkdir()
        (library / 'factory.c').write_text('int factory(void) { return 42; }')
        (library / 'CMakeLists.txt').write_text(
            'cmake_minimum_required(VERSION 3.24)\nproject(factory C)\n'
            'add_library(factory STATIC factory.c)\n'
            'install(TARGETS factory ARCHIVE DESTINATION lib)\n')
        archive = self.root / 'factory.tar.gz'
        with tarfile.open(archive, 'w:gz') as out:
            out.add(library, arcname='factory')
        digest = hashlib.sha256(archive.read_bytes()).hexdigest()
        (incoming / 'CMakeLists.txt').write_text(f'''cmake_minimum_required(VERSION 3.24)
project(ExternalReuse C)
include(ExternalProject)
ExternalProject_Add(build-factory
  URL "{archive.as_uri()}"
  URL_HASH SHA256={digest}
  DOWNLOAD_EXTRACT_TIMESTAMP TRUE
  PREFIX factory
  CMAKE_ARGS -DCMAKE_INSTALL_PREFIX=<INSTALL_DIR>
  BUILD_COMMAND ${{CMAKE_COMMAND}} --build <BINARY_DIR> --parallel 2)
file(READ "${{CMAKE_SOURCE_DIR}}/.github/ci/container-build.sh" policy)
add_custom_command(OUTPUT installed
  COMMAND ${{CMAKE_COMMAND}} -E echo "${{policy}}"
  COMMAND ${{CMAKE_COMMAND}} -E touch installed
  DEPENDS "${{CMAKE_SOURCE_DIR}}/.github/ci/container-build.sh")
add_custom_target(packages ALL DEPENDS installed)
add_dependencies(packages build-factory)
''')
        source, build = self.root / 'source', self.root / 'build'
        def run():
            self.metadata(incoming, key=environment_key(incoming, {}))
            prepare(incoming, source, build)
            subprocess.run(['cmake', '-S', str(source), '-B', str(build), '-G', 'Ninja'],
                           check=True, stdout=subprocess.PIPE, stderr=subprocess.STDOUT)
            return subprocess.check_output(['cmake', '--build', str(build), '--parallel', '2'], text=True)
        run()
        stamp = build / 'factory/src/build-factory-stamp'
        watched = [build / 'factory/lib/libfactory.a',
                   stamp / 'build-factory-download', stamp / 'build-factory-install']
        before = [p.stat().st_mtime_ns for p in watched]
        package_stamp = (build / 'installed').stat().st_mtime_ns
        policy.write_text('CacheExampleOutput=false')
        output = run()
        self.assertEqual(before, [p.stat().st_mtime_ns for p in watched])
        self.assertNotEqual(package_stamp, (build / 'installed').stat().st_mtime_ns)
        self.assertIn('CacheExampleOutput=false', output)
        self.assertIn('no work to do', run())

    def incoming(self):
        path = self.root / 'incoming'
        path.mkdir()
        (path / 'main.c').write_text('int main(void) { return 0; }\n')
        (path / 'unchanged').write_text('same')
        return path

    def metadata(self, path, key='environment'):
        marker = path / '.ci-snapshot.json'
        marker.unlink(missing_ok=True)
        marker.write_text(json.dumps({'revision': 'abc', 'key': key, 'files': inventory(path)}))

    def test_noop_preserves_mtimes_and_modified_files_get_current_times(self):
        incoming = self.incoming()
        self.metadata(incoming)
        source, build = self.root / 'source', self.root / 'build'
        prepare(incoming, source, build)
        build.mkdir()
        (build / 'cached-output').touch()
        old_time = 1_000_000_000
        os.utime(source / 'main.c', (old_time, old_time))
        unchanged = (source / 'unchanged').stat().st_mtime_ns
        # Different checkout/archive mtimes must not change identical sources.
        os.utime(incoming / 'main.c', (old_time - 10, old_time - 10))
        self.assertIsNone(prepare(incoming, source, build))
        self.assertEqual((source / 'main.c').stat().st_mtime, old_time)
        (incoming / 'main.c').write_text('int main(void) { return 1; }\n')
        os.utime(incoming / 'main.c', (old_time - 20, old_time - 20))
        self.assertIsNone(prepare(incoming, source, build))
        self.assertGreater((source / 'main.c').stat().st_mtime, old_time)
        self.assertEqual((source / 'unchanged').stat().st_mtime_ns, unchanged)
        self.assertTrue((build / 'cached-output').exists())

    def test_deletions_and_environment_changes_discard_stale_builds(self):
        incoming = self.incoming()
        self.metadata(incoming)
        source, build = self.root / 'source', self.root / 'build'
        prepare(incoming, source, build)
        build.mkdir()
        (build / 'stale-staged-source').touch()
        (incoming / 'unchanged').unlink()
        self.metadata(incoming)
        self.assertEqual(prepare(incoming, source, build), 'source files removed or changed type')
        self.assertFalse(build.exists())
        self.assertFalse((source / 'unchanged').exists())
        build.mkdir()
        self.metadata(incoming, key='new environment')
        self.assertEqual(prepare(incoming, source, build), 'build environment changed')
        self.assertFalse(build.exists())

    def test_file_to_symlink_change_discards_stale_build(self):
        incoming = self.incoming()
        self.metadata(incoming)
        source, build = self.root / 'source', self.root / 'build'
        prepare(incoming, source, build)
        build.mkdir()
        (incoming / 'unchanged').unlink()
        (incoming / 'unchanged').symlink_to('main.c')
        self.metadata(incoming)
        self.assertEqual(prepare(incoming, source, build), 'source files removed or changed type')
        self.assertFalse(build.exists())
        self.assertTrue((source / 'unchanged').is_symlink())

    def test_real_ninja_build_reuses_outputs_and_rebuilds_changed_source(self):
        incoming = self.incoming()
        (incoming / 'CMakeLists.txt').write_text(
            'cmake_minimum_required(VERSION 3.16)\nproject(Fixture C)\nadd_executable(fixture main.c)\n')
        self.metadata(incoming)
        source, build = self.root / 'source', self.root / 'build'
        def compile_source():
            prepare(incoming, source, build)
            subprocess.run(['cmake', '-S', str(source), '-B', str(build), '-G', 'Ninja'],
                           check=True, stdout=subprocess.PIPE, stderr=subprocess.STDOUT)
            return subprocess.check_output(['cmake', '--build', str(build)], text=True)
        compile_source()
        executable = build / 'fixture'
        timestamp = executable.stat().st_mtime_ns
        # Registry layers need not preserve nanosecond timestamps; the inner PAX
        # archive must preserve Ninja's recorded source and output timestamps.
        archive = self.root / 'build-cache.tar'
        subprocess.run(['tar', '--format=pax', '-cf', str(archive), '-C', str(self.root), 'source', 'build'], check=True)
        shutil.rmtree(source)
        shutil.rmtree(build)
        subprocess.run(['tar', '-xf', str(archive), '-C', str(self.root)], check=True)
        self.assertEqual(executable.stat().st_mtime_ns, timestamp)
        self.assertIn('no work to do', compile_source())
        self.assertEqual(executable.stat().st_mtime_ns, timestamp)
        (incoming / 'main.c').write_text('int main(void) { return 7; }\n')
        os.utime(incoming / 'main.c', (1, 1))
        self.assertNotIn('no work to do', compile_source())
        self.assertEqual(subprocess.run([str(executable)]).returncode, 7)


if __name__ == '__main__':
    unittest.main()
