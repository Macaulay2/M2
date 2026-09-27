"""Exercise selective CMake checkout with local Git repositories (no network)."""
import os
from pathlib import Path
import shutil
import subprocess
import tempfile
import unittest

MODULE = Path(__file__).resolve().parents[1] / "submodules.cmake"
REQUIRED = ["submodules/" + name for name in ("memtailor", "mathic", "mathicgb")]
FALLBACKS = ("bdwgc", "flint", "frobby", "givaro", "fflas_ffpack", "googletest")


class SubmoduleTests(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.temp = tempfile.TemporaryDirectory()
        cls.root = Path(cls.temp.name)
        cls.env = dict(os.environ, GIT_ALLOW_PROTOCOL="file",
                       GIT_AUTHOR_NAME="Test", GIT_AUTHOR_EMAIL="test@example.invalid",
                       GIT_COMMITTER_NAME="Test", GIT_COMMITTER_EMAIL="test@example.invalid")
        cls.library = cls.root / "library"
        cls.run_command("git", "init", str(cls.library))
        (cls.library / "README").write_text("pinned source\n")
        cls.run_command("git", "add", ".", cwd=cls.library)
        cls.run_command("git", "commit", "-m", "pinned", cwd=cls.library)
        cls.pinned = cls.run_command("git", "rev-parse", "HEAD", cwd=cls.library).stdout.strip()
        cls.upstream = cls.root / "upstream"
        cls.run_command("git", "init", str(cls.upstream))
        for path in REQUIRED + ["submodules/" + n for n in FALLBACKS]:
            cls.run_command("git", "submodule", "add", str(cls.library), "M2/" + path,
                            cwd=cls.upstream)
        cls.run_command("git", "commit", "-am", "submodules", cwd=cls.upstream)
        # The superproject pins the earlier commit, not the remote's branch tip.
        (cls.library / "README").write_text("newer source\n")
        cls.run_command("git", "commit", "-am", "newer", cwd=cls.library)

    @classmethod
    def tearDownClass(cls):
        cls.temp.cleanup()

    @classmethod
    def run_command(cls, *args, cwd=None, check=True):
        return subprocess.run(args, cwd=cwd, env=cls.env, check=check,
                              text=True, stdout=subprocess.PIPE, stderr=subprocess.STDOUT)

    def setUp(self):
        self.work = Path(tempfile.mkdtemp(dir=self.root))
        self.repo = self.work / "repo"
        self.run_command("git", "clone", str(self.upstream), str(self.repo))
        self.build = self.work / "build"
        self.build.mkdir()

    def configure(self, **options):
        values = dict(CMAKE_SOURCE_DIR=self.repo / "M2", CMAKE_BINARY_DIR=self.build,
                      GIT_FOUND="TRUE", GIT_EXECUTABLE=shutil.which("git"),
                      GIT_SUBMODULE="ON", BUILD_TESTING="ON")
        values.update({name.upper() + "_FOUND": "TRUE" for name in FALLBACKS})
        values["GTEST_FOUND"] = "TRUE"
        values.update(options)
        script = self.work / "check.cmake"
        script.write_text("cmake_minimum_required(VERSION 3.24)\n" +
                          "".join(f'set({key} "{value}")\n' for key, value in values.items()) +
                          "set(GOOGLETEST_FOUND ${GTEST_FOUND})\n" +
                          f'include("{MODULE}")\n' +
                          "".join(f'message(STATUS "fallback {name}=${{_m2_build_{name}}}")\n'
                                  for name in FALLBACKS))
        return self.run_command("cmake", "-P", str(script), check=False)

    def test_system_libraries_are_not_checked_out(self):
        result = self.configure()
        self.assertEqual(result.returncode, 0, result.stdout)
        for path in REQUIRED:
            directory = self.repo / "M2" / path
            self.assertTrue((directory / "README").exists())
            revision = self.run_command("git", "rev-parse", "HEAD", cwd=directory).stdout.strip()
            self.assertEqual(revision, self.pinned)
        self.assertFalse((self.repo / "M2/Macaulay2/editors/emacs").exists())
        for name in FALLBACKS:
            self.assertFalse((self.repo / "M2/submodules" / name / "README").exists())
            self.assertIn(f"fallback {name}=FALSE", result.stdout)

    def test_missing_libraries_are_selected(self):
        for name in FALLBACKS:
            with self.subTest(name=name):
                flag = "GTEST_FOUND" if name == "googletest" else name.upper() + "_FOUND"
                result = self.configure(**{flag: "FALSE"})
                self.assertEqual(result.returncode, 0, result.stdout)
                self.assertIn(f"fallback {name}=TRUE", result.stdout)
                self.assertTrue((self.repo / "M2/submodules" / name / "README").exists())

    def test_no_googletest_without_tests(self):
        result = self.configure(GTEST_FOUND="FALSE", BUILD_TESTING="OFF")
        self.assertEqual(result.returncode, 0, result.stdout)
        self.assertFalse((self.repo / "M2/submodules/googletest/README").exists())

    def test_installed_fallback_keeps_its_sources_and_targets(self):
        stamp = self.build / "libraries/flint/src/build-flint-stamp/build-flint-install"
        stamp.parent.mkdir(parents=True)
        stamp.touch()
        result = self.configure()
        self.assertEqual(result.returncode, 0, result.stdout)
        self.assertIn("fallback flint=TRUE", result.stdout)
        self.assertTrue((self.repo / "M2/submodules/flint/README").exists())

    def test_offline_reports_missing_required_sources(self):
        # Older configurations placed these markers in source-archive submodules.
        for path in REQUIRED:
            (self.repo / "M2" / path / ".nogit").touch()
        result = self.configure(GIT_SUBMODULE="OFF")
        self.assertNotEqual(result.returncode, 0)
        self.assertIn("Required submodule submodules/memtailor is missing", result.stdout)
        self.assertFalse((self.repo / "M2/submodules/memtailor/README").exists())

    def test_populated_archive_needs_no_git(self):
        result = self.configure()
        self.assertEqual(result.returncode, 0, result.stdout)
        archive = self.work / "archive" / "M2"
        shutil.copytree(self.repo / "M2", archive)
        result = self.configure(CMAKE_SOURCE_DIR=archive, GIT_FOUND="FALSE")
        self.assertEqual(result.returncode, 0, result.stdout)
        self.assertNotIn("Updating required submodules", result.stdout)

    def test_failed_update_is_fatal(self):
        result = self.configure(GIT_EXECUTABLE=shutil.which("false"))
        self.assertNotEqual(result.returncode, 0)
        self.assertIn("Could not update required submodules", result.stdout)


if __name__ == "__main__":
    unittest.main()
