#!/usr/bin/env python3
"""Unit tests for the manifest generator (no M2 installation required)."""
import importlib.util
from pathlib import Path
import unittest
import sys

sys.dont_write_bytecode = True

spec = importlib.util.spec_from_file_location(
    "package_dependencies", Path(__file__).resolve().parents[1] / "package-dependencies.py")
deps = importlib.util.module_from_spec(spec)
spec.loader.exec_module(deps)


class DependenciesTest(unittest.TestCase):
    def test_literal_calls(self):
        text = '''needsPackage "Alpha"; loadPackage("Beta", Reload => true);
                  importFrom_Gamma {"x"}; importFrom("Delta", {"y"});
                  importFrom_"Epsilon" {"z"}; needsPackage "External";'''
        known = {"Alpha", "Beta", "Gamma", "Delta", "Epsilon"}
        self.assertEqual(deps.literal_imports(text, known), known)

    def test_comments_and_strings(self):
        text = '''-- needsPackage "Alpha"
                  -* needsPackage "Beta" -* nested *- *-
                  "loadPackage \\"Gamma\\""; needsPackage variable;'''
        self.assertEqual(deps.literal_imports(text, {"Alpha", "Beta", "Gamma"}), set())

    def test_examples_and_tests(self):
        text = '''TEST /// needsPackage "Alpha" ///
                  doc /// Example
                      loadPackage("Beta")
                  ///'''
        self.assertEqual(deps.literal_imports(text, {"Alpha", "Beta"}), {"Alpha", "Beta"})

    def test_base_dependencies(self):
        graph = {"Style": set(), "FirstPackage": set(), "Macaulay2Doc": set(),
                 "Alpha": {"Beta"}, "Beta": set()}
        deps.check_install_order(graph)
        graph["Macaulay2Doc"].add("Alpha")
        with self.assertRaisesRegex(RuntimeError, "Cyclic installation dependencies"):
            deps.check_install_order(graph)

    def test_missing_bootstrap(self):
        with self.assertRaisesRegex(RuntimeError, "Missing installation prerequisite"):
            deps.check_install_order({"Alpha": set()})


if __name__ == "__main__":
    unittest.main()
