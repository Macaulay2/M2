testpkg = temporaryFileName() | ".m2"
testpkg << ///newPackage("TestPackage")
beginDocumentation()
TEST "1 + 1 == 2"
/// << close
loadPackage("TestPackage", FileName => testpkg)
check "TestPackage"
pkgtest = (tests "TestPackage")#0
assert instance(pkgtest, TestInput)
assert Equation(locate pkgtest, (testpkg, 3, 1, 4, 1,,))
assert Equation(toString pkgtest, testpkg | ":3:1-4:1:")
assert Equation(net pkgtest, (testpkg | ":3:1-4:1:")^-1)
assert Equation(code pkgtest, "--" | toAbsolutePath testpkg |
    ":4: location of test code" | newline | "1 + 1 == 2")
