testpkg = temporaryFileName() | ".m2"
testpkg << ///newPackage("TestPackage")
beginDocumentation()
TEST "assert Equation(1 + 1, 2)"
/// << close
loadPackage("TestPackage", FileName => testpkg)
check "TestPackage"
pkgtest = (tests "TestPackage")#0
assert instance(pkgtest, TestInput)
assert Equation(locate pkgtest, (testpkg, 3, 1, 4, 1,,))
assert Equation(toString pkgtest, testpkg | ":3:1-4:1:")
assert Equation(net pkgtest, (testpkg | ":3:1-4:1:")^-1)
expectedCode =  "--" | toAbsolutePath testpkg |
	":4: location of test code" | newline | "assert Equation(1 + 1, 2)"
assert Equation(code pkgtest, expectedCode)
assert Equation(code 0, expectedCode)


assert( (for i from 1 to 2 list { for j from 1 to 2 do { if j == 2 then break 444; } }) === {{444}, {444}} ) -- see issue #2522


f = x -> () -> x
assert ( hash f 1 =!= hash f 2 )
assert ( hash res =!= hash syz )
