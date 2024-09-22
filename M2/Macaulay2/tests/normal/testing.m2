testpkg = temporaryFileName() | ".m2"
testpkg << ///newPackage("TestPackage")
beginDocumentation()
TEST "assert Equation(1 + 1, 2)"
/// << close
loadPackage("TestPackage", FileName => testpkg)
check "TestPackage"
pkgtest = tests(0, "TestPackage")
assert instance(pkgtest, TestClosure)
loc = (testpkg, 3, 0, 3, 32, 3, 0)
assert Equation(toSequence locate pkgtest, loc)
beginDocumentation()
expectedCode = net((net new FilePosition from loc) | ///: --source code:
TEST "assert Equation(1 + 1, 2)"///)
printWidth = 0 -- don't wrap
assert Equation(net code pkgtest, expectedCode)
assert Equation(net code 0, expectedCode)

assert( (for i from 1 to 2 list { for j from 1 to 2 do { if j == 2 then break 444; } }) === {{444}, {444}} ) -- see issue #2522


f = x -> () -> x
assert ( hash f 1 =!= hash f 2 )
assert ( hash res =!= hash syz )
