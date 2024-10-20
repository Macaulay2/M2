testpkg = temporaryFileName() | ".m2"
testpkg << ///newPackage("TestPackage")
beginDocumentation()
TEST "assert Equation(1 + 1, 2)"
/// << close
loadPackage("TestPackage", FileName => testpkg)
check "TestPackage"
pkgtest = tests(0, "TestPackage")
assert instance(pkgtest, TestInput)
assert Equation(toSequence locate pkgtest, (testpkg, 3, 5, 3, 32, 3, 5))
assert Equation(toString pkgtest, "assert Equation(1 + 1, 2)")
assert Equation(net pkgtest, "TestInput[" | testpkg | ":3:5-3:32]")
beginDocumentation()
expectedCode = DIV{
    new FilePosition from (minimizeFilename testpkg, 3, 5, 3, 32, 3, 5),
    ": --source code:",
    PRE{CODE{"class" => "language-macaulay2",
	    "TEST \"assert Equation(1 + 1, 2)\""}}}
assert BinaryOperation(symbol ===, code pkgtest, expectedCode)
assert BinaryOperation(symbol ===, code 0, expectedCode)

assert( (for i from 1 to 2 list { for j from 1 to 2 do { if j == 2 then break 444; } }) === {{444}, {444}} ) -- see issue #2522


f = x -> () -> x
assert ( hash f 1 =!= hash f 2 )
assert ( hash res =!= hash syz )
