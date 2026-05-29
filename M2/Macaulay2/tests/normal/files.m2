-- TODO: add more unit tests for file operations

dir = temporaryFileName()
assert(changeDirectory makeDirectory dir == realpath dir)
assert(currentDirectory() == realpath dir)

-- #4389
dir | "/hello.m2" << "hello = () -> \"Hello, world!\"" << endl << close
oldpath = path
path = {"."}
changeDirectory "/"
load "hello.m2"
assert Equation(hello(), "Hello, world!")
path = oldpath

removeFile(dir | "/hello.m2")
removeDirectory dir
assert not isDirectory dir

if fileExists homeDirectory then (
    assert(changeDirectory() == homeDirectory);
    assert(currentDirectory() == homeDirectory))

assert(baseFilename "/foo/bar/baz" == "baz")
assert(baseFilename "/foo/bar/baz/" == "baz")
assert(baseFilename "foo" == "foo")
assert(baseFilename "foo/" == "foo")
assert(baseFilename "foo////" == "foo")
assert(baseFilename "" == "")
assert(baseFilename "/" == "/")
assert(baseFilename "////" == "/")
