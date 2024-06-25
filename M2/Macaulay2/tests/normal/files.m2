-- TODO: add more unit tests for file operations

dir = temporaryFileName()
assert(changeDirectory makeDirectory dir == dir | "/")
assert(currentDirectory() == dir | "/")
if fileExists homeDirectory then (
    assert(changeDirectory() == homeDirectory);
    assert(currentDirectory() == homeDirectory))
removeDirectory dir

assert(baseFilename "/foo/bar/baz" == "baz")
assert(baseFilename "/foo/bar/baz/" == "baz")
assert(baseFilename "foo" == "foo")
assert(baseFilename "foo/" == "foo")
assert(baseFilename "foo////" == "foo")
assert(baseFilename "" == "")
assert(baseFilename "/" == "/")
assert(baseFilename "////" == "/")
