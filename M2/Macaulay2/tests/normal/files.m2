-- TODO: add more unit tests for file operations

dir = temporaryFileName()
assert(changeDirectory makeDirectory dir == realpath dir)
assert(currentDirectory() == realpath dir)
if fileExists homeDirectory then (
    assert(changeDirectory() == homeDirectory);
    assert(currentDirectory() == homeDirectory))
removeDirectory dir
assert not isDirectory dir

assert(baseFilename "/foo/bar/baz" == "baz")
assert(baseFilename "/foo/bar/baz/" == "baz")
assert(baseFilename "foo" == "foo")
assert(baseFilename "foo/" == "foo")
assert(baseFilename "foo////" == "foo")
assert(baseFilename "" == "")
assert(baseFilename "/" == "/")
assert(baseFilename "////" == "/")

missing = temporaryFileName()
(err, out) = capture("realpath " | toExternalString missing)
assert(err and match("realpath failed: No such file or directory", out))

filename = temporaryFileName()
filename << "" << close
filename = realpath filename
fragment = "#L1:C1-L2:C2_L1:C1"
toURL' = value Core#"private dictionary"#"toURL"
assert(toURL' (filename | fragment) == rootURI | urlEncode filename | fragment)
removeFile filename
