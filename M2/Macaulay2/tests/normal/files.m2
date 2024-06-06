-- TODO: add more unit tests for file operations

dir = temporaryFileName()
assert(changeDirectory makeDirectory dir == dir | "/")
assert(currentDirectory() == dir | "/")
if fileExists homeDirectory then (
    assert(changeDirectory() == homeDirectory);
    assert(currentDirectory() == homeDirectory))
removeDirectory dir
