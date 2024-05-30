-- TODO: add more unit tests for file operations

dir = temporaryFileName()
assert(changeDirectory makeDirectory dir == dir | "/")
assert(currentDirectory() == dir | "/")
assert(changeDirectory() == homeDirectory)
assert(currentDirectory() == homeDirectory)
removeDirectory dir
