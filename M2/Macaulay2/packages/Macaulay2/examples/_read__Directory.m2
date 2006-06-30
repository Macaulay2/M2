dir = temporaryFileName()
makeDirectory dir
(fn = dir | "/" | "foo") << "hi there" << close
readDirectory dir
removeFile fn
removeDirectory dir
