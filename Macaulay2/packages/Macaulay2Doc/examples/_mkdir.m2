p = temporaryFileName() | "/"
mkdir p
isDirectory p
(fn = p | "foo") << "hi there" << close
get fn
removeFile fn
removeDirectory p
