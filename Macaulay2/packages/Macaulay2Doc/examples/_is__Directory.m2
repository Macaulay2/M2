isDirectory "."
fn = temporaryFileName()
fn << "hi there" << close
isDirectory fn
removeFile fn
