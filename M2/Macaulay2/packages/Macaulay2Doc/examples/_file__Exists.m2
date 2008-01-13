fn = temporaryFileName()
fileExists fn
fn << "hi there" << close
fileExists fn
removeFile fn
