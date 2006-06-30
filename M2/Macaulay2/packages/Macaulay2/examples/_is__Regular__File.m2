fn = temporaryFileName()
fn << "hi there" << close
isRegularFile fn
removeFile fn
