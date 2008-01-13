fn = temporaryFileName()
fn << "hi there" << close
fileMode fn
removeFile fn
