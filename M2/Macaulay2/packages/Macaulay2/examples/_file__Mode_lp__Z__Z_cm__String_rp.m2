fn = temporaryFileName()
fn << "hi there" << close
m = fileMode fn
fileMode(m|7,fn)
fileMode fn
removeFile fn
