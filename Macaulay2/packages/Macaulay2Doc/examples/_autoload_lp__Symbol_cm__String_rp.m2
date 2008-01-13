fn = temporaryFileName()
fn << "f = x -> x+1\n" << close
autoload(f,fn)
code f
f 4
removeFile fn
