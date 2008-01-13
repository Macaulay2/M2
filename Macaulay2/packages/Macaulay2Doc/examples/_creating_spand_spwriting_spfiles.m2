"testfile" << 2^100 << endl << close
value get "testfile"
f = "testfile" << ""
f << "hi" << endl
f << "ho" << endl
f << close
get "testfile"
removeFile "testfile"
