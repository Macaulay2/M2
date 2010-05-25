test1 = inp -> (
out1 = "file1";
out1 << "[";
out1 << inp;
out1 << "]" << endl << close;
out2 = "file2";
out2 << "[";
out2 << inp;
out2 << "]" << endl << close;
)

test2 = inp -> (
out1 = "file1";
out2 = "file2";
out1 << "[";
out2 << "[";
out1 << inp;
out2 << inp;
out1 << "]" << endl << close;
out2 << "]" << endl << close;
)

test1 "hi there"
contents1 = get "file1"
contents2 = get "file2"
assert(contents1 == contents2)

test2 "hi there"
contents1 = get "file1"
contents2 = get "file2"
assert(contents1 == contents2)

