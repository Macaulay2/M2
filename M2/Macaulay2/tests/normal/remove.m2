x = new MutableHashTable from {"a" => 1, "b" => 2, "c" => 3}
remove(x, "a")
assert (pairs x == {("b", 2), ("c", 3)})
remove(x, "d")
assert (pairs x == {("b", 2), ("c", 3)})

x = new MutableList from {1, 2, 3, 4}
remove(x, 0)
assert(toList x == {2, 3, 4})
remove(x, -1)
assert(toList x == {2, 3})

filename = temporaryFileName() | ".dbm"
x = openDatabaseOut filename
x#"a" = "foo"
x#"b" = "bar"
x#"c" = "baz"
assert(sort keys x == {"a", "b", "c"})
remove(x, "a")
assert(sort keys x == {"b", "c"})
close x
removeFile filename
