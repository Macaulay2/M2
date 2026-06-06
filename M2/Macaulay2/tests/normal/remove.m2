x = new MutableHashTable from {"a" => 1, "b" => 2, "c" => 3}
assert(remove(x, "a") == 1)
assert (pairs x == {("b", 2), ("c", 3)})
assert(remove(x, "d") === null)
assert (pairs x == {("b", 2), ("c", 3)})

x = new MutableList from {1, 2, 3, 4}
assert(remove(x, 0) == 1)
assert(toList x == {2, 3, 4})
assert(remove(x, -1) == 4)
assert(toList x == {2, 3})
assert(remove(x, 1) == 3)
assert(toList x == {2})
assert(remove(x, 0) == 2)
assert(toList x == {})

filename = temporaryFileName() | ".dbm"
x = openDatabaseOut filename
x#"a" = "foo"
x#"b" = "bar"
x#"c" = "baz"
assert(sort keys x == {"a", "b", "c"})
assert(remove(x, "a") == "foo")
assert(sort keys x == {"b", "c"})
close x
removeFile filename
