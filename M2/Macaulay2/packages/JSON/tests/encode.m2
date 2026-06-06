-- numbers
assert Equation(toJSON 1, "1")
assert Equation(toJSON 3.14159, "3.14159")
assert Equation(toJSON pi, "3.141592653589793")
assert Equation(toJSON(1/2), ".5")

-- strings
assert Equation(toJSON "Hello, world!", "\"Hello, world!\"")
assert Equation(toJSON "¡pʃɹoʍ 'oʃʃǝH", "\"¡pʃɹoʍ 'oʃʃǝH\"")

-- true/false/null
assert Equation(toJSON true, "true")
assert Equation(toJSON false, "false")
assert Equation(toJSON null, "null")

-- arrays
assert Equation(toJSON {1, 2, 3}, "[1, 2, 3]")
assert Equation(toJSON [1, 2, 3], "[1, 2, 3]")
assert Equation(toJSON <|1, 2, 3|>, "[1, 2, 3]")
assert Equation(toJSON({1, 2, 3}, ValueSeparator => " , "), "[1 , 2 , 3]")
assert Equation(toJSON({1, 2, 3, {4, 5}}, Indent => 0), ///[
1,
2,
3,
[
4,
5
]
]///)
assert Equation(toJSON({1, 2, 3, {4, 5}}, Indent => 2), ///[
  1,
  2,
  3,
  [
    4,
    5
  ]
]///)
assert Equation(toJSON({1, 2, 3, {4, 5}}, Indent => "  "), ///[
  1,
  2,
  3,
  [
    4,
    5
  ]
]///)

assert Equation(toJSON({}, Indent => 2), "[]")
assert Equation(toJSON({{}}, Indent => 2), ///[
  []
]///)

-- objects
assert Equation(toJSON(hashTable{"a" => 1, "b" => 2, "c" => 3}, Sort => true),
    "{\"a\": 1, \"b\": 2, \"c\": 3}")
assert Equation(toJSON(hashTable{"a" => 1, "b" => 2, "c" => 3}, Sort => true,
	ValueSeparator => " , "), "{\"a\": 1 , \"b\": 2 , \"c\": 3}")
assert Equation(toJSON(hashTable{"a" => 1, "b" => 2, "c" => 3}, Sort => true,
	NameSeparator => " : "), "{\"a\" : 1, \"b\" : 2, \"c\" : 3}")

-- Dispatch => Thing
assert Equation(toJSON(1, 2), "[1, 2]")

-- converting other types to strings
assert Equation(toJSON foo, "\"foo\"")
assert Equation(toJSON ZZ, "\"ZZ\"")
assert Equation(toJSON Core, "\"Core\"")

-- hypertext
assert Equation(toJSON HREF("foo.html", "foo"),
    ///"<a href=\"foo.html\">foo</a>"///)

-- control characters
x = ascii(0..31) | "\\\""
assert Equation(fromJSON toJSON x, x)

-- json (alias for toJSON): the alias-by-assignment was exported but no
-- test previously exercised it. Pin agreement on representative shapes.
assert Equation(json 1, "1")
assert Equation(json "Hello, world!", "\"Hello, world!\"")
assert Equation(json {1, 2, 3}, "[1, 2, 3]")
assert Equation(json true, "true")
assert Equation(json null, "null")
assert Equation(json(hashTable{"a" => 1, "b" => 2}, Sort => true),
    "{\"a\": 1, \"b\": 2}")
-- json and toJSON must agree on every input.
assert all({1, 3.14, "foo", true, false, null, {1,2}, hashTable{"k" => 1}},
    y -> json y === toJSON y)

-- IndentLevel option (previously exported but never set in a test).
-- It shifts the starting indent of the pretty-printed output. With
-- Indent set, IndentLevel = 1 prefixes every line with the indent step.
assert Equation(toJSON({1, 2, 3}, Indent => 2, IndentLevel => 1), ///[
    1,
    2,
    3
  ]///)
-- Without Indent, IndentLevel has no observable effect.
assert Equation(toJSON({1, 2, 3}, IndentLevel => 3), "[1, 2, 3]")

-- Round-trip property: fromJSON . toJSON should be the identity on
-- representable values. Previously only the control-chars case at
-- line 76 above tested this.
for v in {0, -1, 1, 42, 3.14, "string with spaces", true, false, null,
          {1, 2, 3}, {}, {{1,2},{3,4}}} do
    assert(fromJSON toJSON v === v)
-- HashTables also round-trip (up to ordering -- compare via pairs sets).
H = hashTable{"a" => 1, "b" => "two", "c" => {3, 4}}
H' = fromJSON toJSON H
assert(set pairs H' === set pairs H)
