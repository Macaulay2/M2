c = RFunction "c"
matrix' = RFunction "matrix"
array = RFunction "array"
assert Equation(value matrix'(c(1, 2, 3, 11, 12, 13), "nrow" => 2, "ncol" => 3),
    {{1, 2}, {3, 11}, {12, 13}})
assert Equation(value array(c splice(5, 9, 3, 10..15), "dim" => c(3, 3, 2)), {
	{{5, 9, 3}, {10, 11, 12}, {13, 14, 15}},
	{{5, 9, 3}, {10, 11, 12}, {13, 14, 15}}})
A = matrix {{1, 2, 3}, {4, 5, 6}}
assert Equation(A, transpose matrix value RObject A)
