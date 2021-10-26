S = QQ[x,y,z]
assert( (OM = ker matrix{{x,y,z}}) === image map(S^{{-1},{-1},{-1}},S^{{-2},{-2},{-2}},{{-y, 0, -z}, {x, -z, 0}, {0, y, x}}) )
F = res OM
F.dd
assert( (F.dd_1) === map(S^{{-2},{-2},{-2}},S^{{-3}},{{z}, {x}, {-y}}) )

end

print generateAssertions ///
S = QQ[x,y,z]
OM = ker matrix{{x,y,z}}
F = res OM
F.dd
F.dd_1
///
