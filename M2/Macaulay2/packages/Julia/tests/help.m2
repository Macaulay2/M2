h = help juliaGetGlobal "sqrt"
assert instance(h, Net)
assert match("sqrt\\(x\\)", toString h)
assert match("Return", toString h)
juliaValue "\"m2 test docstring\"\nm2testdocumented(x) = x"
assert match("m2 test docstring", toString help juliaGetGlobal "m2testdocumented")
