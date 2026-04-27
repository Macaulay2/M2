document { Key => "foo", "hi there", EXAMPLE "a", "ho there", EXAMPLE "b" }
ex = examples "foo"
assert( ex#-2 == "a" )
assert( ex#-1 == "b" )

-- if changing this, make sure that for instance
-- BeginningMacaulay2 still looks okay.
debug Core
expected = "<table class=\"examples\">
  <tr>
    <td>
      <pre><code class=\"language-macaulay2\">M2</code></pre>
    </td>
  </tr>
</table>\n"
result = EXAMPLE PRE "M2"
assert Equation(html result, expected)
