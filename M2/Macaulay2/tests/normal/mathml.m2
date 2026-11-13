-- TODO: add more tests

-- used to cause a recursion error
mathML Matrix

assert Equation(mathML(QQ[a..d]), "<mi>ℚ</mi> <mrow><mo>[</mo><mrow><mrow><mi>a</mi><mo>..</mo><mi>d</mi></mrow></mrow><mo>]</mo></mrow>")
