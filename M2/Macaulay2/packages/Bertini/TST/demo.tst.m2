loadPackage("Bertini",Reload =>true)

R= QQ[x]
R = QQ[ x ]
F = {x^2*(x-1)}
A0 = bertiniPosDimSolve(F)


R = QQ[ x ]
F = {x^2*(x-1)}
S0 = bertiniZeroDimSolve(F)
assert(#S0==2)
assert(class first S0===Point)

R = QQ[ x ]
F = {x^2*(x-1)}
S1 = bertiniZeroDimSolve(F,UseRegeneration=>1)
assert(#S1==1)
assert(class first S1===Point)

R = QQ[ x ]
F = {x^2*(x-1)}
S3 = bertiniZeroDimSolve(F,BertiniInputConfiguration => {"UseRegeneration"=>1})
assert(#S3==1)
assert(class first S3===Point)




R = QQ[ x ]
I = ideal {x^2*(x-1)}
S0 = bertiniZeroDimSolve(I)
assert(#S0==2)
assert(class first S0===Point)

R = QQ[ x ]
I = ideal {x^2*(x-1)}
S1 = bertiniZeroDimSolve(I,UseRegeneration=>1)
assert(#S1==1)
assert(class first S1===Point)

R = QQ[ x ]
I = ideal {x^2*(x-1)}
S3 = bertiniZeroDimSolve(I,BertiniInputConfiguration => {"UseRegeneration"=>1})
assert(#S3==1)
assert(class first S3===Point)

end

--TODO: explain in the documentation how BertiniInputConfiguration are written in uppercase.
--TODO: atom editor needs Reload coloring like true
