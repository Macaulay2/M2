R = QQ[a..d];
M = image matrix{{a,b,c}}
symmetricAlgebra M
symmetricAlgebra(R^{1,2,3})
symmetricAlgebra(M, Variables=>{x,y,z})
symmetricAlgebra(M, VariableBaseName=>G, MonomialSize=>16)
symmetricAlgebra(M, Degrees=> {7:1})
