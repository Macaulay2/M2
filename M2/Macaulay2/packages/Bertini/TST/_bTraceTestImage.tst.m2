needsPackage "Bertini"

     R=CC[x0,x1,x2]
     F={x0^3-x1^3+x2^3+1}
     sliceH=makeB'Slice(2,{x0,x1,x2,1},NameB'Slice=>"H",
	 B'NumberCoefficients => {{1.31226+1.5445*ii, -.365044+1.05073*ii, 1.05898+1.0507*ii, -.348229+.168593*ii},
	      {.899764+.780578*ii, -.676936-.130762*ii, -.0064778-.407225*ii, .531146-.569493*ii}}                                      )
     makeB'InputFile(storeBM2Files,
    	 AffVariableGroup=>{x0,x1,x2},
    	 ParameterGroup=>{T},
    	 B'Configs=>{{ParameterHomotopy,1}},
    	 B'Functions=>{sliceH},
    	 B'Polynomials=>{"H0+T","H1"}|F)
     runBertini(storeBM2Files,PreparePH2=>true)
     tt=b'TraceTestImage(storeBM2Files)
     assert(#tt==3)
--   Example    
     R=CC[x,y,z]**CC[a,b]
     xyzSub={{x,a},{y,a^2+b},{z,a^2+b^2}}
     sliceH=makeB'Slice(2,{x,y,z,1},NameB'Slice=>"H",B'NumberCoefficients => {{-.340395+.508813*ii, 1.50753+.649406*ii, .660055-.674066*ii, .191868-.0701391*ii}, {.630048+.052575*ii, .176085+.684432*ii, .438564+.0946367*ii, .569508+.010619*ii}})
     makeB'InputFile(storeBM2Files,
    	 AffVariableGroup=>{a,b},
    	 ParameterGroup=>{T},
    	 B'Configs=>{{ParameterHomotopy,1}},
    	 B'Functions=>xyzSub|{sliceH},
    	 B'Polynomials=>{"H1","H0+T"})
     runBertini(storeBM2Files,PreparePH2=>true)
     s=importSolutionsFile(storeBM2Files)
     tt2=b'TraceTestImage(storeBM2Files,MapPoints=>({a,a^2+b,a^2+b^2},{a,b}))
     assert(#tt2===3)
     assert(clean(1e-10,matrix{tt2})==0)
--   Example
     R=CC[x0,x1,x2]
     F={x0^3-x1^3+x2^3+1}
     sliceH=makeB'Slice(2,{x0,x1,x2,1},NameB'Slice=>"H")
     makeB'InputFile(storeBM2Files,
    	 AffVariableGroup=>{x0,x1,x2},
    	 ParameterGroup=>{T},
    	 B'Configs=>{{ParameterHomotopy,1}},
    	 B'Functions=>{sliceH},
    	 B'Polynomials=>{"H0+T","H1"}|F)
     runBertini(storeBM2Files,PreparePH2=>true)
    tt3= b'TraceTestImage(storeBM2Files,StopBeforeTest=>true)--Returns the trace for each parameter homotopy using -gamma, 0, and gamma respectively. 
    assert(#tt3==3)
    assert(#(tt3_0)==3)


