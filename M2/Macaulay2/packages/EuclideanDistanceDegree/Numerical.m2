--Projective formulation for intersections with linear spaces
rand := randomValue
(stageOne,stageTwo) = (1,2); 

--Assume ring is a complex inexact field
--G is a subset of F. 

NumericalComputationOptions = new Type of MutableHashTable

parameterKeys = {"StartWeight", "TargetWeight", "StartData", "TargetData", "GammaVector"}
jacKeys= {"JacobianWitnessModel", "JacobianStartSubmodel", "JacobianTargetSubmodel"}
modelKeys = {"Model","WitnessModel", "StartSubmodel", "TargetSubmodel"}
degreeKeys = {"DegreeWitnessModel", "DegreeSubmodel"}

bertiniKeys = {"BertiniStartFiberSolveConfiguration", "BertiniMembershipTestConfiguration", "BertiniSubstitute", "BertiniConstants"}
coordinateKeys = {"PrimalCoordinates", "HomogeneousVariableGroups", "AffineVariableGroups"}
planeKeys = {"Infinity", "PairGeneralHyperplaneList"}
variableKeys = {"JacobianVars", "GradientVars", "ScaleVars", "DataVars", "WeightVars"}

directoryKeys = {"Directory"} 
solutionKeys = {"TrackSolutions"}
outputKeys = {"OutputType", "FinerRestriction"}
fixValues = {"FixedData", "FixedWeight", "FixedSubmodel", "FixedJacobianSubmodel"}

nocKeys = join(parameterKeys,jacKeys,modelKeys,degreeKeys,bertiniKeys,coordinateKeys,planeKeys,
    variableKeys,directoryKeys,solutionKeys,outputKeys,fixValues)

newNumericalComputationOptions = method(Options => { TempDirectory => null, Submodel => {}})
newNumericalComputationOptions(List, List) := o -> (F, G) -> (
    NCO := new NumericalComputationOptions from apply(nocKeys, i -> i=>null);

    -- Temp directory for Bertini input file
    dir := o.TempDirectory ?? temporaryFileName();
    NCO#"Directory" = dir;

    -- Model keys
    NCO#"Model"=F;
    NCO#"WitnessModel"=G;
    L := o.Submodel;
    NCO#"StartSubmodel"=L;
    NCO#"TargetSubmodel"=L;

    -- Degrees of models
    NCO#"DegreeSubmodel"=L/degree/first;
    NCO#"DegreeWitnessModel"=G/degree/first;

    -- Jacobians of models
    NCO#"JacobianWitnessModel"=diff(matrix{gens ring first G}, transpose matrix{G});
    NCO#"JacobianTargetSubmodel"=NCO#"JacobianStartSubmodel"=diff(matrix{gens ring first G}, transpose matrix {L});
    
    -- Data keys (used for other homotopy types)
    numX:=#gens ring first G;
    NCO#"TargetData"=NCO#"StartData"=apply(numX,i->random CC); 
    NCO#"TargetWeight"=apply(numX,i->1);
    NCO#"StartWeight"=apply(numX,i->random CC); 
    --NCO#"GammaVector"=apply(numX-1,i->random CC); 

    -- Variable group keys for Bertini
    scan(bertiniKeys,i->NCO#i={});
    NCO#"HomogeneousVariableGroups"={gens ring first F};
    NCO#"AffineVariableGroups"={};  
    NCO#"PrimalCoordinates"=gens ring first F;  -- This is different when working with a parameterization

    -- Fixed keys (which model to use for which run)
    NCO#"FixedData"="StartData";
    NCO#"FixedWeight"="StartWeight";
    NCO#"FixedSubmodel"="StartSubmodel";
    NCO#"FixedJacobianSubmodel"="JacobianStartSubmodel";

    -- Keys defining variable block names
    NCO#"JacobianVars"="jv";
    NCO#"GradientVars"="gv";
    NCO#"ScaleVars"="sv";
    NCO#"DataVars"="dv";
    NCO#"WeightVars"="wv";

    -- Hyperplanes
    NCO#"Infinity"=null;
    NCO#"PairGeneralHyperplaneList"=null;

    -- Flags
    NCO#"IsProjective"=false;  -- currently not used
    NCO#"OutputType"="Standard";
    NCO#"FinerRestriction"={};

    NCO
)

possibleHT = {"Weight", "Data", "Submodel"}
stageOne=1
stageTwo=2
ht="Weight"
isStageOne=true
isStageTwo=true

homotopyEDDegree = method()
homotopyEDDegree(NumericalComputationOptions, String, Boolean, Boolean) := (NCO, ht, isStageOne, isStageTwo) -> (
    if not member(ht,possibleHT) then error("Argument 1 is not in "|toString possibleHT);
    weightHomotopy := ht === possibleHT_0;
    dataHomotopy := ht === possibleHT_1;
    submodelHomotopy := ht === possibleHT_2;

    if not fileExists NCO#"Directory" then mkdir NCO#"Directory";

    jacL := NCO#(NCO#"FixedJacobianSubmodel");
    L := NCO#(NCO#"FixedSubmodel");
    startWeight := NCO#"StartWeight";
    targetWeight := NCO#"TargetWeight";
    startData := NCO#"StartData";
    targetData := NCO#"TargetData";
    
    -- Symbol names for special variables used in the homotopy machinery/inputs
    (lagMult,numerHB,denomQ,primal,tWeight) := ("lagMult","numerHB","denomQ","primal","tWeight"); 
    (jv,gv,sv) := (NCO#"JacobianVars",NCO#"GradientVars",NCO#"ScaleVars");   
    (dv,wv) := (NCO#"DataVars",NCO#"WeightVars");
    
    -- F is the "model" (target variety). V(G) ∩ V(L) is a CI inside V(F) ∩ V(L).
    (F,G,startL,targetL,jacG) := (NCO#"Model",NCO#"WitnessModel",NCO#"StartSubmodel",NCO#"TargetSubmodel",NCO#"JacobianWitnessModel");

    -- Optional gamma vector for randomization (not used here; keep for extensibility)
    -- randomGamma:=NCO#"GammaVector";

    -- List of primal coordinates (ambient variables)
    xList := NCO#"PrimalCoordinates";

    -- Ensure a hyperplane at infinity section is set; create it if missing.
    if NCO#"Infinity" === null then NCO#"Infinity" = makeB'Section(xList, NameB'Section=>"HX");

    vRing := (numVars,s,kk) -> kk[apply(numVars,i->s|i)];   

    -- Build extensic symbolic ring
    nc := #xList;
    kk0 := QQ; 
    extrinsicRing := kk0[flatten transpose apply(#G+#L,i->apply(nc,j->jv|i|"v"|j))];
    scan({sv,gv,dv,wv}, {#G+#L+1,nc,nc,nc}, (s,numVars) -> extrinsicRing = extrinsicRing ** vRing(numVars,s,kk0));

    symbolicScaleMatrix := basis({0,1,0,0,0},extrinsicRing);  -- the sv's
    symbolicGradient := basis({0,0,1,0,0},extrinsicRing);  -- the gv's
    symbolicJac := genericMatrix(extrinsicRing,#G+#L,nc);

    -- symbolicSystem is the model system: (Scale) * (Gradient || Jacobian) that we want to solve
    -- to be specialized by pairing functions below.
    symbolicSystem := symbolicScaleMatrix*(symbolicGradient||symbolicJac);
    
    jacZero:={};
    pairJac:={};
    pairRowFunction := (M1, M2, hom) -> (
        arg := flatten entries M1;
        val := flatten entries M2;

        idxs := toList(0 .. #arg - 1);
        zeroVals := select(idxs, i -> val_i == 0);
        nonzeroVals := select(idxs, i -> val_i != 0);
        
        jacZero = jacZero | apply(zeroVals, i -> (
            symbolicSystem = sub(symbolicSystem, {arg_i=>0});
            arg_i => 0
        ));
        
        pairJac = pairJac | apply(nonzeroVals, i ->
            makeB'Section(
                {val_i},
                B'NumberCoefficients=>{1},	
                B'Homogenization=>hom,	
                NameB'Section=>arg_i
            )
        )
    );
    
    pairParameterFunction := (p0,p1,r1,r2,sym,bool) -> (
	    if bool then apply(#p0,
            --- for each i we encode an equation like sym_i = p_0_i* r1+p1_i*r2,
            --- x = 2*r1+ 7*r2; Typically, r1 = 1-T and r2 = T.
            i->makeB'Section(
                {p0_i,p1_i},
                B'NumberCoefficients=>{r1,r2},		
                NameB'Section=>sym_i 
        ))
        else apply(#p0,
            --- for each i we encode an equation like sym_i = p0_i * 1,
            --- x = 2*1;  
            i->makeB'Section(
                {p0_i},
                B'NumberCoefficients=>{1},		
                NameB'Section=>sym_i 
        ))
    );	

    -- Pair symbolic vars with their start/target values and set up the homotopy between them
    weight := symbolicWeight := gens vRing(nc,wv,kk0);
    data := symbolicData := gens vRing(nc,dv,kk0);
    pairData := pairParameterFunction(startData,targetData, "(1-TData)","TData", symbolicData, dataHomotopy);	    
    pairWeight := pairParameterFunction(startWeight,targetWeight, "(1-TWeight)","TWeight", symbolicWeight, weightHomotopy);

    kk2 := ring first startWeight;
    topS := kk2[numerHB,denomQ,tWeight];
    (topNumerHB,topDenomQ,topTWeight) := toSequence flatten entries basis({1},topS);
    
    pairGradient := apply(#xList, i -> makeB'Section(
        {xList_i},
	    ContainsPoint=>{data_i},
	    B'NumberCoefficients=>{weight_i},
	    NameB'Section=>symbolicGradient_(0,i),
	    B'Homogenization=>"HX"
    ));

    -- Homogenize and pair the Jacobian
    jacLG := jacL||jacG;
    kk3 := coefficientRing ring first F;
    jacRing := kk3[gens ring first F|{"HX"}];
    HX := last gens jacRing;

    homogLG := homogenize(sub(matrix{L|G},jacRing), HX) // entries // flatten;
    homogJac := matrix apply(numrows jacLG,i->apply(numcols jacLG,j->diff((gens jacRing)_j,homogLG_i)));
    pairRowFunction(symbolicJac, homogJac, "HX");

    -- Pair lagrange multipliers for column homogenization
    (degSubmodel,degWitnessModel) := (NCO#"DegreeSubmodel",NCO#"DegreeWitnessModel");
    degAugJac := {1} | apply(degSubmodel|degWitnessModel, i -> i-1);
    maxDegree := degAugJac//max;
    degRescale := degAugJac/(i -> maxDegree-i);  -- how much to rescale each column
    bLagrangeVars := lagList := apply(#degRescale, i -> "L"|i);
    rescaling := new MutableList from apply(#degRescale, i -> lagList_i);
    
    -- Build generic linear forms H(i,v,j) used to homogenize per-column degrees
    --Homogenize cols by multiplying by a diagonal matrix of linear products on the left. 
    generalHyperplaneList := flatten apply(#degRescale, i -> apply(degRescale_i, j -> (
        hg:="H"|i|"v"|j;  --wants to be both
        rescaling#i = (rescaling#i)|"*"|hg;
        hg
    )));
    
    -- Reuse previously paired hyperplanes if provided; otherwise pair new ones now.
    if NCO#"PairGeneralHyperplaneList"=!=null then pairGeneralHyperplanes:=NCO#"PairGeneralHyperplaneList"
    else ( 
        pairGeneralHyperplanes=apply(#generalHyperplaneList, i->
            makeB'Section(xList|{"HX"},
            NameB'Section=>generalHyperplaneList_i)
        );
        NCO#"PairGeneralHyperplaneList"=pairGeneralHyperplanes
    );

    -- Connect each scale slot with the corresponding homogenizing product.
    pairScale := apply(flatten entries symbolicScaleMatrix, rescaling, (i,j) -> i=>j);
    
    bModelVars := gens ring first F|{"HX"};
    bPoly := homogLG|flatten entries symbolicSystem;
    bConfiguration := {"TrackType"=>0, "PrintPathProgress"=>1000} | (NCO#"BertiniStartFiberSolveConfiguration");    
    BF := pairData|pairWeight|pairJac|pairGradient|pairGeneralHyperplanes|pairScale;

    writeSolveInputFunction := (stage,nif) -> (
        if stage===stageOne then (
            PG:={"adfadfdf"};
            BC:={"TData"=>0,"TWeight"=>0}
        )
        else if stage===stageTwo then (
            BC={};
            if dataHomotopy then PG={"TData"}
            else if weightHomotopy then PG={"TWeight"}
        );

        if stage===stageOne then bConfiguration = bConfiguration | {"UseRegeneration"=>1};
        if stage===stageTwo then bConfiguration = bConfiguration | {"UseRegeneration"=>0};

        makeB'InputFile(
            NCO#"Directory",
            NameB'InputFile=>nif,
            HomVariableGroup=>{bLagrangeVars,bModelVars},
            BertiniInputConfiguration=>bConfiguration|{"ParameterHomotopy"=>stage},
            B'Polynomials=>bPoly,
            B'Constants=>jacZero|BC,
            ParameterGroup=>PG,
            B'Functions=>BF
        )
    );

    --our solution file will always be member points. 
    criticalPointName := "criticalPointFile";
    runSolveInputFunction := (stage,nif) -> (
        writeSolveInputFunction(stage,nif); 

        if stage==stageTwo then(
            writeParameterFile(NCO#"Directory",{0},NameParameterFile=>"start_parameters");
            writeParameterFile(NCO#"Directory",{1},NameParameterFile=>"final_parameters")
        );

        runBertini(NCO#"Directory",NameB'InputFile=>nif);
        readFile(NCO#"Directory");

        if stage==stageOne then(	
            moveB'File(NCO#"Directory","bertini_session.log","stageOne_log",CopyB'File=>true);
            moveB'File(NCO#"Directory","nonsingular_solutions","stageOne_solutions",CopyB'File=>true);
            moveB'File(NCO#"Directory","nonsingular_solutions","start",CopyB'File=>true);
            moveB'File(NCO#"Directory","nonsingular_solutions","member_points",CopyB'File=>true);
            moveB'File(NCO#"Directory","nonsingular_solutions",criticalPointName,CopyB'File=>true)
        );

        if stage==stageTwo then(
            moveB'File(NCO#"Directory","bertini_session.log","stageTwo_log",CopyB'File=>true);
            moveB'File(NCO#"Directory","nonsingular_solutions","stageTwo_solutions",CopyB'File=>true);
            moveB'File(NCO#"Directory","main_data","stageTwo_main_data",CopyB'File=>true);
            --moveB'File(NCO#"Directory","nonsingular_solutions","start",CopyB'File=>true);
            moveB'File(NCO#"Directory","nonsingular_solutions","member_points",CopyB'File=>true);
            moveB'File(NCO#"Directory","nonsingular_solutions",criticalPointName,CopyB'File=>true)
        );	    
    );

    ------------------------------------------------------------------------
    -- (FUNCTIONS 3) Membership tests and incidence matrices.
    --   We create and run two inputs per hypersurface:
    --     - TrackType=1 (positive-dimensional solve)
    --     - TrackType=3 (membership test to decide if points lie on the hypersurface)
    --   Output: an incidence matrix (list of lists) from importIncidenceMatrix.
    ------------------------------------------------------------------------    
    ttOne:=1;
    ttThree:=3;    
    nameFileFunction:=(stage,case,indexCase,hypersurface,theTrackType)->("input_first_MT_"|case|"_"|indexCase|"_"|theTrackType);

    writeIsMembershipFunction := (stage,case,indexCase,hypersurface,theTrackType) -> (
	nif := nameFileFunction(stage,case,indexCase,hypersurface,theTrackType);
    	if stage===stageOne then BC:={"TData"=>0,"TWeight"=>0};
    	if stage===stageTwo then BC={"TData"=>1,"TWeight"=>1};
    	if not member(stage,{1,2}) then error"stage is in {1,2}";

    	makeB'InputFile(
            NCO#"Directory",
    	    NameB'InputFile=>nif,
	        AffVariableGroup=>flatten flatten {bLagrangeVars,bModelVars},
    	    BertiniInputConfiguration=>bConfiguration|{"TrackType"=>theTrackType},
	        B'Polynomials=>{hypersurface},
	        B'Constants=>jacZero|BC,
	        --ParameterGroup=>PG,
    	    B'Functions=>BF
	)
    );

    -- Example test: isMembershipFunction(stageOne, "TT", 0, "x1*x2-x3*x4")
    isMembershipFunction:=(stage,case,indexCase,hypersurface)->(
        --Pos dim solve TrackType=>1
        writeIsMembershipFunction(stage,case,indexCase,hypersurface,ttOne);
        nif:=nameFileFunction(stage,case,indexCase,hypersurface,ttOne);
        runBertini(NCO#"Directory",NameB'InputFile=>nif);
        moveB'File(NCO#"Directory","bertini_session.log","bertini_session_"|nif|".log",CopyB'File => false);

        --MT TrackType=>3
        writeIsMembershipFunction(stage,case,indexCase,hypersurface,ttThree);
        nif=nameFileFunction(stage,case,indexCase,hypersurface,ttThree);
        runBertini(NCO#"Directory",NameB'InputFile=>nif);
        moveB'File(NCO#"Directory","bertini_session.log","bertini_session_"|nif|".log",CopyB'File => false);

        outIM := importIncidenceMatrix(NCO#"Directory");
        outIM
    );
    
    -- Filter points using incidence matrix
    filterSolutionFunction:=(nsf,kp,ns,numCoords)->(
        -- member_points structure assumption: first line header, then groups
        -- of (1 + numCoords) lines per solution. Only selected solutions are kept.
    	firstLine := true;
    	countSol := 0;
    	countLine := 0;
    	groupSize := 1+numCoords;
    	isSelected := null;

    	sf := openOut(NCO#"Directory"|"/"|nsf);
    	scanLineSolutionFunction := (ell) -> (
      	    if firstLine then (
                firstLine=false;
                sf << toString(#kp) << endl
            )
      	    else if countSol < ns then (
                if countLine==0 then isSelected = member(countSol,kp);
                countLine = countLine+1;
                if isSelected then sf << ell << endl;
                if countLine == groupSize then (
                    countLine = 0; 
                    countSol = countSol+1;
                )
            )
        );
        scanLines(scanLineSolutionFunction,(NCO#"Directory")|"/"|"member_points");      
        close sf;
        nsf
    );

    -- positionFilterFunction:
    --   stage   : stage1 or stage2 (affects BC and file naming)
    --   case    : label (e.g. "SaturateH", "IntersectF")
    --   indexCase : index within the polyList
    --   hypersurface : polynomial to test
    --   bin     : "typeA" (drop if ON hypersurface) or "typeB" (keep only if ON)
    positionFilterFunction := (stage,case,indexCase,hypersurface,bin) -> (
    	if bin==="typeA" then isOffHypersurface := (m->(m==={}))
    	else if bin==="typeB" then isOffHypersurface = (m->(m=!={}))
	    else error"last argument is typeA or typeB";

	imMT := isMembershipFunction(stage,case,indexCase,hypersurface);
    	kp := select(#imMT, i -> isOffHypersurface imMT#i);

    	ns:= #imMT;
	(nsf,nc) := ("filterFile", #flatten {bLagrangeVars,bModelVars});

	filterSolutionFunction("filterFile",kp,ns,nc);
    	moveB'File(NCO#"Directory","filterFile","member_points",CopyB'File=>true);
	#kp
    );

    -- stageEDDegBound holds a label and the counts after stage1 and stage2 filtering.
    stageEDDegBound:=new MutableList from {"GEDvUED",null,null};

    -- Drop solutions that lie on offPolyList
    runSaturateUnionFunction:=(polyList,stage)->(
    	(case,bin) := ("SaturateH","typeA");
    	scan(#polyList,i->(
            stageEDDegBound#stage = positionFilterFunction(stageOne,case,i,polyList_i,bin);
        ));	 
    );

    -- Keep only solutions that lie on onPolyList
    runRestrictIntersectionFunction:=(polyList,stage)->(
    	(case, bin) := ("IntersectF", "typeB");
    	scan(#polyList, i -> (
		    stageEDDegBound#stage = positionFilterFunction(stageOne, case, i, polyList_i, bin);
        ));
    );
    
    runComputationStage:=(stage,offPolyList,onPolyList)->(
        if stage==stageOne then runSolveInputFunction(stageOne,"input_first_solve")
        else runSolveInputFunction(stageTwo,"input_second_solve");

        if stage===stageOne or NCO#"OutputType"=!="TestHomotopyConjectureGEDvUED" then runSaturateUnionFunction(offPolyList,stage);
        runRestrictIntersectionFunction(onPolyList,stage);
        if stage===stageTwo and NCO#"FinerRestriction"=!={} then runRestrictIntersectionFunction(NCO#"FinerRestriction",stage);
    ); 

    -- Polynomials that should vanish (onPolyList) or should not vanish (offPolyList).
    -- offPolyList includes:
    --   - HX (hyperplane at infinity)
    --   - "L0" (first Lagrange multiplier) to remove trivial solutions at infinity
    --   - all generic hyperplanes used in rescaling
    offPolyList := {HX,"L0"}|((pairGeneralHyperplanes/(i->i#NameB'Section)));

    -- onPolyList are the homogenized F equations (in the Jacobian ring with HX)
    onPolyList := F/(i->homogenize(sub(i,jacRing),HX));

    -- Execute the requested stages, then return the count for the last executed stage.
    if isStageOne then runComputationStage(stageOne,offPolyList,onPolyList);
    if isStageTwo then runComputationStage(stageTwo,offPolyList,onPolyList);
    if isStageTwo then return stageEDDegBound#2 else if isStageOne then return stageEDDegBound#1
)

numericalOptions = { TempDirectory => null, Submodel => {} }
numericWeightEDDegree = method(Options => numericalOptions)
numericWeightEDDegree(List, List, List, List) := o -> (F, G, data, weight) -> (
    NCO := newNumericalComputationOptions(F, G, o);
    NCO#"StartWeight" = weight;
    NCO#"TargetData" = NCO#"StartData" = data;
    homotopyEDDegree(NCO, "Weight", true, false)
)

numericUnitEDDegree = method(Options => numericalOptions)
numericUnitEDDegree(List, List) := o -> (F, G) -> numericWeightEDDegree(
    F, G,
    apply(#gens ring first F, i->randCC()),
    apply(#gens ring first F, i->1_(ring first F)),
    o
)

numericGenericEDDegree = method(Options => numericalOptions)
numericGenericEDDegree(List, List) := o -> (F, G) -> numericWeightEDDegree(
    F, G,
    apply(#gens ring first F, i->randCC()),
    apply(#gens ring first F, i->randCC()),
    o
)

aEDOptions = { TempDirectory => null, Tolerance => 1e-6, Submodel => {}, SampleGenerator => null }
averageNumericEDDegree = method(Options => aEDOptions)
averageNumericEDDegree(List, List, ZZ) := o -> (F, G, n) -> (
    if o.Submodel =!= null then L := o.Submodel else L = {};
    if o.TempDirectory =!= null then dir := o.TempDirectory else dir = temporaryFileName();
    if o.SampleGenerator =!= null then 
        SampleGenerator := o.SampleGenerator 
    else
        SampleGenerator = () -> apply(#gens ring first F, i -> randomRR());

    -- Initial run
    R := ring first F;
    NCO := newNumericalComputationOptions(F, G, Submodel => o.Submodel, TempDirectory => o.TempDirectory);
    NCO#"StartData" = SampleGenerator();
    NCO#"TargetData" = apply(#gens R, i -> randomRR());
    NCO#"StartWeight" = NCO#"TargetWeight" = apply(#gens R, i -> 1_R);
    NCO#"BertiniStartFiberSolveConfiguration" = {"FinalTol" => 1e-12};
    homotopyEDDegree(NCO, "Data", true, false);

    -- path to sampled data and average real critical points
    avgEDDeg := 0;
    for i to n-1 do (
        NCO#"StartData" = SampleGenerator();
        homotopyEDDegree(NCO, "Data", false, true);
        critPoints := importMainDataFile(NCO#"Directory", NameMainDataFile => "stageTwo_main_data");
        avgEDDeg = avgEDDeg + #realPoints(critPoints, Tolerance => o.Tolerance);
    );
    numeric(avgEDDeg/n)
)

end

restart
loadPackage "EuclideanDistanceDegree"
loadPackage "Probability"
n = 100;
R = QQ[x,y];
F = G = {x^2 + 4*y^2 - 4};
Z = normalDistribution();
sampleGen = () -> apply(#gens R, i -> random Z);
averageNumericEDDegree({F,G}, sampleGen, n)

restart
loadPackage "EuclideanDistanceDegree"
R = QQ[x,y];
F = G = {x^2 + 4*y^2 - 4};
dir = temporaryFileName();
NCO = newNumericalComputationOptions(dir, F, G);
NCO#"BertiniStartFiberSolveConfiguration" = {"FinalTol" => 1e-12};

NCO#"StartData" = NCO#"TargetData" = apply(#gens R, i -> random(RR));
homotopyEDDegree(NCO, "Data", true, false);

NCO#"StartData" = apply(#gens R, i -> random(RR));
homotopyEDDegree(NCO, "Data", false, true);

limitPoints := importMainDataFile(NCO#"Directory", NameMainDataFile => "stageTwo_main_data");

cnt1 = number(limitPoints, P -> (
    coords := drop(drop(P#Coordinates, #G+1), -1);
    HX := last P#Coordinates;
    affineCoords := apply(coords, x -> x / HX);
    all(affineCoords, x -> abs(imaginaryPart x) <= 1e-4)
));
cnt2 = realPoints(limitPoints);

affinePoints = apply(limitPoints, P -> (
    coords = drop(drop(P#Coordinates, #G+1), -1);
    HX = last P#Coordinates;
    apply(coords, x -> x / HX)
));
netList limitPoints

netList readDirectory dir 
scanLines(l -> print(l), dir | "/stageTwo_solutions");

numericGenericEDDegree(F, G, TempDirectory => dir)
averageNumericEDDegree({F,G}, sampleGen, 100);