--**************************--
-- -*- coding: utf-8 -*-
newPackage(
	"RandomMonomialIdeals",
    	Version => "1.0",
    	Date => "November 15, 2017",
    	Authors => {
	    {
		Name => "Sonja Petrovic",
		Email => "sonja.petrovic@iit.edu",
		HomePage => "http://math.iit.edu/~spetrov1/"
	    },
	    {
		Name => "Despina Stasi",
		Email => "stasdes@iit.edu",
		HomePage => "http://math.iit.edu/~stasdes/"
	    },
	    {
		Name => "Dane Wilburne",
		Email => "dwilburn@hawk.iit.edu",
		HomePage => "http://mypages.iit.edu/~dwilburn/"
	    },
	    {
		Name => "Tanner Zielinski",
		Email => "tzielin1@hawk.iit.edu",
		HomePage => "https://www.linkedin.com/in/tannerzielinski/"
	    },
	    {
		Name => "Daniel Kosmas",
		Email => "dkosmas@hawk.iit.edu",
		HomePage => "https://www.linkedin.com/in/daniel-kosmas-03160988/"
	    },
	    {
		Name => "Parker Joncus", 
		Email => "pjoncus@hawk.iit.edu"
	    },
	    {
		Name => "Richard Osborn", 
		Email => "rosborn@hawk.iit.edu"
	    },
	    {
	    	Name => "Monica Yun", 
	    	Email => "myun1@hawk.iit.edu"
	    },
	    {
	    	Name => "Genevieve Hummel", 
	    	Email => "ghummel1@hawk.iit.edu"
	    }
	},
    	Headline => "A package for generating Erdos-Renyi-type random monomial ideals",
     	PackageImports => { "Depth", "BoijSoederberg", "Serialization" },
    	DebuggingMode => false
    	)

export {
    "randomMonomialSets",
    "randomMonomialSet",
    "idealsFromGeneratingSets",
    "randomMonomialIdeals",
    "Coefficients",
    "VariableName",
    "mingenStats",
    "IncludeZeroIdeals",
    "dimStats",
    "regStats",
    "CMStats",
    "borelFixedStats",
    "ShowTally",
    "degStats",
    "bettiStats",
    "SaveBettis",
    "CountPure",
    "Verbose",
    "pdimStats",
    "Sample",
    "sample",
    "ModelName", "Parameters", "SampleSize", "getData",
    "writeSample",
    "Model",
    "ER",
    "statistics",
    "Mean", "StdDev", "Histogram"
}

--***************************************--
--  Exported methods 	     	     	 --
--***************************************--

Sample = new Type of MutableHashTable
Model = new Type of HashTable

Data = local Data
Generate = local Generate

ER = method(TypicalValue => Model)
ER (ZZ,ZZ,RR) := (n,D,p) -> (
    if n<1 then error "n expected to be a positive integer";
    if p<0.0 or 1.0<p then error "p expected to be a real number between 0.0 and 1.0";
    x := symbol x;
    R := QQ[x_1..x_n];
    tbl := new MutableHashTable;
    tbl.Name = "Erdos-Renyi";
    tbl.Parameters = (n,D,p);
    tbl.Generate = ()->randomMonomialSet(R,D,p);
    new Model from tbl
)

ER (PolynomialRing,ZZ,RR) := (R,D,p) -> (
    if p<0.0 or 1.0<p then error "p expected to be a real number between 0.0 and 1.0";
    tbl := new MutableHashTable;
    tbl.Name = "Erdos-Renyi";
    tbl.Parameters = (R,D,p);
    tbl.Generate = ()->randomMonomialSet(R,D,p);
    new Model from tbl
)

ER (ZZ,ZZ,ZZ) := (n,D,M) -> (
    if n<1 then error "n expected to be a positive integer";
    x := symbol x;
    R := QQ[x_1..x_n];
    tbl := new MutableHashTable;
    tbl.Name = "Erdos-Renyi";
    tbl.Parameters = (n,D,M);
    tbl.Generate = ()->randomMonomialSet(R,D,M);
    new Model from tbl
)

ER (PolynomialRing,ZZ,ZZ) := (R,D,M) -> (
    tbl := new MutableHashTable;
    tbl.Name = "Erdos-Renyi";
    tbl.Parameters = (R,D,M);
    tbl.Generate = ()->randomMonomialSet(R,D,M);
    new Model from tbl
)

ER (ZZ,ZZ,List) := (n,D,pOrM) -> (
    if n<1 then error "n expected to be a positive integer";
    if #pOrM != D then error "pOrM expected to be a list of length D";
    if not all(pOrM, q->instance(q, ZZ)) and not all(pOrM, q->instance(q,RR))
      then error "pOrM must be a list of all integers or all real numbers";
    x := symbol x;
    R := QQ[x_1..x_n];
    tbl := new MutableHashTable;
    tbl.Name = "Erdos-Renyi";
    tbl.Parameters = (n,D,pOrM);
    tbl.Generate = ()->randomMonomialSet(R,D,pOrM);
    new Model from tbl
)

ER (PolynomialRing,ZZ,List) := (R,D,pOrM) -> (
    if #pOrM != D then error "pOrM expected to be a list of length D";
    if not all(pOrM, q->instance(q, ZZ)) and not all(pOrM, q->instance(q,RR))
      then error "pOrM must be a list of all integers or all real numbers";
    if all(pOrM, q->instance(q,RR)) and any(pOrM,q-> q<0.0 or 1.0<q)
      then error "pOrM expected to be a list of real numbers between 0.0 and 1.0";
    tbl := new MutableHashTable;
    tbl.Name = "Erdos-Renyi";
    tbl.Parameters = (R,D,pOrM);
    tbl.Generate = ()->randomMonomialSet(R,D,pOrM);
    new Model from tbl
)

sample = method(TypicalValue => Sample)
sample (Model, ZZ) := (M,N) -> (
    if N<1 then stderr << "warning: N expected to be a positive integer" << endl;
    s:=new Sample;
    s.ModelName = M.Name;
    s.Parameters = M.Parameters;
    s.SampleSize = N;
    s.Data = apply(N,i->M.Generate());
    s
)

sample String := filename -> (
    if not isDirectory filename then error "expected a directory";
    modelFile := realpath filename | "Model.txt";
    model := lines read openIn modelFile;
    s := new Sample;
    s.ModelName = model#1;
    s.Parameters = value toString stack drop(model,{0,1});
    s.SampleSize = value model#0;
    dataFile := realpath filename | "Data.txt";
    s.Data = value read openIn dataFile;
    s
)

getData = method()
getData Sample := s -> (s.Data)

writeSample = method()
writeSample (Sample, String) := (s, filename) -> (
    if fileExists filename then (
	stderr << "warning: filename already exists. Overwriting." << endl;
        if not isDirectory filename then (
	    removeFile filename;
	    mkdir filename;
	);
    ) else (
        mkdir filename;
    );
    realpath filename | "Model.txt" <<
	s.SampleSize << endl <<
	s.ModelName << endl <<
	serialize s.Parameters << close;
    realpath filename | "Data.txt" << serialize s.Data << close; -- Write other data
)


statistics = method(TypicalValue => HashTable)
statistics (Sample, Function) := HashTable => (s,f) -> (
    fData := apply(s.Data,f);
    mean := (sum fData)/s.SampleSize; -- <- should the mean be returned as RR? 
    new HashTable from {Mean=>mean,
     StdDev=>sqrt(sum apply(fData, x-> (mean-x)^2)/s.SampleSize),
     Histogram=>tally fData}
)


randomMonomialSets = method(TypicalValue => List, Options => {Coefficients => QQ,
	                                                        VariableName => "x",
								Strategy => "ER"})
randomMonomialSets (ZZ,ZZ,RR,ZZ) := List => o -> (n,D,p,N) -> (
    if p<0.0 or 1.0<p then error "p expected to be a real number between 0.0 and 1.0";
    randomMonomialSets(n,D,toList(D:p),N,o)
)

randomMonomialSets (PolynomialRing,ZZ,RR,ZZ) := List => o -> (R,D,p,N) -> (
    if p<0.0 or 1.0<p then error "p expected to be a real number between 0.0 and 1.0";
    randomMonomialSets(R,D,toList(D:p),N,o)
)

randomMonomialSets (ZZ,ZZ,ZZ,ZZ) := List => o -> (n,D,M,N) -> (
    if N<1 then stderr << "warning: N expected to be a positive integer" << endl;
    x := toSymbol o.VariableName;
    R := o.Coefficients[x_1..x_n];
    apply(N,i-> randomMonomialSet(R,D,M,o))
)

randomMonomialSets (PolynomialRing,ZZ,ZZ,ZZ) := List => o -> (R,D,M,N) -> (
    if N<1 then stderr << "warning: N expected to be a positive integer" << endl;
    apply(N,i-> randomMonomialSet(R,D,M,o))
)

randomMonomialSets (ZZ,ZZ,List,ZZ) := List => o -> (n,D,pOrM,N) -> (
    if n<1 then error "n expected to be a positive integer";
    if N<1 then stderr << "warning: N expected to be a positive integer" << endl;
    x := toSymbol o.VariableName;
    R := o.Coefficients[x_1..x_n];
    apply(N,i-> randomMonomialSet(R,D,pOrM,o))
)

randomMonomialSets (PolynomialRing,ZZ,List,ZZ) := List => o -> (R,D,pOrM,N) -> (
    if N<1 then stderr << "warning: N expected to be a positive integer" << endl;
    apply(N,i-> randomMonomialSet(R,D,pOrM,o))
)

randomMonomialSet = method(TypicalValue => List, Options => {Coefficients => QQ,
	                                                       VariableName => "x",
							       Strategy => "ER"})
randomMonomialSet (ZZ,ZZ,RR) := List => o -> (n,D,p) -> (
    if p<0.0 or 1.0<p then error "p expected to be a real number between 0.0 and 1.0";
    randomMonomialSet(n,D,toList(D:p),o)
)

randomMonomialSet (PolynomialRing,ZZ,RR) := List => o -> (R,D,p) -> (
    if p<0.0 or 1.0<p then error "p expected to be a real number between 0.0 and 1.0";
    randomMonomialSet(R,D,toList(D:p),o)
)

randomMonomialSet (ZZ,ZZ,ZZ) := List => o -> (n,D,M) -> (
    if n<1 then error "n expected to be a positive integer";
    x := toSymbol o.VariableName;
    R := o.Coefficients[x_1..x_n];
    randomMonomialSet(R,D,M)
)

randomMonomialSet (PolynomialRing,ZZ,ZZ) := List => o -> (R,D,M) -> (
    if M<0 then stderr << "warning: M expected to be a nonnegative integer" << endl;
    if o.Strategy === "Minimal" then error "Minimal not implemented for fixed size ER model";
    allMonomials := flatten flatten apply(toList(1..D),d->entries basis(d,R));
    C := take(random(allMonomials), M);
    if C==={} then {0_R} else C
)

randomMonomialSet (ZZ,ZZ,List) := List => o -> (n,D,pOrM) -> (
    if n<1 then error "n expected to be a positive integer";
    x := toSymbol o.VariableName;
    R := o.Coefficients[x_1..x_n];
    randomMonomialSet(R,D,pOrM,o)
)

randomMonomialSet (PolynomialRing,ZZ,List) := List => o -> (R,D,pOrM) -> (
    if #pOrM != D then error "pOrM expected to be a list of length D";
    if not all(pOrM, q->instance(q, ZZ)) and not all(pOrM, q->instance(q,RR))
      then error "pOrM must be a list of all integers or all real numbers";
    B := {};
    if all(pOrM,q->instance(q,ZZ)) then (
        if o.Strategy === "Minimal" then (
            currentRingM := R;
            apply(D, d->(
                chosen := take(random(flatten entries basis(d+1, currentRingM)), pOrM_d);
                B = flatten append(B, chosen/(i->sub(i, R)));
                currentRingM = currentRingM/promote(ideal(chosen), currentRingM)
            )))
        else
            B = flatten apply(toList(1..D), d->take(random(flatten entries basis(d,R)), pOrM_(d-1)));
    )
    else if all(pOrM,q->instance(q,RR)) then (
        if any(pOrM,q-> q<0.0 or 1.0<q) then error "pOrM expected to be a list of real numbers between 0.0 and 1.0";
        if o.Strategy === "Minimal" then (
            currentRing := R;
            apply(D, d->(
                chosen := select(flatten entries basis(d+1, currentRing), m->random(0.0,1.0)<=pOrM_d);
                B = flatten append(B, chosen/(i->sub(i, R)));
                currentRing = currentRing/promote(ideal(chosen), currentRing)
            )))
        else
            B = flatten apply(toList(1..D),d-> select(flatten entries basis(d,R),m-> random(0.0,1.0)<=pOrM_(d-1)));
	);
    B = apply(B,m->sub(m,R));
    if B==={} then {0_R} else B
)



bettiStats = method(TypicalValue =>Sequence, Options =>{IncludeZeroIdeals=>true, SaveBettis => "", CountPure => false, Verbose => false})
bettiStats List :=  o-> (ideals) -> (
    N := #ideals; Z:=0;
    if o.SaveBettis != "" then (
    	if fileExists o.SaveBettis then (
	    stderr << "warning: filename already exists. Overwriting." << endl;
	    removeFile o.SaveBettis;
	    );
	);
    if not o.IncludeZeroIdeals then (
	(ideals,Z) = extractNonzeroIdeals(ideals);
	if o.Verbose then stdio << "There are "<<N<<" ideals in this sample. Of those, "<<Z<<" are the zero ideal." << endl;
    	if (Z>0 and not o.IncludeZeroIdeals) then stdio <<"The Betti statistics do not include those for the zero ideals."<< endl
	);
    if (o.Verbose and o.IncludeZeroIdeals) then (
	Z = (extractNonzeroIdeals(ideals))_1;
	stdio << "There are "<<N<<" ideals in this sample. Of those, "<<Z<<" are the zero ideal." << endl;
	if Z>0 then stdio <<"The Betti statistics do include those for the zero ideals."<< endl
	);
    -- sum of the betti tables and betti shapes:
    betaShapes := new BettiTally;
    bettisHistogram := {};
    pure := 0; -- count pure Betti tables
    -- add up all the betti tables:
    apply(#ideals,i->(
        resi := betti res ideals_i;
	if o.CountPure then if isPure resi then pure = pure +1;
        if o.SaveBettis != "" then o.SaveBettis << resi << endl;
    	bettisHistogram = append(bettisHistogram, resi);
  	-- let's only keep a 1 in all spots where there was a non-zero Betti number:
	beta1mtx := matrix(resi);
	Rtemp := (ring ideals_i)^1/ideals_i;
	beta1shape := new BettiTally from mat2betti  matrix pack(1+pdim(Rtemp), apply(flatten entries beta1mtx, i-> if i>0 then i=1 else i=0));
	betaShapes = betaShapes + beta1shape
	)
    );
    if o.SaveBettis != "" then o.SaveBettis << close;
    -- compute the average Betti table shape:
    bShapeMean := mat2betti(1/#ideals*(sub(matrix(betaShapes), RR)));
    -- compute the average (entry-wise) Betti table:
    betaSum := sum bettisHistogram;
    bMean := mat2betti(1/#ideals*(sub(matrix(betaSum), RR)));
    -- compute the standard deviation (entry-wise) of the Betti tables:
    bMeanMtx := matrix bMean;
    betaVariance := 1/#ideals * sum apply(bettisHistogram, currentBetti -> (
    	    mtemp := new MutableMatrix from bMeanMtx;
	    currentBettiMatrix := matrix currentBetti;
    	    apply(numrows currentBettiMatrix, i->
		apply(numcols currentBettiMatrix, j->
	    	    (
			--compute  mtemp_(i,j) := (bMean_(i,j) - bCurrent_(i,j)):
			mtemp_(i,j) = mtemp_(i,j) - currentBettiMatrix_j_i
			)
	    	    )
		);
	    --square entries of mtemp, to get (bMean_(i,j) - bCurrent_(i,j))^2:
    	    mtemp = matrix pack(apply( flatten entries mtemp,i->i^2), numcols mtemp)
    	    )
	);
    --    betaStdDev := betaVariance^(1/2); -- <--need to compute entry-wise for the matrix(BettyTally)
    bStdDev := mat2betti matrix pack(apply( flatten entries betaVariance,i->sqrt i), numcols betaVariance);
    if o.CountPure then return (bShapeMean,bMean,bStdDev,pure);
    (bShapeMean,bMean,bStdDev)
    )



degStats = method(TypicalValue =>Sequence, Options =>{ShowTally => false, Verbose => false})
degStats List :=  o-> (ideals) -> (
    N := #ideals;
    deg := 0;
    degHistogram:={};
    apply(#ideals, i->(
        degi := degree ideals_i;
        degHistogram = append(degHistogram, degi)
	)
    );
    ret:=();
    avg:=sub(1/N*(sum degHistogram), RR);
    Ex2:=sub(1/N*(sum apply(elements(tally degHistogram), i->i^2)), RR);
    var:= Ex2 - avg^2;
    stdDev:= var^(1/2);
    if o.ShowTally
    	then(ret=(avg, stdDev,tally degHistogram); return ret;);
    if o.Verbose then (
	numberOfZeroIdeals := (extractNonzeroIdeals(ideals))_1;
	stdio <<  "There are "<<N<<" ideals in this sample. Of those, "<< numberOfZeroIdeals <<" are the zero ideal." << endl;
	if numberOfZeroIdeals>0 then stdio <<"The degree statistics do include those for the zero ideals."<< endl
	);
    ret = (avg, stdDev)
)

--creates a list of monomialIdeal objects from a list of monomial generating sets
idealsFromGeneratingSets =  method(TypicalValue => List, Options => {IncludeZeroIdeals => true, Verbose => false})
idealsFromGeneratingSets(List):= o -> (B) -> (
    N := # B;
    n := numgens ring ideal B#0; -- ring of the first monomial in the first gen set
    ideals := {};
    for i from 0 to #B-1 do {
	ideals = B / (b-> monomialIdeal b);
	};
    (nonzeroIdeals,numberOfZeroIdeals) := extractNonzeroIdeals(ideals);
    if o.Verbose then
     stdio <<"There are "<<#B<<" ideals in this sample. Of those, "<<numberOfZeroIdeals<<" are the zero ideal."<< endl;
    if o.IncludeZeroIdeals then return ideals else return (nonzeroIdeals,numberOfZeroIdeals);
)


dimStats = method(TypicalValue => Sequence, Options => {ShowTally => false, Verbose =>false})
dimStats List := o-> (ideals) -> (
    N := #ideals;
    dims:=0;
    dimsHistogram:={};
    apply(#ideals,i->(
        dimi := dim ideals_i;
    dimsHistogram = append(dimsHistogram, dimi)
    )
    );
    ret:= ();
    avg:=sub(1/N*(sum dimsHistogram), RR);
    Ex2:=sub(1/N*(sum apply(elements(tally dimsHistogram), i->i^2)), RR);
    var:= Ex2 - avg^2;
    stdDev:= var^(1/2);
    if o.ShowTally
         then(ret = (avg, stdDev, tally dimsHistogram), return ret;);
    if o.Verbose then (
	numberOfZeroIdeals := (extractNonzeroIdeals(ideals))_1;
	stdio <<  "There are "<<N<<" ideals in this sample. Of those, "<< numberOfZeroIdeals <<" are the zero ideal." << endl;
	if numberOfZeroIdeals>0 then stdio <<"The Krull dimension statistics do include those for the zero ideals."<< endl
	);
    ret = (avg, stdDev)
)

regStats = method(TypicalValue => Sequence, Options => {ShowTally => false, Verbose => false})
regStats List := o-> (ideals) -> (
    N:=#ideals;
    ideals = extractNonzeroIdeals(ideals);
    ideals = ideals_0;
    reg := 0;
    ret := ();
    regHistogram:={};
    if set {} === set ideals then (
	regHistogram = N:-infinity;
	stdDev := 0;
	if o.ShowTally then(
	    ret=(-infinity, 0, tally regHistogram);
	    return ret;
	    );
	if o.Verbose then
         stdio <<"All ideals in this list are the zero ideal." << endl;
	ret = (-infinity, 0)
    )
    else (
	apply(#ideals,i->(
              regi := regularity ideals_i;
              regHistogram = append(regHistogram, regi)
	     ));
             avg := sub(1/#ideals*(sum regHistogram), RR);
    	     Ex2 := sub((1/(#ideals))*(sum apply(elements(tally regHistogram), i->i^2)), RR);
    	     var := Ex2-avg^2;
    	     stdDev = var^(1/2);
    	     if o.ShowTally
    	        then(ret=(avg, stdDev,tally regHistogram); return ret;);
	     if o.Verbose then (
		 stdio << "There are "<<N<<" ideals in this sample. Of those, "<< toString(N-#ideals) <<" are the zero ideal." << endl;
              	 stdio << "The zero ideals were extracted from the sample before reporting the regularity statistics."<< endl;
		 );
    	     ret = (avg, stdDev)
         )

)

 randomMonomialIdeals = method(TypicalValue => List, Options => {Coefficients => QQ, VariableName => "x", IncludeZeroIdeals => true, Strategy => "ER"})

 randomMonomialIdeals (ZZ,ZZ,List,ZZ) := List => o -> (n,D,pOrM,N) -> (
        B:={};
        if all(pOrM,q->instance(q,RR)) then
	    B=randomMonomialSets(n,D,pOrM,N,Coefficients=>o.Coefficients,VariableName=>o.VariableName,Strategy=>"Minimal")
	else if all(pOrM,q->instance(q,ZZ)) then
	    B=randomMonomialSets(n,D,pOrM,N,Coefficients=>o.Coefficients,VariableName=>o.VariableName, Strategy=>o.Strategy);
	idealsFromGeneratingSets(B,IncludeZeroIdeals=>o.IncludeZeroIdeals)
)
 randomMonomialIdeals (ZZ,ZZ,RR,ZZ) := List => o -> (n,D,p,N) -> (
 	B:=randomMonomialSets(n,D,p,N,Coefficients=>o.Coefficients,VariableName=>o.VariableName,Strategy=>"Minimal");
	idealsFromGeneratingSets(B,IncludeZeroIdeals=>o.IncludeZeroIdeals)
)
 randomMonomialIdeals (ZZ,ZZ,ZZ,ZZ) := List => o -> (n,D,M,N) -> (
 	B:=randomMonomialSets(n,D,M,N,Coefficients=>o.Coefficients,VariableName=>o.VariableName);
	idealsFromGeneratingSets(B,IncludeZeroIdeals=>o.IncludeZeroIdeals)
)

CMStats = method(TypicalValue => QQ, Options =>{Verbose => false})
CMStats (List) := QQ => o -> (ideals) -> (
    cm := 0;
    N := #ideals;
    R := ring(ideals#0);
    for i from 0 to #ideals-1 do (
     if isCM(R/ideals_i) == true then cm = cm + 1 else cm = cm);
     if o.Verbose then (
       numberOfZeroIdeals := (extractNonzeroIdeals(ideals))_1;
       stdio <<"There are "<<N<<" ideals in this sample. Of those, " << numberOfZeroIdeals << " are the zero ideal." << endl;
       if numberOfZeroIdeals>0 then stdio <<"The zero ideals are included in the reported count of Cohen-Macaulay quotient rings."<< endl;
       stdio << cm << " out of " << N << " ideals in the given sample are Cohen-Macaulay." << endl;
       );
   cm/N
)

borelFixedStats = method(TypicalValue =>QQ, Options =>{Verbose => false})
borelFixedStats (List) := QQ => o -> (ideals) -> (
    bor := 0;
    N:=#ideals;
    for i from 0 to #ideals-1 do (
        if isBorel((ideals_i)) == true then bor = bor + 1 else bor = bor);
    if o.Verbose then (
       numberOfZeroIdeals := (extractNonzeroIdeals(ideals))_1;
       stdio <<"There are "<<N<<" ideals in this sample. Of those, " << numberOfZeroIdeals << " are the zero ideal." << endl;
       if numberOfZeroIdeals>0 then stdio <<"The zero ideals are included in the reported count of Borel-fixed monomial ideals."<< endl;
       stdio << bor << " out of " << N << " monomial ideals in the given sample are Borel-fixed." << endl;
       );
    bor/N
)
mingenStats = method(TypicalValue => Sequence, Options => {ShowTally => false, Verbose =>false})
mingenStats (List) := Sequence => o -> (ideals) -> (
    N:=#ideals;
    ideals = extractNonzeroIdeals(ideals);
    numberOfZeroIdeals := ideals_1;
    ideals = ideals_0;
    num := 0;
    numgensHist := {};
    m := 0;
    complexityHist := {};
    ret:=();
    if set {} === set ideals then (
        numgensHist = N:-infinity;
	complexityHist = N:-infinity;
	numStdDev := 0;
	comStdDev := 0;
	if o.ShowTally then(ret=(-infinity, 0, tally numgensHist, -infinity, 0, tally complexityHist); return ret;);
	if o.Verbose then stdio <<"This sample included only zero ideals." << endl;
	ret = (-infinity, 0, -infinity, 0)
    )
    else (
        apply(#ideals,i->(
            mingensi := gens gb ideals_i;
            numgensi := numgens source mingensi;
            mi := max({degrees(mingensi)}#0#1);
	    numgensHist = append(numgensHist, numgensi);
	    complexityHist = append(complexityHist, mi#0)
	    )
        );
    numAvg:=sub((1/(#ideals))*(sum numgensHist), RR);
    comAvg:=sub((1/(#ideals))*(sum complexityHist), RR);
    numEx2:=sub((1/(#ideals))*(sum apply(elements(tally numgensHist), i->i^2)), RR);
    comEx2:=sub((1/(#ideals))*(sum apply(elements(tally complexityHist), i->i^2)), RR);
    numVar:= numEx2 - numAvg^2;
    comVar:= comEx2 - comAvg^2;
    numStdDev= numVar^(1/2);
    comStdDev= comVar^(1/2);
    if o.ShowTally
       then(ret=(numAvg, numStdDev, tally numgensHist, comAvg, comStdDev, tally complexityHist); return ret;);
    if o.Verbose then (
        stdio <<"There are "<<N<<" ideals in this sample. Of those, " << numberOfZeroIdeals << " are the zero ideal." << endl;
	if numberOfZeroIdeals>0 then stdio <<"The statistics returned (mean and standard deviation of # of min gens and mean and standard deviation of degree comlexity) do NOT include those for the zero ideals."<< endl
	);
    ret = (numAvg, numStdDev, comAvg, comStdDev)
  )
)


pdimStats = method(TypicalValue=>Sequence, Options => {ShowTally => false, Verbose => false})
pdimStats (List) := o-> (ideals) -> (
    N:=#ideals;
    pdHist:={};
    R:=ring(ideals_0);
    apply(#ideals,i->
	(
        pdimi := pdim(R^1/ideals_i);
	pdHist = append(pdHist, pdimi)
	)
    );
    ret:=();
    avg:=sub(((1/N)*(sum pdHist)),RR);
    Ex2:=sub(((1/N)*(sum apply(elements(tally pdHist), i->i^2))), RR);
    var:= Ex2 - avg^2;
    stdDev:= var^(1/2);
    if o.ShowTally
         then(ret = (avg, stdDev, tally pdHist), return ret;);
    if o.Verbose then (
	numberOfZeroIdeals := (extractNonzeroIdeals(ideals))_1;
        stdio <<"There are "<<N<<" ideals in this sample. Of those, " << numberOfZeroIdeals << " are the zero ideal." << endl;
	if numberOfZeroIdeals>0 then stdio <<"The projective dimension statistics do include those for the zero ideals."<< endl
	);
    ret=(avg, stdDev)
)

--**********************************--
--  Internal methods	    	    --
--**********************************--

toSymbol = (p) -> (
     if instance(p,Symbol)
         then p
     else if instance(p,String)
         then getSymbol p
     else
         error ("expected a string or symbol, but got: ", toString p))

-- Internal method that takes as input list of ideals and splits out the zero ideals, counting them:
    -- input list of ideals
    -- output a sequence (list of non-zero ideals from the list , the number of zero ideals in the list)
-- (not exported, therefore no need to document)
extractNonzeroIdeals = ( ideals ) -> (
    nonzeroIdeals := select(ideals,i->i != 0);
    numberOfZeroIdeals := # ideals - # nonzeroIdeals;
    -- numberOfZeroIdeals = # positions(B,b-> b#0==0); -- since 0 is only included if the ideal = ideal{}, this is safe too
    return(nonzeroIdeals,numberOfZeroIdeals)
)
-- we may not need the next one for any of the methods in this file; we'll be able to determine this soon. keep for now.
-- Internal method that takes as input list of generating sets and splits out the zero ideals, counting them:
    -- input list of generating sets
    -- output a sequence (list of non-zero ideals from the list , the number of zero ideals in the list)
-- (not exported, therefore no need to document)
extractNonzeroIdealsFromGens = ( generatingSets ) -> (
    nonzeroIdeals := select(generatingSets,i-> i#0 != 0_(ring i#0)); --ideal(0)*ring(i));
    numberOfZeroIdeals := # generatingSets - # nonzeroIdeals;
    -- numberOfZeroIdeals = # positions(B,b-> b#0==0); -- since 0 is only included if the ideal = ideal{}, this is safe too
    return(nonzeroIdeals,numberOfZeroIdeals)
)

-- the following function is needed to fix the Boij-Soederberg "matrix BettiTally" method
-- that we can't use directly for StdDev computation, because we're working over RR not over ZZ:
matrix(BettiTally, ZZ, ZZ) := opts -> (B,lowestDegree, highestDegree) -> (
     c := pdim B + 1;
     r := highestDegree - lowestDegree + 1;
     --M := mutableMatrix(ZZ,r,c);
     M := mutableMatrix(RR,r,c);
     scan(pairs B, (i,v) -> (
	       if v != 0 then
	         M_(i_2-i_0-lowestDegree, i_0) = v;
	       ));
     matrix M
     )




--******************************************--
-- DOCUMENTATION     	       	    	    --
--******************************************--
beginDocumentation()

doc ///
 Key
  RandomMonomialIdeals
 Headline
  A package for generating Erdos-Renyi-type random monomial ideals and variations
 Description
  Text
   {\em RandomMonomialIdeals} is a  package for sampling random monomial ideals from an Erdos-Renyi-type distribution, the graded version of it, and some extensions. 
   It also introduces new objects, Sample and Model, to allow for streamlined handling of random objects and their statistics in Macaulay2. 
   Most of the models implemented are drawn from the paper {\em Random Monomial Ideals} by Jesus A. De Loera, Sonja Petrovic, Lily Silverstein, Despina Stasi, and Dane Wilburne 
   (@HREF"https://arxiv.org/abs/1701.07130"@). 
   
   The main method, @TO randomMonomialSets@, generates a sample of size $N$ from the distribution $\mathcal B(n, D, p)$ of sets of monomials of degree at most $D$, on $n$ variables, where $p$ is the probability of selecting any given monomial: 
  Example
   n=3; D=2; p=0.5; N=4; 
   L = randomMonomialSets(n,D,p,N)
  Text 
   For a formal definition of the model, see Section 1 of @HREF"https://arxiv.org/abs/1701.07130"@. 
   
   This model was inspired by random graphs. To parallel the two variants of the Erdos-Renyi model for graphs - fixing either the probability of an edge or the total number of edges -
   the package also includes the model with fixed {\em number} of monomials to be generated: 
  Example
    n=3; D=2; M=3; N=4;
   L = randomMonomialSets(n,D,M,N)
  Text
   To sample from the {\em graded} model from Section 6 of @HREF"https://arxiv.org/abs/1701.07130"@, simply replace $p$ by a list of $D$ probabilities, one for each degree. 
   In the example below, monomials of degree 1 are not selected (probability = 0), while each monomial of degree 2 is selected with probability 1.
  Example
   n=3; D=2; N=4;
   randomMonomialSets(n,D,{0.0,1.0},N)
  Text
   The package also allows for sampling from the {\em graded} version of the fixed number of monomials model, where we specify the requested number of monomials of each degree. 
   In the example below, we sample random sets of monomials with one monomial of degree 1, zero of degree 2 and three monomials of degree 3.
  Example
   n=3; D=3; N=4;
   randomMonomialSets(n,D,{1,0,3},N)
  Text
   Finally we can request the monomial sets generated by the {\em graded} model with fixed number of monomials, to be minimal generating sets. We can also employ the minimal strategy for a couple of the other versions of the randomMonomialSets method.
  Example
   n=3; D=3; N=4;
   randomMonomialSets(n,D,{1,0,3},N, Strategy=>"Minimal")   
   randomMonomialSets(n,D,{0.0,0.3,0.5},N, Strategy=>"Minimal")
   randomMonomialSets(n,D,0.1,N, Strategy=>"Minimal")   
  Text
   Once a sample is generated, one can compute various statistics regarding algebraic properties of the sample. 
   The methods in the package offer a way to compute and summarize statistics on some of the common properties, such as 
   degree, dimension, projective dimension, Castelnuovo-Mumford regularity, etc. For example, we can use @TO dimStats@ to get the Krull dimension statistics: 
  Example 
   ideals=idealsFromGeneratingSets(L)
   dimStats(ideals,ShowTally=>true)
  Text 
   The first entry in the output of  @TO dimStats@ is the mean Krull dimension of the sample. The second entry is the standard deviation.
   Similarly, one can obtain the mean and standard deviations of the number of minimal generators and degree complexity via @TO mingenStats@,
   and the average Betti table shape, mean Betti table, and its standard deviation via @TO bettiStats@: 
  Example
   mingenStats ideals
   bettiStats ideals
  Text
   For developing other models and computing statistics on objects other than monomial ideals, the package also 
   defines a new Type, @TO Sample@, which allows for a convenient storage of statistics from a sample of algebraic objects and streamlines writing sample data into files.

   For example, below we create a sample of size 10 over the Erdos-Renyi distribution $\mathcal B(n, D, p)$ on monomials over the ring $Q[y,w]$ with $D=4$, and $p=0.5$, and 
   then a sample of size 15 over the graded version of this distribution on monomials over the ring $Z/101[z_1..z_8]$ with $D=2$, and $p={0.25,0.5}$: 
  Example
   sample1 = sample(ER(QQ[y,w],4,0.5),10)
   sample2 = sample(ER(ZZ/101[z_1..z_8],2,{0.25,0.75}),15)
  Text
   The output is a hashtable with 4 entries. To obtain the random sets of monomials (the actual data we are interested in),
   use the command @TO getData@: 
  Example
   keys sample1
   sample2.Parameters
   myData = getData(sample1);
   myData_0
  Text 
   We can also use the @TO Sample@ object to calculate the mean, standard deviation, and tally of the dimension of the ideals generated by the sample: 
  Example
   statistics(sample(ER(CC[z_1..z_8],5,0.1),100), degree@@ideal)
  Text
  
   Most of the methods in this package offer various options, such as selecting a specific ring with which to work, or change variable names, coefficients, etc. Here is a simple example:
  Example
   R=ZZ/101[a..e];
   randomMonomialSets(R,D,p,N)
   randomMonomialSets(n,D,p,N,VariableName=>"t")
  Text
   In some cases, we may want to work directly with the sets of randomly chosen monomials, while at other times it may be more convenient to pass directly to the random monomial ideals.
   Both options induce the same distribution on monomial ideals:
  Example
   randomMonomialSets(3,4,1.0,1)
   monomialIdeal flatten oo
   randomMonomialIdeals(3,4,1.0,1)
 SeeAlso
  randomMonomialSet
  Verbose
  Sample
///

doc ///
 Key
  randomMonomialSets
  (randomMonomialSets,ZZ,ZZ,RR,ZZ)
  (randomMonomialSets,PolynomialRing,ZZ,RR,ZZ)
  (randomMonomialSets,ZZ,ZZ,ZZ,ZZ)
  (randomMonomialSets,PolynomialRing,ZZ,ZZ,ZZ)
  (randomMonomialSets,ZZ,ZZ,List,ZZ)
  (randomMonomialSets,PolynomialRing,ZZ,List,ZZ)
 Headline
  randomly generates lists of monomials in fixed number of variables up to a given degree
 Usage
  randomMonomialSets(ZZ,ZZ,RR,ZZ)
  randomMonomialSets(PolynomialRing,ZZ,RR,ZZ)
  randomMonomialSets(ZZ,ZZ,ZZ,ZZ)
  randomMonomialSets(PolynomialRing,ZZ,ZZ,ZZ)
  randomMonomialSets(ZZ,ZZ,List,ZZ)
  randomMonomialSets(PolynomialRing,ZZ,List,ZZ)
 Inputs
  n: ZZ
    number of variables, OR
  : PolynomialRing
    the ring in which the monomials are to live if $n$ is not specified
  D: ZZ
    maximum degree
  p: RR
     the probability of selecting a monomial, OR
  M: ZZ
     number of monomials in the set, up to the maximum number of monomials in $n$ variables of degree at most $D$  OR
  : List
     of real numbers whose $i$-th entry is the probability of selecting a monomial of degree $i$, OR
  : List
     of integers whose $i$-th entry is the number of monomials of degree $i$ in each set, up to the maximum number of monomials in $n$ variables of degree exactly $i$
  N: ZZ
    number of sets to be generated
 Outputs
  : List
   random generating sets of monomials
 Description
  Text
   randomMonomialSets creates $N$ random sets of monomials of degree $d$, $1\leq d\leq D$, in $n$ variables.
   It does so by calling @TO randomMonomialSet@ $N$ times.
 SeeAlso
  randomMonomialSet
///

doc ///
 Key
  bettiStats
  (bettiStats,List)
 Headline
  statistics on Betti tables of a sample of monomial ideals
 Usage
  bettiStats(List)
 Inputs
  L: List
   of @TO monomialIdeal@s, or any objects to which @TO betti@ @TO res@ can be applied.
 Outputs
  : Sequence
   of BettiTallies, representing the mean Betti table shape and the mean Betti table of the elements in the list {\tt L}.
 Description
  Text
   For a sample of ideals stored as a List, this method computes some basic Betti table statistics of the sample.
   Namely, it computes the average shape of the Betti tables (where 1 is recorded in entry (ij) for each element if beta_{ij} is not zero),
   and it also computes the average Betti table (that is, the table whose (ij) entry is the mean value of beta_{ij} for all ideals in the sample).
  Example
   R = ZZ/101[a..e];
   L={monomialIdeal"a2b,bc", monomialIdeal"ab,bc3",monomialIdeal"ab,ac,bd,be,ae,cd,ce,a3,b3,c3,d3,e3"}
   (meanBettiShape,meanBetti,stdDevBetti) = bettiStats L;
   meanBettiShape
   meanBetti
   stdDevBetti
  Text
   For sample size $N$, the average Betti table {\em shape} simply considers nonzero Betti numbers. It is to be interpreted as follows:
   entry (i,j) encodes the following sum of indicators:
   $\sum_{all ideals} 1_{beta_{ij}>0} / N$; that is,
   the proportion of ideals with a nonzero beta_{ij}.
   Thus an entry of 0.33 means 33% of ideals have a non-zero Betti number there.
  Example
   apply(L,i->betti res i)
   meanBettiShape
  Text
   For sample size $N$, the average Betti table is to be interpreted as follows:
   entry $(i,j)$ encodes  $\sum_{I\in ideals}beta_{ij}(R/I) / N$:
  Example
   apply(L,i->betti res i)
   meanBetti
  Text
   Note that this method will work on a @TO List@ of any objects to which @TO betti@ @TO res@ can be applied.
///

doc ///
  Key
    SaveBettis
    [bettiStats, SaveBettis]
  Headline
    optional input to store all Betti tables computed
  Description
    Text
     The method that computes statistics on Betti tables has an option to save all betti tables to a file.
     This may be useful if betti res computation, called from @TO bettiStats@, takes too long.
    Example
     ZZ/101[a..e];
     L={monomialIdeal"a2b,bc", monomialIdeal"ab,bc3",monomialIdeal"ab,ac,bd,be,ae,cd,ce,a3,b3,c3,d3,e3"}
     bettiStats (L,SaveBettis=>"myBettiDiagrams")
  SeeAlso
    bettiStats
    CountPure
    Verbose
    IncludeZeroIdeals
///

doc ///
  Key
    CountPure
    [bettiStats, CountPure]
  Headline
    optional input to show the number of objects in the list whose Betti tables are pure
  Description
    Text
      Put {\tt CountPure => true} in @TO bettiStats@ to show this output:
    Example
     ZZ/101[a..c];
     L={monomialIdeal"ab,bc", monomialIdeal"ab,bc3"}
     (meanShape,meanBetti,stdevBetti,pure) = bettiStats (L,CountPure=>true);
     pure
  SeeAlso
    bettiStats
    SaveBettis
    Verbose
    IncludeZeroIdeals
///

doc ///
 Key
  degStats
  (degStats,List)
 Headline
  statistics on the degrees of a list of monomial ideals
 Usage
  degStats(List)
 Inputs
  ideals: List
   of @TO monomialIdeal@s or any objects to which @TO degree@ can be applied.
 Outputs
  : Sequence
   whose first entry is the average degree of a list of monomial ideals, second entry is the standard deviation of the degree, and third entry (if option turned on) is the degree tally
 Description
  Text
   degStats computes the degree of R/I for each ideal I in the list and computes the mean and statnadr deviation of the degrees. 
  Example
   R=ZZ/101[a,b,c];
   ideals = {monomialIdeal"a3,b,c2", monomialIdeal"a3,b,ac"}
   degStats(ideals)
  Text
   The following examples use the existing functions @TO randomMonomialSets@ and @TO idealsFromGeneratingSets@ or @TO randomMonomialIdeals@ to automatically generate a list of ideals, rather than creating the list manually:
  Example
   ideals = idealsFromGeneratingSets(randomMonomialSets(4,3,1.0,3))
   degStats(ideals)
  Example
   ideals = randomMonomialIdeals(4,3,1.0,3)
   degStats(ideals)
  Text
   Note that this function can be run with a list of any objects to which @TO degree@ can be applied.
///

doc ///
 Key
  randomMonomialIdeals
  (randomMonomialIdeals,ZZ,ZZ,RR,ZZ)
  (randomMonomialIdeals,ZZ,ZZ,ZZ,ZZ)
  (randomMonomialIdeals,ZZ,ZZ,List,ZZ)
 Headline
  generates random sets of monomial ideals
 Usage
  randomMonomialIdeals(ZZ,ZZ,RR,ZZ)
  randomMonomialIdeals(ZZ,ZZ,ZZ,ZZ)
  randomMonomialIdeals(ZZ,ZZ,List,ZZ)
 Inputs
  n: ZZ
    number of variables
  D: ZZ
    maximum degree
  p: RR
     probability to select a monomial in the ER model, OR
  M: ZZ
     the number of monomials, up to the maximum number of monomials in $n$ variables of degree at most $D$, used to generate each ideal, OR
  : List
     of real numbers whose $i$-th entry is the probability of selecting a monomial of degree $i$, OR
  : List
     of integers whose $i$-th entry is the number of monomials of degree $i$ used to generate each ideal, up to the maximum number of monomials in $n$ variables of degree exactly $i$.
  N: ZZ
    the number of random monomial ideals to be generated
 Outputs
  : List
   list of randomly generated @TO monomialIdeal@, and the number of zero ideals removed, if any
 Description
  Text
   randomMonomialIdeals creates $N$ random monomial ideals, with each monomial generator having degree $d$, $1\leq d\leq D$, in $n$ variables.
   If $p$ is a real number, it generates each of these ideals according to the Erdos-Renyi-type model (see @HREF"https://arxiv.org/abs/1701.07130"@):
   from the list of all monomials of degree $1,\dots,D$ in $n$ variables, it selects each one, independently, with probability $p$.
  Example
   n=2; D=3; p=0.2; N=10;
   randomMonomialIdeals(n,D,p,N)
   randomMonomialIdeals(5,3,0.4,4)
  Text
   Note that this model does not generate the monomial $1$:
  Example
   randomMonomialIdeals(3,2,1.0,1)
  Text
   If $M$ is an integer, then randomMonomialIdeals creates $N$ random monomial ideals of size at most $M$:
   randomly select $M$ monomials from the list of all monomials of degree $1,\dots,D$ in $n$ variables, then generate the ideal from this set.
  Example
   n=8; D=4; M=7; N=3;
   randomMonomialIdeals(n,D,M,N)
  Text
   Note that each generating set of each ideal has at most $M = 7$ monomials. If one monomial divides another monomial that was generated, it will not be in the generating set.

   The input of type @TO List@ controls the number of monomials in the generating set of each degree for the graded ER model.
   Specifically, this input is either a list of real numbers between 0 and 1, inclusive, whose i-th entry is
   the probability of including a monomial of degree i in the monomial set, or it is a list of nonnegative
   integers whose i-th entry is the number of monomials of each degree to include in the monomial set. Consider the following two examples:
   If $p=p_1,\dots,p_D$ is a list of real numbers of length $D$, then randomMonomialSet generates the set utilizing the graded Erdos-Renyi-type model:
   select each monomial of degree $1\le d\le D$, independently, with probability $p_d$.
  Example
   randomMonomialIdeals(2,3,{0.0, 1.0, 1.0},1)
  Text
   Note that the degree-1 monomials were not generated, since the first probability vector entry, $p_1$, is 0.

   If $M=M_1,\dots,M_D$ is a list of integers of length $D$, then @TO randomMonomialIdeals@ creates a list of @TO MonomialIdeal@, where at most $M_d$ monomials are of degree $d$.

  Example
   randomMonomialIdeals(3,3,{1,1,1},1)
  Text
   Observe that there are at most one degree-1, one degree-2, and one degree-3 monomials generating each ideal.
   
   If {\tt Strategy=>"ER"}, the default setting for the graded fixed number of generators version of the method, each set of monomials used to generate a @TO MonomialIdeal@ in our list is not necessarily minimal.
   Else if {\tt Strategy=> "Minimal"} then each monomial ideal in the list is generated by minimal sets of $M_d$ monomials, or maximum number possible,  of total degree $d$, starting from the smallest degree.

  Example
   randomMonomialIdeals(3,3,{1,1,1},1, Strategy=>"Minimal")
  Text
   Observe that there are at most one degree-1, one degree-2, and one degree-3 monomials generating each ideal. Also observe that {\tt Strategy=>"Minimal"} generally gives more generators than the default {\tt Strategy=>"ER"}.
     
 Caveat
  Since the method returns a list of @TO MonomialIdeal@s, only the minimal generating set will be displayed.
  In contrast, @TO randomMonomialSet@ will display the full (not necessarily minimal) generating set produced by the model.
 SeeAlso
   randomMonomialSets
   idealsFromGeneratingSets
///

doc ///
 Key
  randomMonomialSet
  (randomMonomialSet,ZZ,ZZ,RR)
  (randomMonomialSet,PolynomialRing,ZZ,RR)
  (randomMonomialSet,ZZ,ZZ,ZZ)
  (randomMonomialSet,PolynomialRing,ZZ,ZZ)
  (randomMonomialSet,ZZ,ZZ,List)
  (randomMonomialSet,PolynomialRing,ZZ,List)
 Headline
  randomly generates a list of monomials in fixed number of variables up to a given degree
 Usage
  randomMonomialSet(ZZ,ZZ,RR)
  randomMonomialSet(PolynomialRing,ZZ,RR)
  randomMonomialSet(ZZ,ZZ,ZZ)
  randomMonomialSet(PolynomialRing,ZZ,ZZ)
  randomMonomialSet(ZZ,ZZ,List)
  randomMonomialSet(PolynomialRing,ZZ,List)
 Inputs
  n: ZZ
    number of variables, OR
  : PolynomialRing
    the ring in which monomials are to live if $n$ is not specified
  D: ZZ
    maximum degree
  p: RR
     the probability of selecting a monomial, OR
  M: ZZ
     number of monomials in the set, up to the maximum number of monomials in $n$ variables of degree at most $D$  OR
  : List
     of real numbers whose $i$-th entry is the probability of selecting a monomial of degree $i$, OR
  : List
     of integers whose $i$-th entry is the number of monomials of degree $i$ in each set, up to the maximum number of monomials in $n$ variables of degree exactly $i$
 Outputs
  : List
   random set of monomials
 Description
  Text
   randomMonomialSet creates a list of monomials, up to a given degree $d$, $1\leq d\leq D$, in $n$ variables.
   If $p$ is a real number, it generates the set according to the Erdos-Renyi-type model, that is, based on a Binomial distribution:
   from the list of all monomials of degree $1,\dots,D$ in $n$ variables, it selects each one, independently, with probability $p$.
  Example
   n=2; D=3; p=0.2;
   randomMonomialSet(n,D,p)
   randomMonomialSet(3,2,0.6)
  Text
   Note that this model does not generate the monomial $1$:
  Example
   randomMonomialSet(3,2,1.0)
  Text
   If $M$ is an integer, then randomMonomialSet creates a list of monomials of size $M$:
   randomly select $M$ monomials from the list of all monomials of degree $1,\dots,D$ in $n$ variables.
  Example
   n=10; D=5; M=4;
   randomMonomialSet(n,D,M)
  Text
   Note that it returns a set with $M = 4$ monomials.
  Text
   If $M$ is greater than the total number of monomials in $n$ variables of degree at most $D$, then the method will simply return all those monomials (and not $M$ of them). For example:
  Example
   randomMonomialSet(2,2,10)
  Text
   returns 5 monomials in a generating set, and not 10, since there are fewer than 10 monomials to choose from.

   The input of type @TO List@ controls the number of monomials in the generating set of each degree for the graded ER model.
   Specifically, this input is either a list of real numbers between 0 and 1, inclusive, whose i-th entry is
   the probability of including a monomial of degree i in the monomial set, or it is a list of nonnegative
   integers whose i-th entry is the number of monomials of each degree to include in the monomial set. Consider the following two examples:
   If $p=p_1,\dots,p_D$ is a list of real numbers of length $D$, then randomMonomialSet generates the set utilizing the graded Erdos-Renyi-type model:
   select each monomial of degree $1\le d\le D$, independently, with probability $p_d$.
  Example
   randomMonomialSet(2,3,{0.0, 1.0, 1.0})
  Text
   Note that the degree-1 monomials were not generated, since the first probability vector entry is 0.
  Text
   If $M=M_1,\dots,M_D$ is a list of integers of length $D$, then randomMonomialSet creates a list of monomials, where $M_d$ monomials are of degree $d$.
  Example
   randomMonomialSet(2,3,{2,1,1})
  Text
   Observe that there are two degree-1, one degree-2, and one degree-3 monomials.

   If {\tt Strategy=>"ER"}, the default setting for the graded fixed number of generators version of the method, the set of monomials we obtain will not necessarily be minimal.
   Else if {\tt Strategy=> "Minimal"} then the set of monomials constitutes a minimal generating set which is build up of $M_d$ monomials, or the maximum number possible, of total degree $d$, for $d$ from 1 to $D$, starting from $d=1$.

  Example
   randomMonomialSet(3,3,{1,1,1}, Strategy=>"Minimal")
  Text
   Observe that there are at most one degree-1, one degree-2, and one degree-3 monomials.

   Sometimes we are already working in a specific ring and would like the random sets of monomials to live in the same ring:
  Example
   D=3;p=.5; R=ZZ/101[a,b,c];
   randomMonomialSet(R,D,p)
   ring oo_0
 SeeAlso
   randomMonomialSets
///

doc ///
 Key
  idealsFromGeneratingSets
  (idealsFromGeneratingSets, List)
 Headline
  creates ideals from sets of monomials
 Usage
  idealsFromGeneratingSets(List)
 Inputs
  B: List
    of sets of monomials
 Outputs
  : List
    of @TO monomialIdeal@s
 Description
  Text
   idealsFromGeneratingSets takes a list of sets of monomials and converts each set into a monomial ideal.
  Example
   n=4; D=2; p=1.0; N=3;
   B=randomMonomialSets(n,D,p,N); B/print
   idealsFromGeneratingSets(B)
  Text
   In case the {\tt IncludeZeroIdeals} is set to false, the method also counts how many sets are converted to the zero ideal.
 SeeAlso
  randomMonomialIdeals
///

doc ///
 Key
  mingenStats
  (mingenStats, List)
 Headline
  statistics on the minimal generators of a list of monomial ideals: number and degree complexity
 Usage
  mingenStats(List)
 Inputs
  ideals: List
    of @TO monomialIdeal@s or @TO ideal@s
 Outputs
  : Sequence
    with the following entries: the average number of minimal generators, the standard deviation of the number of minimal generators, the average degree complexity, and the standard deviation of the degree complexity.
    If ShowTally is turned on, then the output sequence also includes the tallies of the two numbers following their standard deviation.
 Description
  Text
   mingenStats removes zero ideals from the list of ideals, then calculates the average and the standard deviation for the number of minimal generators and degree complexity of the list of nonzero ideals.
  Example
   n=4; D=3; p={0.0,1.0,0.0}; N=3;
   B=randomMonomialIdeals(n,D,p,N);
   mingenStats(B)
  Text
   If the list given is a list of all zero ideals, mingenStats returns -infinity for the mean number of minimal generators and mean degree complexity.
  Example
   B=randomMonomialIdeals(3,3,0.0,1);
   mingenStats(B)
  Text
   Note that this function can be called on a list of @TO Ideal@ objects instead.
 Caveat
  mingenStats removes zero ideals from the list of ideals before computing the two values.
///

doc ///
  Key
    Coefficients
    [randomMonomialSet, Coefficients]
    [randomMonomialSets, Coefficients]
    [randomMonomialIdeals, Coefficients]
  Headline
    optional input to choose the coefficients of the ambient polynomial ring
  Description
    Text
      Put {\tt Coefficients => r} for a choice of field r as an argument in
      the function @TO randomMonomialSet@ or @TO randomMonomialSets@.
    Example
      n=2; D=3; p=0.2;
      randomMonomialSet(n,D,p)
      ring ideal oo
      randomMonomialSet(n,D,p,Coefficients=>ZZ/101)
      ring ideal oo
  SeeAlso
    randomMonomialSet
    randomMonomialSets
    randomMonomialIdeals
///

doc ///
  Key
    VariableName
    [randomMonomialSet, VariableName]
    [randomMonomialSets, VariableName]
    [randomMonomialIdeals, VariableName]
  Headline
    optional input to choose the variable name for the generated polynomials
  Description
    Text
      Put {\tt VariableName => x} for a choice of string or symbol x as an argument in
      the function @TO randomMonomialSet@, @TO randomMonomialSets@ or @TO randomMonomialIdeals@
    Example
      n=2; D=3; p=0.2;
      randomMonomialSet(n,D,p)
      randomMonomialSet(n,D,p,VariableName => y)
  SeeAlso
    randomMonomialSet
    randomMonomialSets
    randomMonomialIdeals
///

doc ///
  Key
    [randomMonomialSet, Strategy]
    [randomMonomialSets, Strategy]
    [randomMonomialIdeals, Strategy]
  Headline
    optional input to choose the strategy for generating the monomial set
  Description
    Text
      Put {\tt Strategy => "ER"} or {\tt Strategy => "Minimal"} as an argument in the function @TO randomMonomialSet@, @TO randomMonomialSets@, or randomMonomialIdeals.
      "ER" draws random sets of monomials from the ER-type distribution B(n,D,p), while "Minimal" saves computation time by using quotient rings to exclude any non-minimal generators from the list. 
      For @TO randomMonomialSets@ with the number of generators of pre-specified degrees is the input, choosing {\tt Strategy => "Minimal"} will result in larger minimal generating sets.
///

doc ///
 Key
   IncludeZeroIdeals
   [idealsFromGeneratingSets, IncludeZeroIdeals]
   [randomMonomialIdeals, IncludeZeroIdeals]
   [bettiStats, IncludeZeroIdeals]
 Headline
   optional input to choose whether or not zero ideals should be included
 Description
   Text
     When the option is used with the method @TO randomMonomialIdeals@, if {\tt IncludeZeroIdeals => true} (the default), then zero ideals will be included in the list of random monomial ideals.
     If {\tt IncludeZeroIdeals => false}, then any zero ideals produced will be excluded, along with the number of them.
   Example
     n=2;D=2;p=0.0;N=1;
     ideals = randomMonomialIdeals(n,D,p,N)
   Text
     The 0 listed is the zero ideal:
   Example
     ideals_0
   Text
     In the example below, in contrast, the list of ideals returned is empty since the single zero ideal generated is excluded:
   Example
     randomMonomialIdeals(n,D,p,N,IncludeZeroIdeals=>false)
   Text
     The option can also be used with the method @TO bettiStats@.
     If {\tt ideals} contains zero ideals, you may wish to exclude them from the Betti statistics.
     In this case, use the optional input as follows:
   Example
     R=ZZ/101[a..c]
     L={monomialIdeal (a^2*b,b*c), monomialIdeal(a*b,b*c^3),monomialIdeal 0_R};
     apply(L,i->betti res i)
     bettiStats(L,IncludeZeroIdeals=>false)
     bettiStats(L,IncludeZeroIdeals=>false,Verbose=>true)
 SeeAlso
   randomMonomialIdeals
   bettiStats
   idealsFromGeneratingSets
   Verbose
///
doc ///
 Key
  dimStats
  (dimStats,List)
 Headline
  statistics on the Krull dimension of a list of monomial ideals
 Usage
  dimStats(List)

 Inputs
  ideals: List
    of @TO monomialIdeal@s or any objects to which @TO dim@ can be applied.

 Outputs
  : Sequence
   whose first entry is the average Krull dimension of a list of monomial ideals, the second entry is the standard deviation of the Krull dimension, and third entry (if option turned on) is the Krull dimension tally
 Description
  Text
   dimStats finds the average and standard deviation of the Krull dimension for a list of monomial ideals.
  Example
		R=ZZ/101[a,b,c];
		ideals = {monomialIdeal"a3,b,c2", monomialIdeal"a3,b,ac"}
    dimStats(ideals)
  Text
   The following examples use the existing functions @TO randomMonomialSets@ and @TO idealsFromGeneratingSets@ or @TO randomMonomialIdeals@ to automatically generate a list of ideals, rather than creating the list manually:
  Example
   ideals = idealsFromGeneratingSets(randomMonomialSets(4,3,1.0,3))
   dimStats(ideals)
  Example
   ideals = randomMonomialIdeals(4,3,1.0,3)
   dimStats(ideals)
  Example
   ideals = idealsFromGeneratingSets(randomMonomialSets(3,7,0.01,10))
   dimStats(ideals)
  Example
   ideals = randomMonomialIdeals(5,7,0.05,8)
   dimStats(ideals)
  Example
   ideals = idealsFromGeneratingSets(randomMonomialSets(5,7,1,10))
   dimStats(ideals)
  Text
   Note that this function can be run with a list of any objects to which @TO dim@ can be applied.
///


doc ///
 Key
   ShowTally
   [dimStats, ShowTally]
   [mingenStats, ShowTally]
   [degStats, ShowTally]
   [regStats, ShowTally]
   [pdimStats, ShowTally]
 Headline
   optional input to choose if the tally is to be returned
 Description
   Text
     If {\tt ShowTally => false} (the default value), then only the 2 basic statistics - mean and standard deviation - of the function will be returned.
     If {\tt ShowTally => true}, then both the statistics and the tally will be returned.
   Example
	   R=ZZ/101[a,b,c];
		 ideals = {monomialIdeal"a3,b,c2", monomialIdeal"a3,b,ac"}
		 dimStats(ideals)
     mingenStats(ideals)
     degStats(ideals)
     pdimStats(ideals)
   Text
     In the example above, only the statistics are outputted since by default {\tt ShowTally => false}.
   Text
    In order to view the tally, ShowTally must be set to true ({\tt ShowTally => true}) when the function is called:
   Example
     dimStats(ideals,ShowTally=>true)
     mingenStats(ideals,ShowTally=>true)
     degStats(ideals,ShowTally=>true)
     regStats(ideals,ShowTally=>true)
     pdimStats(ideals,ShowTally=>true)
 SeeAlso
   dimStats
   mingenStats
   degStats
   regStats
   pdimStats
///

doc ///
 Key
  pdimStats
  (pdimStats,List)
 Headline
  statistics on projective dimension of a list of monomial ideals
 Usage
  pdimStats(List)
 Inputs
  ideals: List
    of @TO monomialIdeal@s or @TO ideal@s
 Outputs
  : Sequence
   whose first entry is the mean projective dimension, the second entry is the standard deviation of the projective dimension, and third entry (if option turned on) is the projective dimension tally for quotient rings of ideals in the list {\tt ideals}.
 Description
  Text
   pdimStats finds the mean and standard deviation of the projective dimension of elements in the list:
  Example
   R=ZZ/101[a,b,c];
   ideals = {monomialIdeal(a^3,b,c^2), monomialIdeal(a^3,b,a*c)}
   pdimStats(ideals)
  Text
   pdimStats will also output the projective dimension tally using the optional input ShowTally
  Example
   R=ZZ/101[a,b,c];
   ideals = {monomialIdeal(a,c),monomialIdeal(b),monomialIdeal(a^2*b,b^2)}
   pdimStats(ideals, ShowTally=>true)
  Text
   The following examples use the existing function @TO randomMonomialIdeals@ to automatically generate a list of ideals, rather than creating the list manually:
  Example
   ideals = randomMonomialIdeals(4,3,1.0,3)
   pdimStats(ideals)
   ideals = randomMonomialIdeals(4,6,0.01,10)
   pdimStats(ideals)
  Text
   Note that this function can be run with a list of @TO ideal@s as well.
 SeeAlso
   ShowTally
///

doc ///
 Key
  regStats
  (regStats, List)
 Headline
  statistics on the regularities of a list of monomial ideals
 Usage
  regStats(List)
 Inputs
  : List
   of @TO monomialIdeal@s or any object to which @TO regularity@ can be applied
 Outputs
  : Sequence
   whose first entry is the mean regularity of a list of monomial ideals, second entry is the standard deviation of the regularities, and third entry (if option is turned on) is the regularity tally.
 Description
  Text
   regStats removes zero ideals from the list of ideals, then calculates the average and the standard deviation of the regularity of the list of nonzero ideals.
  Example
   R=ZZ/101[a,b,c];
   ideals = {monomialIdeal(a^3,b,c^2), monomialIdeal(a^3,b,a*c)}
   regStats(ideals)
  Text
   If the list given is a list of all zero ideals, regStats returns -infinity for the mean regularity.
  Example
   B=randomMonomialIdeals(3,3,0.0,1)
   regStats(B)
  Text
   Note that this function can be called on a list of @TO Ideal@ objects instead.
 Caveat
  regStats removes zero ideals from the list of ideals before computing the two values.
 SeeAlso
  ShowTally
 ///

doc ///
 Key
  CMStats
  (CMStats, List)
 Headline
  fraction of monomial ideals in the given list whose quotient ring is Cohen-Macaulay
 Usage
  CMStats(List)
 Inputs
  ideals: List
    of @TO monomialIdeal@s or any object to which @TO isCM@ can be applied
 Outputs
  : QQ
   the fraction of Cohen-Macaulay ideals in the list
 Description
  Text
   CMStats simply checks whether the coordinate ring of each ideal in the given sample is arithmetically Cohen-Macaulay, and returns the proportion that are.
  Example
    R=ZZ/101[a,b,c];
    ideals = {monomialIdeal"a3,b,c2", monomialIdeal"a3,b,ac"}
    CMStats(ideals)
  Text
    Note that the method can be run on a list of @TO Ideal@s, too.
///

doc ///
 Key
  borelFixedStats
  (borelFixedStats, List)
 Headline
  fraction of Borel-fixed monomial ideals in the given list
 Usage
  borelFixedStats(List)
 Inputs
  ideals: List
    of @TO monomialIdeal@s or any object to which @TO isBorel@ can be applied
 Outputs
  : QQ
   the fraction of Borel-fixed monomial ideals in the list
 Description
  Text
   borelFixedStats computes the fraction of Borel-fixed ideals in the list of monomial ideals.
  Example
    R=ZZ/101[a,b,c];
    ideals = {monomialIdeal"a3", monomialIdeal"a3,b,ac"}
    borelFixedStats(ideals)
///

doc ///
 Key
   Verbose
   [degStats, Verbose]
   [pdimStats, Verbose]
   [dimStats, Verbose]
   [idealsFromGeneratingSets, Verbose]
   [regStats, Verbose]
   [CMStats, Verbose]
   [borelFixedStats, Verbose]
   [mingenStats, Verbose]
   [bettiStats, Verbose]
 Headline
   optional input to request verbose feedback
 Description
   Text
     Some of the methods that use this option by default exclude zero ideals when computing statistics on a set of ideals.
     Others do not, but the user may wish to know how many ideals are, say, trivially Cohen-Macaulay.
     If {\tt Verbose => true}, then the methods will display an additional informational statement regarding the statistics in question.
     The default value is false.
   Example
     n=3;D=3;p=0.0;N=3;
     ideals = randomMonomialIdeals(n,D,p,N)
     regStats(ideals)
     CMStats(ideals)
   Text
     In the examples above, one may wonder, for example, why 3 out of 3 ideals in the list are Cohen-Macaulay.
     In order to view the additional information, set {\tt Verbose => true}:
   Example
     regStats(ideals, Verbose => true)
     CMStats(ideals, Verbose => true)
   Text
     Other methods that have this option are as follows. Let us look at a nontrivial list of ideals to see more interesting statistics.
   Example
     n=3;D=3;p=0.1;N=3;
     ideals = randomMonomialIdeals(n,D,p,N)
     regStats(ideals, Verbose => true)
     CMStats(ideals, Verbose => true)
     degStats(ideals, Verbose => true)
     dimStats(ideals, Verbose=>true)
     borelFixedStats(ideals, Verbose => true)
     mingenStats(ideals, Verbose=>true)
     bettiStats(ideals, Verbose => true)
     M = randomMonomialSets(n,D,p,N);
     idealsFromGeneratingSets(M, Verbose => true)
 SeeAlso
   degStats
   pdimStats
   dimStats
   idealsFromGeneratingSets
   regStats
   CMStats
   borelFixedStats
   mingenStats
///

doc ///
 Key
  Sample
 Headline
  a type used to store a sample from a statistical model
 Description
  Text 
   This type is used to store a sample from a given @TO Model@. 
   To create a sample, use the @TO sample@ method. 
///

doc ///
 Key
  Model
 Headline
  a type used to store a statistical model and its parameters
 Description
  Text 
   This type is used to store the information about a model: model name, parameters, and generating function.
///

doc ///
 Key
  sample
  (sample,Model,ZZ)
 Headline
  generates a Sample object sampling from the given Model
 Usage
  sample(Model,ZZ)
 Inputs
  M: Model
    to be sampled from
  N: ZZ
    sample size
 Outputs
  : Sample
   a sample of size $N$ from the given Model
 Description
  Text
   The method generates $N$ realizations of the random variable that has the distribution specified by the given Model.
  Example
   s=sample(ER(3,2,0.2),4)
  Text
   One obtains the data from the Sample object (that is, the actual sample in the statistical sense) as follows:
  Example
   getData s
  Text
   The actual sample contains more information than just the data itself:  
  Example 
   peek s
  Text 
   and one can easily obtain sample statistics: 
  Example
   statistics(s,degree@@ideal)
///

doc ///
 Key
  (sample,String)
 Headline
  creates a Sample object from a folder on disk
 Usage
  sample(String)
 Inputs
  FileName: String
    where the sample is stored
 Outputs
  : Sample
   Sample read from disk
 Description
  Text
   A Sample object is read from the specified filename
 SeeAlso
   Sample
   writeSample
///

doc ///
  Key
    ModelName
  Headline
    model name from Sample
  Description
    Text
      Stores the name of the model from which the sample was generated from.
    Example
     (sample(ER(2,2,0.5),2)).ModelName
  SeeAlso
    sample
///

doc ///
  Key
    Parameters
  Headline
    model parameters from Sample
  Description
    Text
      Stores the paramters of the model from which the sample was generated from.
    Example
     (sample(ER(2,2,0.5),2)).Parameters
  SeeAlso
    sample
///

doc ///
  Key
    SampleSize
  Headline
    size of the sample
  Description
    Example
     (sample(ER(1,1,0.0),10)).SampleSize
  SeeAlso
    sample
///

doc ///
 Key
  writeSample
  (writeSample,Sample,String)
 Headline
  write sample to a file
 Usage
  writeSample(Sample,String)
 Inputs
  S: Sample
    to be written to file
  FileName: String
    file name to write sample to
 Description
  Text
   Write a sample to disk. This creates a folder in which the model and data are stored.
   The sample can then be read via the @TO (sample,String)@ function.
 SeeAlso
   (sample,String)
///

doc ///
 Key
  getData
  (getData,Sample)
 Headline
  get the underlying samples
 Usage
  Data = getData(Sample)
 Inputs
  S: Sample
    to extract data from
 Outputs
  Data: List
   of all samples in object
 Description
  Example
   getData sample(ER(3,4,0.1),5)
///

doc ///
 Key
  ER
 Headline
  model for sampling from Erdos-Renyi type distributions on monomials
 Description
  Text
   An Erdos-Renyi type model on monomials is a distribution over sets of monomials.
   When generating a monomial set, each monomial considered is added to the set with a fixed probability.
   The monomials are chosen from a given polynomial ring and are bounded by degree.
  Example
   n=4; D=8; p=0.05;
   myModel = ER(n,D,p)
 SeeAlso
  randomMonomialSets
///

doc ///
 Key
  (ER,ZZ,ZZ,RR)
 Headline
  Erdos-Renyi type distribution on monomials over (n,D,p)
 Usage
  ER(ZZ,ZZ,RR)
 Inputs
  n: ZZ
    number of variables
  D: ZZ
    maximum degree
  p: RR
     the probability of selecting a monomial
 Outputs
  : Model
   Erdos-Renyi type model
 Description
  Text
   Creates an ER-type model for sampling monomials in $n$ variables of degree at most $D$ independently with probability $p$.
  Example
   n=3; D=4; p=0.1;
   myModel = ER(n,D,p)
 SeeAlso
  randomMonomialSets
///

doc ///
 Key
  (ER,PolynomialRing,ZZ,RR)
 Headline
  Erdos-Renyi type distribution on monomials over (R,D,p)
 Usage
  ER(PolynomialRing,ZZ,RR)
 Inputs
  R: PolynomialRing
    the ring in which monomials are chosen from
  D: ZZ
    maximum degree
  p: RR
     the probability of selecting a monomial
 Outputs
  : Model
   Erdos-Renyi type model
 Description
  Text
   Creates an ER-type model for sampling monomials of degree at most $D$ from the ring $R$ independently with probability $p$.
  Example
   D=4; p=0.1;
   myModel = ER(ZZ/101[a..d],D,p)
 SeeAlso
  randomMonomialSets
///

doc ///
 Key
  (ER,ZZ,ZZ,ZZ)
 Headline
  Erdos-Renyi type distribution on monomials over (n,D,M)
 Usage
  ER(ZZ,ZZ,ZZ)
 Inputs
  n: ZZ
    number of variables
  D: ZZ
    maximum degree
  M: ZZ
     number of monomials in the set
 Outputs
  : Model
   Erdos-Renyi type model
 Description
  Text
   Creates an ER-type model for sampling a set of $M$ monomials in $n$ variables of degree at most $D$.
  Example
   n=3; D=4; M=5;
   myModel = ER(n,D,M)
 SeeAlso
  randomMonomialSets
///

doc ///
 Key
  (ER,PolynomialRing,ZZ,ZZ)
 Headline
  Erdos-Renyi type distribution on monomials over (R,D,M)
 Usage
  ER(PolynomialRing,ZZ,ZZ)
 Inputs
  R: PolynomialRing
    the ring in which monomials are chosen from
  D: ZZ
    maximum degree
  M: ZZ
     number of monomials in the set
 Outputs
  : Model
   Erdos-Renyi type model
 Description
  Text
   Creates an ER-type model for sampling a set of $M$ monomials of degree at most $D$ from the ring $R$.
  Example
   D=4; M=5;
   myModel = ER(ZZ/101[a..d],4,5)
 SeeAlso
  randomMonomialSets
///

doc ///
 Key
  (ER,ZZ,ZZ,List)
 Headline
  Graded Erdos-Renyi type distribution on monomials over (n,D,L)
 Usage
  ER(ZZ,ZZ,List)
 Inputs
  n: ZZ
    number of variables
  D: ZZ
    maximum degree
  L: List 
     of real numbers whose i-th entry is the probability of selecing a monomial of degree i, 
     or of integers whose i-th entry is the number of monomials of degree i in each set
 Outputs
  : Model
   Erdos-Renyi type model
 Description
  Text
   Creates a graded ER-type model for sampling monomials in $n$ variables of degree at most $D$.
  Example
   n1=3; D1=4; L1={0.1,0.2,0.3,0.4};
   n2=3; D2=4; L2={1,2,2,1};
   myModel1 = ER(n1,D1,L1)
   myModel2 = ER(n2,D2,L2)
 SeeAlso
  randomMonomialSets
///

doc ///
 Key
  (ER,PolynomialRing,ZZ,List)
 Headline
  Graded Erdos-Renyi type distribution on monomials over (R,D,L)
 Usage
  ER(PolynomialRing,ZZ,List)
 Inputs
  R: PolynomialRing
    the ring in which monomials are chosen from
  D: ZZ
    maximum degree
  L: List 
     of real numbers whose i-th entry is the probability of selecing a monomial of degree i, 
     or of integers whose i-th entry is the number of monomials of degree i in each set
 Outputs
  : Model
   Erdos-Renyi type model
 Description
  Text
   Creates a graded ER-type model for sampling monomials of degree at most $D$ from the ring $R$.
  Example
   D1=4; L1={0.1,0.2,0.3,0.4};
   D2=4; L2={1,2,2,1};
   myModel1 = ER(ZZ/101[a..d],D1,L1)
   myModel2 = ER(ZZ/101[a..d],D2,L2)
 SeeAlso
  randomMonomialSets
///


doc ///
 Key
  statistics
  (statistics,Sample,Function)
 Headline
  generate statistics for a sample
 Usage
  statistics(Sample,Function)
 Inputs
  S: Sample
    Sample to run statistics on
  f: Function
    function over the data
 Outputs
  : HashTable
   containing statistics for the sample
 Description
  Text
   Generates statistics for the sample via the given function. The function is applied
   to each element in the sample, and its result is then used to calculate a mean, 
   standard deviation, and histogram.
  Example
   s=sample(ER(6,3,0.2),15);
   statistics(s, degree@@ideal)
///

doc ///
  Key
    Mean
  Headline
    return value for statistics
  Description
    Text
      Get the mean from the hash table returned by @TO statistics@.
///

doc ///
  Key
    StdDev
  Headline
    return value for statistics
  Description
    Text
      Get the standard deviation from the hash table returned by @TO statistics@.
///

doc ///
  Key
    Histogram
  Headline
    return value for statistics
  Description
    Text
      Get the histogram from the hash table returned by @TO statistics@.
///

--******************************************--
-- TESTS     	     	       	    	    --
--******************************************--

--************************--
--  randomMonomialSets  --
--************************--

TEST ///
    -- Check there are N samples
    N=10;
    n=3; D=2; p=0.5;
    assert (N==#randomMonomialSets(n,D,p,N))
    N=13;
    n=5; D=3; p={0.5,0.25,0.3};
    assert (N==#randomMonomialSets(n,D,p,N))
    N=10;
    n=3; D=2; M=10;
    assert (N==#randomMonomialSets(n,D,M,N))
    N=7;
    n=4; D=3; M={3,3,3};
    assert (N==#randomMonomialSets(n,D,M,N))
///

TEST ///
    -- Check multiple samples agree
    n=4; D=3;
    L = randomMonomialSets(n,D,1.0,3);
    assert (set L#0===set L#1)
    assert (set L#0===set L#2)

///

TEST ///
    --Check monomials are in the same ring
    n = 4; D = 3;
    L = randomMonomialSets(n,D,1.0,3);
    assert(ring(L#0#0)===ring(L#1#0))
    assert(ring(L#1#1)===ring(L#1#2))
    assert(ring(L#2#0)===ring(L#1#2))
    L = randomMonomialSets(n,6,{1,2,3,4,5,6},3, Strategy=>"Minimal");
    assert(ring(L#0#0)===ring(L#1#0))
    assert(ring(L#1#1)===ring(L#1#2))
    assert(ring(L#2#0)===ring(L#1#2))
    L = randomMonomialSets(n,6,{0.2,0.2,0.2,0.2,0.2,0.2},3, Strategy=>"Minimal");
    assert(ring(L#0#0)===ring(L#1#0))
    assert(ring(L#1#1)===ring(L#1#2))
    assert(ring(L#2#0)===ring(L#1#2))
    L = randomMonomialSets(n,6,{0.2,0.2,0.2,0.2,0.2,0.2},3);
    assert(ring(L#0#0)===ring(L#1#0))
    assert(ring(L#1#1)===ring(L#1#2))
    assert(ring(L#2#0)===ring(L#1#2))
///

--***********************--
--  randomMonomialSet  --
--***********************--

TEST ///
    --Check monomials are in the same ring
    n = 4; D = 3;
    L = randomMonomialSet(n,D,1.0);
    assert(ring(L#0)===ring(L#1))
    assert(ring(L#2)===ring(L#3))
///

TEST ///
    -- Check no terms are chosen for a probability of 0

    assert (0==(randomMonomialSet(5,5,0.0))#0)
    assert (0==(randomMonomialSet(5,4,toList(4:0.0)))#0)
    assert (0==(randomMonomialSet(5,4,0.0, Strategy=>"Minimal"))#0)
    assert (0==(randomMonomialSet(5,4,toList(4:0.0), Strategy=>"Minimal"))#0)
    assert (0==(randomMonomialSet(5,4,0))#0)
    assert (0==(randomMonomialSet(5,4,toList(4:0)))#0)
    assert (0==(randomMonomialSet(5,4,toList(4:0), Strategy=>"Minimal"))#0)

///

TEST ///
    -- Check all possible values are outputted with a probability of 1
    n=4; D=3;
    assert (product(toList((D+1)..D+n))/n!-1==#randomMonomialSet(n,D,1.0))
    assert (product(toList((D+1)..D+n))/n!-1==#randomMonomialSet(n,D,{1.0,1.0,1.0}))
    n=6; D=2;
    assert (product(toList((D+1)..D+n))/n!-1==#randomMonomialSet(n,D,1.0))
    assert (product(toList((D+1)..D+n))/n!-1==#randomMonomialSet(n,D,{1.0,1.0}))
    n=4;D=5;
    assert (# flatten entries basis (1, QQ[x_1..x_n])==#randomMonomialSet(n,D,1.0, Strategy=>"Minimal"))
    assert (# flatten entries basis (2, QQ[x_1..x_n])==#randomMonomialSet(n,D,{0.0,1.0,1.0,1.0,1.0}, Strategy=>"Minimal"))
    assert (# flatten entries basis (3, QQ[x_1..x_n])==#randomMonomialSet(n,D,{0.0,0.0,1.0,1.0,1.0}, Strategy=>"Minimal"))
    assert (# flatten entries basis (4, QQ[x_1..x_n])==#randomMonomialSet(n,D,{0.0,0.0,0.0,1.0,1.0}, Strategy=>"Minimal"))
    assert (# flatten entries basis (5, QQ[x_1..x_n])==#randomMonomialSet(n,D,{0.0,0.0,0.0,0.0,1.0}, Strategy=>"Minimal"))
    numMons:=binomial(n+D-1,D);
    assert (#randomMonomialSet(n,D,append(toList(D-1:0),2*numMons),Strategy=>"Minimal")==numMons)
    assert (#randomMonomialSet(4,5,{2,0,0,5,0},Strategy=>"Minimal")==7)
    assert (#randomMonomialSet(4,5,{2,0,0,8,0},Strategy=>"Minimal")==7)
    assert (#randomMonomialSet(4,5,{2,1,0,10,0},Strategy=>"Minimal")==5)
    -- Check that the precise number of monomials are generated
    assert (#randomMonomialSet(4,5,{1,1,1,1,1},Strategy=>"Minimal")==5)
    assert (#randomMonomialSet(5,10,{1,1,1,1,1,1,1,1,1,1},Strategy=>"Minimal")==10)
    
///

TEST ///
    -- Check every monomial is generated
    L=randomMonomialSet(2,3,1.0)
    R=ring(L#0)
    assert(set L===set {R_0,R_1,R_0^2,R_0*R_1,R_1^2,R_0^3,R_0^2*R_1,R_0*R_1^2,R_1^3})
    L=randomMonomialSet(2,3,9)
    R=ring(L#0)
    assert(set L===set {R_0,R_1,R_0^2,R_0*R_1,R_1^2,R_0^3,R_0^2*R_1,R_0*R_1^2,R_1^3})
    L=randomMonomialSet(3,3,{0.0,1.0,0.0})
    R=ring(L#0)
    assert(set L===set {R_0^2,R_0*R_1,R_1^2,R_0*R_2,R_1*R_2,R_2^2})
    L=randomMonomialSet(3,3,1.0, Strategy=>"Minimal");
    R=ring(L#0);
    assert(set L===set {R_0, R_1, R_2})
    L=randomMonomialSet(2,3,{2,3,4})
    R=ring(L#0)
    assert(set L===set {R_0,R_1,R_0^2,R_0*R_1,R_1^2,R_0^3,R_0^2*R_1,R_0*R_1^2,R_1^3})
    L=randomMonomialSet(3,3,{0.0,1.0,1.0}, Strategy=>"Minimal");
    R=ring(L#0);
    assert(set L===set {R_0^2,R_0*R_1,R_1^2,R_0*R_2,R_1*R_2,R_2^2})
    L=randomMonomialSet(3,3,{0.0,0.0,1.0}, Strategy=>"Minimal");
    R=ring(L#0);
    assert(set L===set {R_0^3,R_0^2*R_1,R_0^2*R_2,R_0*R_1*R_2,R_1^3,R_0*R_1^2,R_1^2*R_2,R_0*R_2^2,R_1*R_2^2,R_2^3})
///

TEST ///
    -- Check max degree of monomial less than or equal to D
    n=10; D=5;
    assert(D==max(apply(randomMonomialSet(n,D,1.0),m->first degree m)))
    assert(D==max(apply(randomMonomialSet(n,D,toList(D:1.0)),m->first degree m)))
    M=lift(product(toList((D+1)..(D+n)))/n!-1,ZZ);
    assert(D==max(apply(randomMonomialSet(n,D,M),m->first degree m)))
    assert(D==max(apply((randomMonomialSet(n,D,{0.0,0.0,0.0,0.0,1.0}, Strategy=>"Minimal"),m->first degree m))))
    n=4; D=7;
    assert(D==max(apply(randomMonomialSet(n,D,1.0),m->first degree m)))
    assert(D==max(apply(randomMonomialSet(n,D,toList(D:1.0)),m->first degree m)))
    M=lift(product(toList((D+1)..(D+n)))/n!-1,ZZ);
    assert(D==max(apply(randomMonomialSet(n,D,M),m->first degree m)))
    assert(D==max(apply(randomMonomialSet(n,D,toList(D:1)), m->first degree m)))
///

TEST ///
    -- Check min degree of monomial greater than or equal to 1
    n=8; D=6;
    assert(1==min(apply(randomMonomialSet(n,D,1.0),m->first degree m)))
    assert(1==min(apply(randomMonomialSet(n,D,toList(D:1.0)),m->first degree m)))
    M=lift(product(toList((D+1)..(D+n)))/n!-1,ZZ);
    assert(1==min(apply(randomMonomialSet(n,D,M),m->first degree m)))
    n=3; D=5;
    assert(1==min(apply(randomMonomialSet(n,D,1.0),m->first degree m)))
    assert(1==min(apply(randomMonomialSet(n,D,toList(D:1.0)),m->first degree m)))
    M=lift(product(toList((D+1)..(D+n)))/n!-1,ZZ);
    assert(1==min(apply(randomMonomialSet(n,D,M),m->first degree m)))
    n=10; D=5;
    assert(1==min(apply((randomMonomialSet(n,D,1.0, Strategy=>"Minimal"),m->first degree m))))
    assert(1==min(apply((randomMonomialSet(n,D,toList(D:1.0), Strategy=>"Minimal"),m->first degree m))))
    assert(1==min(apply(randomMonomialSet(n,D,toList(D:1)), m->first degree m)))
///


--*************************--
--  bettiStats  --
--*************************--
TEST///
   R = ZZ/101[a..c];
   L={monomialIdeal (a^2*b,b*c), monomialIdeal(a*b,b*c^3)};
   (meanBettiShape,meanBetti,stdDevBetti) = bettiStats L;
   -- mean Betti table:
   b=new BettiTally from { (0,{0},0) => 2, (1,{2},2) => 2, (1,{3},3) => 1, (2,{4},4) => 1, (1,{4},4) => 1, (2,{5},5) =>1 }
   assert(1/2*sub(matrix lift(2*meanBetti,ZZ),RR) ==  1/2*sub(matrix b,RR))
   -- mean Betti shape:
   b=new BettiTally from { (0,{0},0) => 1, (1,{2},2) => 1, (1,{3},3) => 0.5, (2,{4},4) => 0.5, (1,{4},4) => 0.5, (2,{5},5) =>0.5 }
   assert(1/2*sub(matrix lift(2*meanBettiShape,ZZ),RR) ==  1/2*sub(matrix lift(2*b,ZZ),RR))
   -- std of Betti table:
   assert(0 == stdDevBetti_(0, {0}, 0))
   assert(0.5 == stdDevBetti_(1, {3}, 3))
///


--*************************--
--  degStats  --
--*************************--
TEST///
   --check for p = 0 the average degree should be 1
   listOfIdeals = idealsFromGeneratingSets(randomMonomialSets(3,4,0.0,6));
   assert(1==(degStats(listOfIdeals))_0)
   assert(0==(degStats(listOfIdeals))_1)
   listOfIdeals = idealsFromGeneratingSets(randomMonomialSets(7,2,0,3));
   assert(1==(degStats(listOfIdeals))_0)
   assert(0==(degStats(listOfIdeals))_1)
   --check for p = 1 the average degree is 1
   listOfIdeals = idealsFromGeneratingSets(randomMonomialSets(3,4,1.0,6));
   assert(1==(degStats(listOfIdeals))_0)
   assert(0==(degStats(listOfIdeals))_1)
   --Check average is correct for set monomials
   L=randomMonomialSet(3,3,1.0);
   R=ring(L#0);
   listOfIdeals={monomialIdeal(R_0^3,R_1,R_2^2),monomialIdeal(R_0^3,R_1,R_0*R_2)};
   assert(3.5==(degStats(listOfIdeals,ShowTally=>true))_0)
   assert(2.5==(degStats(listOfIdeals,ShowTally=>true))_1)
   assert(2==sum(values(degStats(listOfIdeals, ShowTally=>true))_2))
   listOfIdeals={monomialIdeal(0_R),monomialIdeal(R_2^2)};
   assert(1.5==(degStats(listOfIdeals, ShowTally=>true))_0)
   assert(0.5==(degStats(listOfIdeals, ShowTally=>true))_1)
   assert(2==sum(values(degStats(listOfIdeals,ShowTally=>true))_2))
   listOfIdeals={monomialIdeal(R_0),monomialIdeal(R_0^2*R_2),monomialIdeal(R_0*R_1^2,R_1^3,R_1*R_2,R_0*R_2^2)};
   assert(sub(8/3,RR)==(degStats(listOfIdeals,ShowTally=>true))_0)
   assert((sub(14/9,RR))^(1/2)==(degStats(listOfIdeals,ShowTally=>true))_1)
   assert(3==sum(values(degStats(listOfIdeals,ShowTally=>true))_2))

///

--************************--
--  dimStats  --
--************************--
TEST ///
    --check for p = 0 the average Krull dimension is n
    listOfIdeals = idealsFromGeneratingSets(randomMonomialSets(3,4,0.0,6));
    assert(3==(dimStats(listOfIdeals))_0)
    assert(0==(dimStats(listOfIdeals))_1)
    listOfIdeals = idealsFromGeneratingSets(randomMonomialSets(7,2,0,3));
    assert(7==(dimStats(listOfIdeals))_0)
    assert(0==(dimStats(listOfIdeals))_1)
    --check for p = 1 the average Krull dimension is 0
    listOfIdeals = idealsFromGeneratingSets(randomMonomialSets(3,4,1.0,6));
    assert(0==(dimStats(listOfIdeals))_0)
    assert(0==(dimStats(listOfIdeals))_1)
    --check for set monomials
    L=randomMonomialSet(3,3,1.0);
    R=ring(L#0);
    listOfIdeals = {monomialIdeal(R_0^3,R_1,R_2^2), monomialIdeal(R_0^3, R_1, R_0*R_2)};
    assert(.5==(dimStats(listOfIdeals, ShowTally=>true))_0)
    assert(.5==(dimStats(listOfIdeals, ShowTally=>true))_1)
    assert(2==sum( values (dimStats(listOfIdeals, ShowTally=>true))_2))
    listOfIdeals = {monomialIdeal (0_R), monomialIdeal (R_2^2)};
    assert(2.5== (dimStats(listOfIdeals,ShowTally=>true))_0)
    assert(.5==(dimStats(listOfIdeals, ShowTally=>true))_1)
    assert(2==sum( values (dimStats(listOfIdeals, ShowTally=>true))_2))
    listOfIdeals = {monomialIdeal (R_0), monomialIdeal (R_0^2*R_2), monomialIdeal(R_0*R_1^2,R_1^3,R_1*R_2,R_0*R_2^2)};
    assert(sub(5/3,RR)==(dimStats(listOfIdeals,ShowTally=>true))_0)
    assert((3-(sub(5/3,RR))^2)^(1/2)==(dimStats(listOfIdeals,ShowTally=>true))_1)
    assert(3==sum( values (dimStats(listOfIdeals, ShowTally=>true))_2))
///

--************************--
--  randomMonomialIdeals  --
--************************--

TEST ///
  -- check the number of ideals
  n=5; D=5; p=.6; N=3;
  B = flatten randomMonomialIdeals(n,D,p,N,IncludeZeroIdeals=>false);
  assert (N===(#B-1+last(B))) -- B will be a sequence of nonzero ideals and the number of zero ideals in entry last(B)
  C = randomMonomialIdeals(n,D,p,N,IncludeZeroIdeals=>true);
  assert (N===#C)
///

TEST ///
  -- check the number of monomials in the generating set of the ideal
  n=4; D=6; M=7; N=1;
  B = flatten randomMonomialIdeals(n,D,M,N);
  assert (M>=numgens B_0)
///

--************--
--  regStats  --
--************--
TEST ///
  -- check average regularity
  n=3; D=5; N=4; p=1.0;
  B=randomMonomialIdeals(n,D,p,N);
  assert((1,0)==regStats(B))
  p={0,1,0,0,0};
  B=randomMonomialIdeals(n,D,p,N);
  assert((2,0)==regStats(B))
  p={0,0,1,0,0};
  B=randomMonomialIdeals(n,D,p,N);
  assert((3,0)==regStats(B))
  p={0,0,0,1,0};
  B=randomMonomialIdeals(n,D,p,N);
  assert((4,0)==regStats(B))
  p={0,0,0,0,1};
  B=randomMonomialIdeals(n,D,p,N);
  assert((5,0)==regStats(B))
  p=0;
  B=randomMonomialIdeals(n,D,p,N);
  assert((-infinity,0)==regStats(B))
///

TEST ///
  -- check all stats
  L=randomMonomialSet(3,3,1.0);
  R=ring(L#0);
  listOfIdeals={monomialIdeal(R_1,R_2^2),monomialIdeal(R_0^3,R_1,R_0*R_2)};
  A = regStats(listOfIdeals, ShowTally=>true);
  assert(2.5===A_0)
  assert(0.5===A_1)
  assert(2==sum(values(A_2)))
///
--***************--
--    CMStats    --
--***************--

TEST ///
 L=randomMonomialSet(5,1,1.0); R=ring(L#0);
 listOfIdeals = {monomialIdeal(0_R)};
 assert(1==CMStats(listOfIdeals))
 listOfIdeals = {monomialIdeal(R_0*R_1, R_2*R_0)};
 assert(0==CMStats(listOfIdeals))
 listOfIdeals = {monomialIdeal(0_R), monomialIdeal(R_0*R_1, R_2*R_0)};
 assert(.5==CMStats(listOfIdeals))
 listOfIdeals = {monomialIdeal(0_R), monomialIdeal(R_0*R_1, R_2*R_0), monomialIdeal(R_0)};
 assert(2/3==CMStats(listOfIdeals))
///

--********************--
--  borrelFixedStats  --
--********************--

TEST ///
L=randomMonomialSet(5,1,1.0); R=ring(L#0);
listOfIdeals = {monomialIdeal(0_R)};
assert(1==borelFixedStats(listOfIdeals))
listOfIdeals = {monomialIdeal(R_0*R_1)};
assert(0==borelFixedStats(listOfIdeals))
listOfIdeals = {monomialIdeal(R_0), monomialIdeal(R_0*R_1)};
assert(.5==borelFixedStats(listOfIdeals))
listOfIdeals = {monomialIdeal(0_R), monomialIdeal(R_0*R_1, R_2*R_0), monomialIdeal(R_0)};
assert(2/3==borelFixedStats(listOfIdeals))
///

--***************--
--  mingenStats  --
--***************--

TEST ///
  -- check average number of minimum generators
  n=4; D=3; p=1.0; N=3;
  B = randomMonomialIdeals(n,D,p,N);
  C = mingenStats(B);
  assert (sub(n,RR)===C_0)
  assert (sub(0,RR)===C_1)
  p={0.0,1.0,0.0};
  D = randomMonomialIdeals(n,D,p,N);
  E = mingenStats(D);
  assert (sub(10,RR)===E_0)
  assert (sub(0,RR)===E_1)
///

TEST ///
  -- check average degree complexity
  n=3; D=5; p=1.0; N=5;
  B = randomMonomialIdeals(n,D,p,N);
  C = mingenStats(B);
  assert(sub(1,RR)===C_2)
  assert(sub(0,RR)===C_3)
  p={0.0,0.0,0.0,0.0,1.0};
  D = randomMonomialIdeals(n,D,p,N);
  E = mingenStats(D);
  assert(sub(5,RR)===E_2)
  assert(sub(0,RR)===E_3)
///

TEST ///
  L=randomMonomialSet(3,3,1.0);
  R=ring(L#0);
  listOfIdeals={monomialIdeal(R_1,R_2^2),monomialIdeal(R_0^3,R_1,R_0*R_2)};
  A = mingenStats(listOfIdeals, ShowTally=>true);
  assert(2.5===A_0)
  assert(0.5===A_1)
  assert(2==sum(values(A_2)))
  assert(2.5===A_3)
  assert(0.5===A_4)
  assert(2==sum(values(A_5)))
///

--***************--
--   pdimStats   --
--***************--

TEST ///
  L=randomMonomialSet(3,3,1.0);
  R=ring(L#0);
  listOfIdeals={monomialIdeal(0_R)};
  assert(sub(0,RR)==(pdimStats(listOfIdeals))_0)
  assert(sub(0,RR)==(pdimStats(listOfIdeals))_1)
  listOfIdeals={monomialIdeal(R_0,R_1,R_2)};
  assert(sub(3,RR)==(pdimStats(listOfIdeals))_0)
  assert(sub(0,RR)==(pdimStats(listOfIdeals))_1)
  listOfIdeals={monomialIdeal(0_R),monomialIdeal(R_0*R_1^2,R_1^3,R_2)};
  assert(sub(1.5,RR)==(pdimStats(listOfIdeals))_0)
  assert(sub(1.5,RR)==(pdimStats(listOfIdeals))_1)
  listOfIdeals={monomialIdeal(R_0^2*R_1,R_2)};
  assert(sub(2,RR)==(pdimStats(listOfIdeals))_0)
  assert(sub(0,RR)==(pdimStats(listOfIdeals))_1)
  listOfIdeals={monomialIdeal(R_0,R_2),monomialIdeal(0_R),monomialIdeal(R_0^2*R_1,R_1^2)};
  assert(sub(4/3,RR)==(pdimStats(listOfIdeals))_0)
  assert(sub(((8/3)-(16/9))^(1/2),RR)==(pdimStats(listOfIdeals))_1)
///


--****************************--
--  idealsFromGeneratingSets  --
--****************************--

TEST///
  -- check the number of ideals
  n=5; D=5; p=.6; N=3;
  B = flatten idealsFromGeneratingSets(randomMonomialSets(n,D,p,N),IncludeZeroIdeals=>false);
  assert (N===(#B-1+last(B))) -- B will be a sequence of nonzero ideals and the number of zero ideals in entry last(B)
  C = idealsFromGeneratingSets(randomMonomialSets(n,D,p,N),IncludeZeroIdeals=>true);
  assert (N===#C)
///

TEST ///
  --check that all elements are MonomialIdeal
  n=3;D=3;p=1.0;N=5;
  B=idealsFromGeneratingSets(randomMonomialSets(n,D,p,N));
  assert (all(B,b->instance(b,MonomialIdeal)))
  C=idealsFromGeneratingSets(randomMonomialSets(n,D,p,N),IncludeZeroIdeals=>false);
  assert (all(C#0,c->instance(c,MonomialIdeal)))
///

--****************************--
--  statistics  --
--****************************--

TEST///
  -- Check generated statistics
  stat = statistics(sample(ER(5,5,1.0),10),x->#x);
  assert(stat.Mean == 251)
  assert(stat.StdDev == 0)
///

end


restart;
uninstallPackage"RandomMonomialIdeals";
installPackage("RandomMonomialIdeals",RemakeAllDocumentation=>true);

check RandomMonomialIdeals 
viewHelp RandomMonomialIdeals
viewHelp bettiStats
