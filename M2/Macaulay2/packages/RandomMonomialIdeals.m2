--**************************--
-- -*- coding: utf-8 -*-
newPackage(
	"RandomMonomialIdeals",
    	Version => "1.0",
    	Date => "January 28, 2019",
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
    	Headline => "Erdos-Renyi-type random monomial ideals",
	Keywords => {"Examples and Random Objects"},
     	PackageImports => { "OldChainComplexes", "Depth", "BoijSoederberg", "Serialization" },
    	DebuggingMode => false,
	Certification => {
	     "journal name" => "The Journal of Software for Algebra and Geometry",
	     "journal URI" => "https://msp.org/jsag/",
	     "article title" => "Random Monomial Ideals: a Macaulay2 package",
	     "acceptance date" => "11 April 2019",
	     "published article URI" => "https://msp.org/jsag/2019/9-1/p08.xhtml",
	     "published article DOI" => "10.2140/jsag.2019.9.65",
	     "published code URI" => "https://msp.org/jsag/2019/9-1/jsag-v9-n1-x08-RandomMonomialIdeals.m2",
	     "release at publication" => "902570b14480e7590b6960b1352331acce3ef817",	    -- git commit number in hex
	     "version at publication" => "1.0",
	     "volume number" => "9",
	     "volume URI" => "https://msp.org/jsag/2019/9-1/"
	     }
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
    "pdimStats",
    "Sample",
    "sample",
    "ModelName", "Parameters", "Generate", "SampleSize", "getData",
    "writeSample",
    "Model",
    "model",
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

model = method(TypicalValue => Model)
model(List,FunctionClosure,String):=(p,f,name)->(
    -- p = parameter list
    -- f = random generator from the model (predefined fn!)
    new Model from { 
	Name => name,
	Parameters => p,
	Generate => () -> f toSequence p
	}
)


ER = method(TypicalValue => Model)
ER (ZZ,ZZ,RR) := (n,D,p) -> (
    if n<1 then error "n expected to be a positive integer";
    if p<0.0 or 1.0<p then error "p expected to be a real number between 0.0 and 1.0";
    x := symbol x;
    R := QQ[x_1..x_n];
    new Model from {
	Name => "Erdos-Renyi", 
    	Parameters => (n,D,p), 
    	Generate => ()->randomMonomialSet(R,D,p)
	}
)

ER (PolynomialRing,ZZ,RR) := (R,D,p) -> (
    if p<0.0 or 1.0<p then error "p expected to be a real number between 0.0 and 1.0";
    new Model from {
	Name => "Erdos-Renyi", 
    	Parameters => (R,D,p), 
    	Generate => ()->randomMonomialSet(R,D,p)
    	}
)

ER (ZZ,ZZ,ZZ) := (n,D,M) -> (
    if n<1 then error "n expected to be a positive integer";
    x := symbol x;
    R := QQ[x_1..x_n];
    new Model from {
	Name => "Erdos-Renyi",
    	Parameters => (n,D,M),
	Generate => ()->randomMonomialSet(R,D,M)
	}
)

ER (PolynomialRing,ZZ,ZZ) := (R,D,M) -> (
    new Model from {
	Name => "Erdos-Renyi",
    	Parameters => (R,D,M),
	Generate => ()->randomMonomialSet(R,D,M)
	}
)

ER (ZZ,ZZ,List) := (n,D,pOrM) -> (
    if n<1 then error "n expected to be a positive integer";
    if #pOrM != D then error "pOrM expected to be a list of length D";
    if not all(pOrM, q->instance(q, ZZ)) and not all(pOrM, q->instance(q,RR))
      then error "pOrM must be a list of all integers or all real numbers";
    x := symbol x;
    R := QQ[x_1..x_n];
    new Model from {
	Name => "Erdos-Renyi",
	Parameters => (n,D,pOrM),
	Generate => ()->randomMonomialSet(R,D,pOrM)
	}
)

ER (PolynomialRing,ZZ,List) := (R,D,pOrM) -> (
    if #pOrM != D then error "pOrM expected to be a list of length D";
    if not all(pOrM, q->instance(q, ZZ)) and not all(pOrM, q->instance(q,RR))
        then error "pOrM must be a list of all integers or all real numbers";
    if all(pOrM, q->instance(q,RR)) and any(pOrM,q-> q<0.0 or 1.0<q)
        then error "pOrM expected to be a list of real numbers between 0.0 and 1.0";
    new Model from {
	Name => "Erdos-Renyi",
	Parameters => (R,D,pOrM),
	Generate => ()->randomMonomialSet(R,D,pOrM)
	}
)

sample = method(TypicalValue => Sample)
sample (Model, ZZ) := (M,N) -> (
    if N<1 then stderr << "warning: N expected to be a positive integer" << endl;
    s:=new Sample;
    s.ModelName = M.Name;
    s.Parameters = M.Parameters;
    s.SampleSize = N;
    s.Data =  apply(N,i->M.Generate());
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
writeSample (Sample, String) := (s, dirname) -> (
    if fileExists dirname then (
	stderr << "warning: directory or file with this name already exists." << endl;
        if not isDirectory dirname then (
	    stderr << "warning: overwrting file." << endl;
	    removeFile dirname;
	    mkdir dirname;
	    );
    	if isDirectory dirname then (
	    stderr << "warning: updating name of directory:" << endl;
	    dirname = dirname |"New";
    	    stderr << dirname << endl; 
	    mkdir dirname;
	    ); 
	) 
    else mkdir dirname;
    realpath dirname | "Model.txt" << s.SampleSize << endl << s.ModelName << endl << serialize s.Parameters << close;
    realpath dirname | "Data.txt" << serialize s.Data << close; -- Write other data
)


statistics = method(TypicalValue => HashTable)
statistics (Sample, Function) := HashTable => (s,f) -> (
    fData := apply(getData s,f);
    histogram := tally fData; 
    if not(class fData_0 === ZZ) then (
	-- in case the data is actually a BettiTally type, then we are able to get the mean and stddev of the tables:
	if (class fData_0 === BettiTally) then (
    	    -- compute the average (entry-wise) tally table:
	    dataSum := sum histogram;
            dataMean := mat2betti(1/s.SampleSize *(sub(matrix(dataSum), RR)));
    	    -- compute the standard deviation (entry-wise) of the Betti tables:
    	    dataMeanMtx := matrix dataMean;
    	    dataVariance := 1/s.SampleSize * sum apply(fData, currentTally -> (
	    	mtemp := new MutableMatrix from dataMeanMtx;
		currentTallyMatrix := matrix currentTally;
		apply(numrows currentTallyMatrix, i->
		    apply(numcols currentTallyMatrix, j->
			(
			    --compute  mtemp_(i,j) := (bMean_(i,j) - bCurrent_(i,j)):
			    mtemp_(i,j) = mtemp_(i,j) - currentTallyMatrix_j_i
			    )
			)
		    );
		--square entries of mtemp, to get (bMean_(i,j) - bCurrent_(i,j))^2:
		mtemp = matrix pack(apply( flatten entries mtemp,i->i^2), numcols mtemp)
		)
	    );
            --    dataStdDev := dataVariance^(1/2); -- <--need to compute entry-wise for the matrix(BettyTally)
    	    dataStdDev := mat2betti matrix pack(apply( flatten entries dataVariance,i->sqrt i), numcols dataVariance); 
	    new HashTable from {
		Mean=>mat2betti dataMeanMtx, 
		StdDev=>dataStdDev,
		Histogram=>histogram
		}
	) else ( 
	        stderr << "Warning: the statistics method is returning only the Tally of the outputs of 
		your function applied to the sample data. If you want more information, such as mean and 
		standard deviation, then ensure you use a function with numerical (ZZ) or BettiTally output." <<endl;
		histogram
	) 
    )
    else (
	mean := (sum fData)/s.SampleSize; 
    	new HashTable from {
	    Mean=>mean,
	    StdDev=>sqrt(sum apply(fData, x-> (mean-x)^2)/s.SampleSize),
	    Histogram=>histogram
	    }
    )
)


randomMonomialSets = method(TypicalValue => List, Options => {
	Coefficients => QQ,
	VariableName => "x",
	Strategy => "ER"
	})
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
    if not (instance(o.VariableName,Symbol) or instance(o.VariableName,String) or instance(o.VariableName,IndexedVariableTable)) then error "expected VariableName to be a string or symbol";
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
    if not (instance(o.VariableName,Symbol) or instance(o.VariableName,String) or instance(o.VariableName,IndexedVariableTable)) then error "expected VariableName to be a string or symbol";
    x := toSymbol o.VariableName;
    R := o.Coefficients[x_1..x_n];
    apply(N,i-> randomMonomialSet(R,D,pOrM,o))
)

randomMonomialSets (PolynomialRing,ZZ,List,ZZ) := List => o -> (R,D,pOrM,N) -> (
    if N<1 then stderr << "warning: N expected to be a positive integer" << endl;
    apply(N,i-> randomMonomialSet(R,D,pOrM,o))
)

randomMonomialSet = method(TypicalValue => List, Options => {
	Coefficients => QQ,
	VariableName => "x",
	Strategy => "ER"
	})
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
    if not (instance(o.VariableName,Symbol) or instance(o.VariableName,String) or instance(o.VariableName,IndexedVariableTable)) then error "expected VariableName to be a string or symbol";
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
    if not (instance(o.VariableName,Symbol) or instance(o.VariableName,String) or instance(o.VariableName,IndexedVariableTable)) then error "expected VariableName to be a string or symbol";
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
		)
	    )
	) else B = flatten apply(toList(1..D), d->take(random(flatten entries basis(d,R)), pOrM_(d-1)));
    ) else if all(pOrM,q->instance(q,RR)) then (
        if any(pOrM,q-> q<0.0 or 1.0<q) then error "pOrM expected to be a list of real numbers between 0.0 and 1.0";
        if o.Strategy === "Minimal" then (
            currentRing := R;
            apply(D, d->(
                chosen := select(flatten entries basis(d+1, currentRing), m->random(0.0,1.0)<=pOrM_d);
                B = flatten append(B, chosen/(i->sub(i, R)));
                currentRing = currentRing/promote(ideal(chosen), currentRing)
		)
	    )
	) else B = flatten apply(toList(1..D),d-> select(flatten entries basis(d,R),m-> random(0.0,1.0)<=pOrM_(d-1)));
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
		apply(numcols currentBettiMatrix, j->(
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
    degHistogram := apply(ideals, i-> degree i);
    ret:=();
    avg:=1./N*(sum degHistogram);
    Ex2:=1./N*(sum apply(elements(tally degHistogram), i->i^2));
    var:= Ex2 - avg^2;
    stdDev:= var^(1/2);
    if o.ShowTally then return (avg, stdDev, tally degHistogram);
    if o.Verbose then (
	numberOfZeroIdeals := (extractNonzeroIdeals(ideals))_1;
	stdio <<  "There are "<<N<<" ideals in this sample. Of those, "<< numberOfZeroIdeals <<" are the zero ideal." << endl;
	if numberOfZeroIdeals>0 then stdio <<"The degree statistics do include those for the zero ideals."<< endl
	);
    (avg, stdDev)
)

--creates a list of monomialIdeal objects from a list of monomial generating sets
idealsFromGeneratingSets =  method(TypicalValue => List, Options => {IncludeZeroIdeals => true, Verbose => false})
idealsFromGeneratingSets(List):= o -> (B) -> (
    N := # B;
    n := numgens ring ideal B#0; -- ring of the first monomial in the first gen set
    ideals := B / (b-> monomialIdeal b);
    (nonzeroIdeals,numberOfZeroIdeals) := extractNonzeroIdeals(ideals);
    if o.Verbose then stdio <<"There are "<<#B<<" ideals in this sample. Of those, "<<numberOfZeroIdeals<<" are the zero ideal."<< endl;
    if o.IncludeZeroIdeals then return ideals else return (nonzeroIdeals,numberOfZeroIdeals);
)


dimStats = method(TypicalValue => Sequence, Options => {ShowTally => false, Verbose =>false})
dimStats List := o-> (ideals) -> (
    N := #ideals;
    dims:=0;
    dimsHistogram := apply(ideals, i-> dim i); 
    ret:= ();
    avg:=1./N*(sum dimsHistogram);
    Ex2:=1./N*(sum apply(elements(tally dimsHistogram), i->i^2));
    var:= Ex2 - avg^2;
    stdDev:= var^(1/2);
    if o.ShowTally then return (avg, stdDev, tally dimsHistogram);
    if o.Verbose then (
	numberOfZeroIdeals := (extractNonzeroIdeals(ideals))_1;
	stdio <<  "There are "<<N<<" ideals in this sample. Of those, "<< numberOfZeroIdeals <<" are the zero ideal." << endl;
	if numberOfZeroIdeals>0 then stdio <<"The Krull dimension statistics do include those for the zero ideals."<< endl
	);
    (avg, stdDev)
)

regStats = method(TypicalValue => Sequence, Options => {ShowTally => false, Verbose => false})
regStats List := o-> (ideals) -> (
    N:=#ideals;
    ideals = extractNonzeroIdeals(ideals);
    ideals = ideals_0;
    reg := 0;
    regHistogram:={};
    if set {} === set ideals then (
	regHistogram = N:-infinity;
	stdDev := 0;
	if o.ShowTally then return (-infinity, 0, tally regHistogram);
	if o.Verbose then stdio <<"All ideals in this list are the zero ideal." << endl;
	(-infinity, 0)
    )
    else (
	regHistogram = apply(ideals, i-> regularity i);
    	avg := 1./#ideals*(sum regHistogram);
    	Ex2 := (1./(#ideals))*(sum apply(elements(tally regHistogram), i->i^2));
    	var := Ex2-avg^2;
    	stdDev = var^(1/2);
    	if o.ShowTally then return (avg, stdDev,tally regHistogram);
    	if o.Verbose then (
	    stdio << "There are "<<N<<" ideals in this sample. Of those, "<< toString(N-#ideals) <<" are the zero ideal." << endl;
	    stdio << "The zero ideals were extracted from the sample before reporting the regularity statistics."<< endl;
	    );
    	(avg, stdDev)
    )
)

randomMonomialIdeals = method(TypicalValue => List, Options => {Coefficients => QQ, VariableName => "x", IncludeZeroIdeals => true, Strategy => "ER"})

randomMonomialIdeals (ZZ,ZZ,List,ZZ) := List => o -> (n,D,pOrM,N) -> (
    B:={};
    if all(pOrM,q->instance(q,RR)) then B=randomMonomialSets(n,D,pOrM,N,Coefficients=>o.Coefficients,VariableName=>o.VariableName,Strategy=>"Minimal")
    else if all(pOrM,q->instance(q,ZZ)) then B=randomMonomialSets(n,D,pOrM,N,Coefficients=>o.Coefficients,VariableName=>o.VariableName, Strategy=>o.Strategy);
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
	if isCM(R/ideals_i) == true then cm = cm + 1 else cm = cm
	);
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
        if isBorel((ideals_i)) == true then bor = bor + 1 else bor = bor
	);
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
	if o.ShowTally then return (-infinity, 0, tally numgensHist, -infinity, 0, tally complexityHist);
	if o.Verbose then stdio <<"This sample included only zero ideals." << endl;
	(-infinity, 0, -infinity, 0)
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
    if o.ShowTally then return (numAvg, numStdDev, tally numgensHist, comAvg, comStdDev, tally complexityHist);
    if o.Verbose then (
        stdio <<"There are "<<N<<" ideals in this sample. Of those, " << numberOfZeroIdeals << " are the zero ideal." << endl;
	if numberOfZeroIdeals>0 then stdio <<"The statistics returned (mean and standard deviation of # of min gens and mean and standard deviation of degree complexity) do NOT include those for the zero ideals."<< endl
	);
    (numAvg, numStdDev, comAvg, comStdDev)
    )
)


pdimStats = method(TypicalValue=>Sequence, Options => {ShowTally => false, Verbose => false})
pdimStats (List) := o-> (ideals) -> (
    N:=#ideals;
    R:=ring(ideals_0);
    pdHist := apply(ideals, i-> pdim(R^1/i));
    ret:=();
    avg:=sub(((1/N)*(sum pdHist)),RR);
    Ex2:=sub(((1/N)*(sum apply(elements(tally pdHist), i->i^2))), RR);
    var:= Ex2 - avg^2;
    stdDev:= var^(1/2);
    if o.ShowTally then return (avg, stdDev, tally pdHist);
    if o.Verbose then (
	numberOfZeroIdeals := (extractNonzeroIdeals(ideals))_1;
        stdio <<"There are "<<N<<" ideals in this sample. Of those, " << numberOfZeroIdeals << " are the zero ideal." << endl;
	if numberOfZeroIdeals>0 then stdio <<"The projective dimension statistics do include those for the zero ideals."<< endl
	);
    (avg, stdDev)
)

--**********************************--
--  Internal methods	    	    --
--**********************************--

toSymbol = method()
toSymbol Symbol := p -> p
toSymbol String := p -> getSymbol p
toSymbol IndexedVariableTable := p -> p

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
	    )
	);
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
   It also introduces new types of objects, @TO Sample@ and @TO Model@, to allow for streamlined handling of random objects and their statistics in {\em Macaulay2}. 
   Most of the models implemented are drawn from the paper {\em Random Monomial Ideals} by Jesus A. De Loera, Sonja Petrovic, Lily Silverstein, Despina Stasi, and Dane Wilburne 
   (@HREF"https://arxiv.org/abs/1701.07130"@). 
   
   The main method, @TO randomMonomialSets@, generates a sample of size $N$ from the distribution $\mathcal B(n, D, p)$ of 
   sets of monomials of degree at most $D$ on $n$ variables, where $p$ is the probability of selecting any given monomial: 
  Example
   n=3; D=2; p=0.5; N=4; 
   L = randomMonomialSets(n,D,p,N)
  Text 
   For a formal definition of the distribution, see Section 1 of @HREF"https://arxiv.org/abs/1701.07130"@. 
   As is customary, we use the word `model' when referring to a distribution of the random objects at hand. 
   
   The model defined by $\mathcal B(n, D, p)$ was inspired by the Erdos-Renyi random graph model denoted by $G(n,p)$:  
   it is a natural generalization, as squarefree monomials of degree tww can be encoded by edges of a graph whose vertices are the variables. 
   The random graph model can be introduced in two ways: one can either fix the probability of an edge or fix the total number of edges. 
   Thus the package also includes the model variant that generates a fixed  {\em number} of monomials: 
  Example
    n=3; D=2; M=3; N=4;
   L = randomMonomialSets(n,D,M,N)
  Text
   To sample from the {\em graded} model from Section 6 of @HREF"https://arxiv.org/abs/1701.07130"@, 
   simply replace $p$ by a list of $D$ probabilities, one for each degree. 
   In the example below, monomials of degree 1 are not selected since their probability = 0, 
   while each monomial of degree 2 is selected with probability 1.
  Example
   n=3; D=2; N=4;
   randomMonomialSets(n,D,{0.0,1.0},N)
  Text
   The package also allows for sampling from the {\em graded} version of the fixed number of monomials model, 
   where we specify the requested number of monomials of each degree. 
   In the example below, we sample random sets of monomials with one monomial of degree 1, zero of degree 2 and three monomials of degree 3.
  Example
   n=3; D=3; N=4;
   randomMonomialSets(n,D,{1,0,3},N)
  Text
   Finally, we can request the monomial sets generated by the {\em graded} model with a fixed number of monomials to be minimal generating sets. We can also employ the minimal strategy for a couple of other versions of the randomMonomialSets method.
  Example
   n=3; D=3; N=4;
   randomMonomialSets(n,D,{1,0,3},N, Strategy=>"Minimal")   
   randomMonomialSets(n,D,{0.0,0.3,0.5},N, Strategy=>"Minimal")
   randomMonomialSets(n,D,0.1,N, Strategy=>"Minimal")   
  Text
   Once a sample (that is, a set of random objects) is generated, one can compute various statistics of algebraic properties of the sample. 
   The methods in the package offer a way to compute and summarize statistics of some of the common properties, such as 
   degree, dimension, projective dimension, Castelnuovo-Mumford regularity, etc. 
   For example, we can use the method @TO dimStats@ to get the Krull dimension statistics: 
  Example 
   ideals=idealsFromGeneratingSets(L)
   dimStats(ideals,ShowTally=>true)
  Text 
   The first entry in the output of the method @TO dimStats@ is the mean Krull dimension of the sample. 
   The second entry is the standard deviation.
   Similarly, one can obtain the mean and standard deviations of the number of minimal generators and degree complexity via @TO mingenStats@,
   and the average Betti table shape, mean Betti table, and its standard deviation via @TO bettiStats@: 
  Example
   mingenStats ideals
   bettiStats ideals
  Text
   For developing other models and computing statistics on objects other than monomial ideals, the package also 
   defines two new types of object, @TO Model@ and @TO Sample@, which allow for convenient storage of statistics 
   from a sample of algebraic objects and streamlines writing sample data into files.

   For example, below we create a sample of size 10 from the Erdos-Renyi distribution $\mathcal B(n, D, p)$ on monomials 
   in $Q[y,w]$ with $D=4$, and $p=0.5$, and then a sample of size 15 from the graded version of this distribution 
   on monomials in $Z/101[z_1..z_8]$ with $D=2$, and $p={0.25,0.5}$: 
  Example
   sample1 = sample(ER(QQ[y,w],4,0.5),10)
   sample2 = sample(ER(ZZ/101[z_1..z_8],2,{0.25,0.75}),15)
  Text
   The output is a hash table with 4 entries. To obtain the random sets of monomials that were generated (that is, the actual data we are interested in),
   use the command @TO getData@: 
  Example
   keys sample1
   sample2.Parameters
   myData = getData(sample1);
   myData_0
  Text 
   We can also use the object of type @TO Sample@ to calculate the mean, standard deviation, and tally of the dimension of the ideals in the sample: 
  Example
   statistics(sample(ER(CC[z_1..z_8],5,0.1),100), degree@@ideal)
  Text
  
   Most of the methods in this package offer various options, such as selecting a specific ring with which to work, 
   or specifying variable names, coefficients, etc. Here is a simple example:
  Example
   R=ZZ/101[a..e];
   randomMonomialSets(R,D,p,N)
   randomMonomialSets(n,D,p,N,VariableName=>"t")
  Text
   In some cases, we may want to work directly with the sets of randomly chosen monomials, while at other times it may be 
   more convenient to pass directly to the corresponding random monomial ideals.
   Both options induce the same distribution on monomial ideals:
  Example
   randomMonomialSets(3,4,1.0,1)
   monomialIdeal flatten oo
   randomMonomialIdeals(3,4,1.0,1)
 SeeAlso
  randomMonomialSet
  Verbose
  sample
  model
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
   This function creates $N$ random sets of monomials of degree $d$, $1\leq d\leq D$, in $n$ variables.
   It does so by calling @TO randomMonomialSet@ $N$ times.
 SeeAlso
  randomMonomialSet
///

doc ///
 Key
  bettiStats
  (bettiStats,List)
 Headline
  statistics on Betti tables of a sample of monomial ideals or list of objects
 Usage
  bettiStats(List)
 Inputs
  L: List
   of objects of type @TO MonomialIdeal@, or any objects to which @TO betti@ @TO res@ can be applied.
 Outputs
  : Sequence
   of objects of type @TO BettiTally@, representing the mean Betti table shape and the mean Betti table of the elements in the list {\tt L}.
 Description
  Text
   For a sample of ideals stored as a list, this function computes some basic Betti table statistics of the sample.
   Namely, it computes the average shape of the Betti tables (where 1 is recorded in entry (ij) for each element if $beta_{ij}$ is not zero),
   and it also computes the average Betti table (that is, the table whose (ij) entry is the mean value of $beta_{ij}$ for all ideals in the sample).
  Example
   R = ZZ/101[a..e];
   L={monomialIdeal"a2b,bc", monomialIdeal"ab,bc3",monomialIdeal"ab,ac,bd,be,ae,cd,ce,a3,b3,c3,d3,e3"}
   (meanBettiShape,meanBetti,stdDevBetti) = bettiStats L;
   meanBettiShape
   meanBetti
   stdDevBetti
  Text
   For sample size $N$, the average Betti table {\em shape} considers nonzero Betti numbers. It is to be interpreted as follows:
   entry (i,j) encodes the following sum of indicators:
   $\sum_{all ideals} 1_{beta_{ij}>0} / N$; that is,
   the proportion of ideals with a nonzero $beta_{ij}$.
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
///

doc ///
  Key
    SaveBettis
    [bettiStats, SaveBettis]
  Headline
    optional input to store all Betti tables computed
  Description
    Text
     The function that computes statistics on Betti tables has an option to save all of the Betti tables to a file.
     This may be useful if the computation from the resolution, which is what is called from @TO bettiStats@, takes too long.
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
      Putting the option {\tt CountPure => true} in function @TO bettiStats@ adds the number of pure Betti tables to the Betti table statistics. 
      In the following example, exactly one of the ideals has a pure Betti table: 
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
  statistics on the degrees of a list of objects
 Usage
  degStats(List)
 Inputs
  ideals: List
   of objects of type @TO MonomialIdeal@, or any objects to which @TO degree@ can be applied.
 Outputs
  : Sequence
   whose first entry is the average degree of a list of monomial ideals, second entry is the standard deviation of the degree, and third entry (if option turned on) is the degree tally
 Description
  Text
   This function computes the degree of $R/I$ for each ideal $I$ in the list and computes the mean and standard deviation of the degrees. 
  Example
   R=ZZ/101[a,b,c];
   ideals = {monomialIdeal"a3,b,c2", monomialIdeal"a3,b,ac"}
   degStats(ideals)
  Text
   The following examples use the existing functions @TO randomMonomialSets@ and @TO idealsFromGeneratingSets@ or 
   @TO randomMonomialIdeals@ to automatically generate a list of ideals, rather than creating the list manually:
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
    probability to select a monomial in the Erdos-Renyi-type model, OR
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
    list of randomly generated monomial ideals, and the number of zero ideals removed from the sample, if any
 Description
  Text
   This function creates $N$ random monomial ideals, with each monomial generator having degree $d$, $1\leq d\leq D$, in $n$ variables.
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
   If $M$ is an integer, then the function creates $N$ random monomial with $M$ (not necessarily minimal) generators:
   randomly select $M$ monomials from the list of all monomials of degree $1,\dots,D$ in $n$ variables, 
   then generate the corresponding ideal.
  Example
   n=8; D=4; M=7; N=3;
   randomMonomialIdeals(n,D,M,N)
  Text
   Note that each generating set of each ideal has at most $M = 7$ monomials. 
   If one monomial divides another monomial that was generated, it will not be in the generating set.

   The input of type @TO List@ controls the number of monomials in the generating set of each degree for the graded ER model.
   Specifically, this input is either a list of real numbers between 0 and 1, inclusive, whose i-th entry is
   the probability of including a monomial of degree i in the monomial set, or it is a list of nonnegative
   integers whose i-th entry is the number of monomials of each degree to include in the monomial set. Consider the following two examples:
   If $p=p_1,\dots,p_D$ is a list of real numbers of length $D$, then randomMonomialSet generates the set utilizing the graded Erdos-Renyi-type model:
   select each monomial of degree $1\le d\le D$, independently, with probability $p_d$.
  Example
   randomMonomialIdeals(2,3,{0.0, 1.0, 1.0},1)
  Text
   Note that no monomials of degree 1 were generated, since the first probability vector entry, $p_1$, is 0.

   If $M=M_1,\dots,M_D$ is a list of integers of length $D$, then the function creates a list of objects of type 
   @TO MonomialIdeal@, where at most $M_d$ monomials are of degree $d$.

  Example
   randomMonomialIdeals(3,3,{1,1,1},1)
  Text
   Observe that there are at most one degree-1, one degree-2, and one degree-3 monomials generating each ideal.
   
   If {\tt Strategy=>"ER"}, which is the default setting for the graded fixed number of generators version of the function, 
   each set of monomials used to generate a monomial ideal in the list is not necessarily minimal.
   Else if {\tt Strategy=> "Minimal"} then each monomial ideal in the list is generated by minimal sets of $M_d$ monomials, 
   or maximum number possible, of total degree $d$, starting from the smallest degree.

  Example
   randomMonomialIdeals(3,3,{1,1,1},1, Strategy=>"Minimal")
  Text
   Observe that there are at most one degree-1, one degree-2, and one degree-3 monomials generating each ideal. 
   Also observe that {\tt Strategy=>"Minimal"} generally gives more generators than the default {\tt Strategy=>"ER"}.
     
 Caveat
  Since the function returns a list of objects of type @TO MonomialIdeal@s, only the minimal generating set will be displayed.
  In contrast, the function @TO randomMonomialSet@ will display the full (not necessarily minimal) generating set produced by the model.
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
   The function randomMonomialSet creates a list of monomials, up to a given degree $d$, $1\leq d\leq D$, in $n$ variables.
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
   If $M$ is an integer, then the function creates a list of monomials of length $M$:
   randomly select $M$ monomials from the list of all monomials of degree $1,\dots,D$ in $n$ variables.
  Example
   n=10; D=5; M=4;
   randomMonomialSet(n,D,M)
  Text
   Note that it returns a set with $M = 4$ monomials.
  Text
   If $M$ is greater than the total number of monomials in $n$ variables of degree at most $D$, then the function will return all those monomials (and not $M$ of them). For example:
  Example
   randomMonomialSet(2,2,10)
  Text
   returns 5 monomials in a generating set, and not 10, since there are fewer than 10 monomials to choose from.

   The input of type @TO List@ controls the number of monomials in the generating set of each degree for the graded ER model.
   Specifically, this input is either a list of real numbers between 0 and 1, inclusive, whose i-th entry is
   the probability of including a monomial of degree i in the monomial set, or it is a list of nonnegative
   integers whose i-th entry is the number of monomials of each degree to include in the monomial set. 
   
   Consider the following two examples:
   If $p=p_1,\dots,p_D$ is a list of real numbers of length $D$, then the function generates the set utilizing the graded Erdos-Renyi-type model:
   select each monomial of degree $1\le d\le D$, independently, with probability $p_d$.
  Example
   randomMonomialSet(2,3,{0.0, 1.0, 1.0})
  Text
   Note that the degree-1 monomials were not generated, since the first probability vector entry is 0.
  Text
   If $M=M_1,\dots,M_D$ is a list of integers of length $D$, then the function creates a list of monomials, 
   where $M_d$ monomials are of degree $d$.
  Example
   randomMonomialSet(2,3,{2,1,1})
  Text
   Observe that there are two degree-1, one degree-2, and one degree-3 monomials.

   If {\tt Strategy=>"ER"}, the default setting for the graded fixed number of generators version of the function, 
   the set of monomials we obtain will not necessarily be minimal.
   Else if {\tt Strategy=> "Minimal"} then the set of monomials constitutes a minimal generating set which is 
   build up of $M_d$ monomials, or the maximum number possible, of total degree $d$, for $d$ from 1 to $D$, starting from $d=1$.

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
   of monomial ideals
 Description
  Text
   Given a list of sets of monomials, the function converts each set into a monomial ideal.
  Example
   n=4; D=2; p=1.0; N=3;
   B=randomMonomialSets(n,D,p,N); B/print
   idealsFromGeneratingSets(B)
  Text
   In case the option {\tt IncludeZeroIdeals} is set to false, the function also counts how many sets are converted to the zero ideal.
 SeeAlso
  randomMonomialIdeals
///

doc ///
 Key
  mingenStats
  (mingenStats, List)
 Headline
  statistics on the minimal generators of a list of ideals: number and degree complexity
 Usage
  mingenStats(List)
 Inputs
  ideals: List
   of objects of type @TO MonomialIdeal@ or @TO Ideal@
 Outputs
  : Sequence
   with the following entries: the average number of minimal generators, the standard deviation of the number of minimal generators, the average degree complexity, and the standard deviation of the degree complexity.
   If the option ShowTally is turned on, then the output sequence also includes the tallies of the two numbers following their standard deviation.
 Description
  Text
   This function removes zero ideals from the list of ideals, then calculates the average and the standard deviation for 
   the number of minimal generators and degree complexity of the list of nonzero ideals.
  Example
   n=4; D=3; p={0.0,1.0,0.0}; N=3;
   B=randomMonomialIdeals(n,D,p,N);
   mingenStats(B)
  Text
   If the list given is a list of all zero ideals, mingenStats returns -infinity for the mean number of minimal 
   generators and mean degree complexity.
  Example
   B=randomMonomialIdeals(3,3,0.0,1);
   mingenStats(B)
 Caveat
  The function mingenStats removes zero ideals from the list of ideals before computing the two values.
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
      Put {\tt Coefficients => r} for a choice of the ground field r as an argument in
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
    optional input to choose the indexed variable name for the polynomial ring
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
    optional input to choose the strategy for generating the random monomial set
  Description
    Text
      Put {\tt Strategy => "ER"} or {\tt Strategy => "Minimal"} as an argument in the function @TO randomMonomialSet@, 
      @TO randomMonomialSets@, or randomMonomialIdeals.
      "ER" draws random sets of monomials from the ER-type distribution $\mathcal B(n,D,p)$, while "Minimal" saves computation 
      time by using quotient rings to exclude any non-minimal generators from the list. 
      For @TO randomMonomialSets@ with the number of generators of pre-specified degrees is the input, 
      choosing {\tt Strategy => "Minimal"} will result in larger minimal generating sets.
///

doc ///
 Key
   IncludeZeroIdeals
   [idealsFromGeneratingSets, IncludeZeroIdeals]
   [randomMonomialIdeals, IncludeZeroIdeals]
   [bettiStats, IncludeZeroIdeals]
 Headline
   optional input to choose whether zero ideals should be included
 Description
   Text
     When the option is used with the function @TO randomMonomialIdeals@, if {\tt IncludeZeroIdeals => true} (the default), 
     then zero ideals will be included in the list of random monomial ideals.
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
     The option can also be used with the function @TO bettiStats@.
     If {\tt ideals} contains zero ideals, you may wish to exclude them when computing the Betti table statistics.
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
  statistics on the Krull dimension of a list of objects
 Usage
  dimStats(List)
 Inputs
  ideals: List
    of objects of type @TO MonomialIdeal@ or any type to which @TO dim@ can be applied.
 Outputs
  : Sequence
   whose first entry is the average Krull dimension of a list of monomial ideals, the second entry is the standard deviation of the Krull dimension, and third entry (if option turned on) is the Krull dimension tally
 Description
  Text
   The function dimStats computes the average and standard deviation of the Krull dimension for a list of monomial ideals.
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
     If {\tt ShowTally => false} (the default value), then only the 2 basic statistics - mean and standard deviation - 
     of the function will be returned.
     If {\tt ShowTally => true}, then both the statistics and the tally will be returned.
   Example
	   R=ZZ/101[a,b,c];
		 ideals = {monomialIdeal"a3,b,c2", monomialIdeal"a3,b,ac"}
		 dimStats(ideals)
     mingenStats(ideals)
     degStats(ideals)
     pdimStats(ideals)
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
  statistics on projective dimension of a list of objects
 Usage
  pdimStats(List)
 Inputs
  ideals: List
   of objects of type @TO MonomialIdeal@ or any type to which @TO pdim@ can be applied.
 Outputs
  : Sequence
   whose first entry is the mean projective dimension, the second entry is the standard deviation of the projective dimension, 
   and third entry (if option turned on) is the projective dimension tally for quotient rings of ideals in the list {\em ideals}.
 Description
  Text
   The function pdimStats computes the mean and standard deviation of the projective dimension of elements in the list:
  Example
   R=ZZ/101[a,b,c];
   ideals = {monomialIdeal(a^3,b,c^2), monomialIdeal(a^3,b,a*c)}
   pdimStats(ideals)
  Text
   The function can also output the projective dimension tally as follows: 
  Example
   R=ZZ/101[a,b,c];
   ideals = {monomialIdeal(a,c),monomialIdeal(b),monomialIdeal(a^2*b,b^2)}
   pdimStats(ideals, ShowTally=>true)
  Text
   The following examples use the existing functions @TO randomMonomialIdeals@ to automatically generate a list of ideals, rather than creating the list manually:
  Example
   ideals = randomMonomialIdeals(4,3,1.0,3)
   pdimStats(ideals)
   ideals = randomMonomialIdeals(4,6,0.01,10)
   pdimStats(ideals)
 SeeAlso
   ShowTally
///

doc ///
 Key
  regStats
  (regStats, List)
 Headline
  statistics on the regularities of a list of objects
 Usage
  regStats(List)
 Inputs
  : List
   of objects of type @TO MonomialIdeal@ or any type to which @TO regularity@ can be applied.
 Outputs
  : Sequence
   whose first entry is the mean regularity of a list of monomial ideals, second entry is the standard deviation of the 
   regularities, and third entry (if option is turned on) is the regularity tally.
 Description
  Text
   This function removes zero ideals from the list of ideals, then calculates the average and the standard deviation of the 
   regularity of the list of nonzero ideals.
  Example
   R=ZZ/101[a,b,c];
   ideals = {monomialIdeal(a^3,b,c^2), monomialIdeal(a^3,b,a*c)}
   regStats(ideals)
  Text
   If the list given is a list of all zero ideals, regStats returns -infinity for the mean regularity.
  Example
   B=randomMonomialIdeals(3,3,0.0,1)
   regStats(B)
 Caveat
  The function removes zero ideals from the list of ideals before computing the two values.
 SeeAlso
  ShowTally
 ///

doc ///
 Key
  CMStats
  (CMStats, List)
 Headline
  fraction of monomial ideals (or other objects) in the given list whose quotient ring is Cohen-Macaulay
 Usage
  CMStats(List)
 Inputs
  ideals: List
    of objects of type @TO MonomialIdeal@, or any type to which @TO isCM@ can be applied
 Outputs
  : QQ
    the fraction of Cohen-Macaulay ideals in the list
 Description
  Text
    The function checks whether the coordinate ring of each ideal in the given sample is arithmetically Cohen-Macaulay and 
    returns the proportion that are.
  Example
    R=ZZ/101[a,b,c];
    ideals = {monomialIdeal"a3,b,c2", monomialIdeal"a3,b,ac"}
    CMStats(ideals)
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
    of objects of type @TO MonomialIdeal@, or any type to which @TO isBorel@ can be applied
 Outputs
  : QQ
    the fraction of Borel-fixed monomial ideals in the list
 Description
  Text
    The function computes the fraction of Borel-fixed ideals in the list of monomial ideals.
  Example
    R=ZZ/101[a,b,c];
    ideals = {monomialIdeal"a3", monomialIdeal"a3,b,ac"}
    borelFixedStats(ideals)
///

doc ///
  Key
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
     Some of the functions that use this option by default exclude zero ideals when computing statistics on 
     a set of ideals, while others do not. 
     If {\tt Verbose => true}, then the functions will display this type of additional informational. 
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
     Other functions that have this option are as follows. Let us look at a list of nontrivial ideals to see more interesting statistics.
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
   a type used to store a data sample from a statistical model
  Description
   Text 
    A sample of algebraic objects is a set of data collected by a random generating process, necessarily captured
    by a formal statistical model. The type Sample defined here stores a set of data generated using an object
    of type Model. This allows for a streamlined way to sample random objects, store the data as a proper statistical sample,
    and study their algebraic properties under the probabilistic regime. 
    
    An object of type Sample is a hash table with the following keys: name of the model used to generate the sample,
    values of the model's parameters used to generate the sample, the size of the sample 
    (that is, the number of data points in it), and the data itself. 
   Example
    peek sample(ER(3,2,0.2),4)
  SeeAlso
   Model
   statistics
///

doc ///
  Key
   Model
  Headline
   a type used to store a statistical model and its parameters
  Description
   Text 
    In order to generate and study random algebraic objects formally, one should define a statistical model for 
    such objects. A model captures the random generating process of the objects. It consists of a name,
    a set of parameters, and a generating function, stored under the corresponding keys in the hash table. 
   Example
    ER(3,2,0.2)   
   Text
    Combined with the type Sample, the type Model defined here stores such information and 
    allows for a streamlined way to sample random objects, store the data as a proper statistical sample,
    and study their algebraic properties under the probabilistic regime. 
  SeeAlso
   Sample
   statistics
///

doc ///
  Key
   sample
   (sample,Model,ZZ)
  Headline
   generates a sample from the given model
  Usage
   sample(Model,ZZ)
  Inputs
   M: Model
     to be sampled from
   N: ZZ
     sample size
  Outputs
   : Sample
    a sample of size $N$ from the given model $M$
  Description
   Text
    This function generates $N$ realizations of the random variable that has the distribution specified by the given model.
   Example
    s=sample(ER(3,2,0.2),4)
   Text
    One obtains the data from the object of type @TO Sample@ (that is, the actual sample in the statistical sense) as follows:
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
  SeeAlso
   statistics
   getData
///

doc ///
  Key
    (sample,String)
  Headline
    creates an object of type Sample from a sample stored in a directory on disk
  Usage
    sample(String)
  Inputs
    : String
      name of the directory where the sample is stored
  Outputs
    : Sample
      Sample read from the specified directory
  Description
    Text
      A random sample of objects can be generated from an arbitrary model and stored to a directory on disk
      using the function writeSample. 
      This shows how to retrieve that stored sample and have it loaded as an object of type Sample: 
    Example 
      writeSample(sample(ER(2,3,0.1),5), "testDirectory")
      mySample = sample("testDirectory")
      peek mySample 
  SeeAlso
    Sample
    writeSample
    Model
///

doc ///
  Key
   model
   (model,List,FunctionClosure,String)
  Headline
   creates a new model for random objects with a given list of parameters and generating function 
  Usage
   model(L,f,name)
  Inputs
   L: List
     of parameter values chosen for the model
   f: FunctionClosure
     function that generates random elements in this model
   name: String
     of the model constructed
  Outputs
   : Model
    with those fixed parameter values
  Description
   Text
    To create your own model for random polynomials or other algebraic objects, use the model method as follows.
    Suppose you wish to construct a set of M random polynomials in 3 variables of degree 2. You may 
    use {\em Macaulay2}'s built-in random function: 
   Example
    f=(D,n,M)->(R=QQ[x_1..x_n];apply(M,i->random(D,R)))
   Text 
    To formalize the study of these random polynomials, embed this function into an object of type @TO Model@: 
   Example
    myModel = model({2,3,4},f,"rand(D,n,M): M random polynomials in n variables of degree D")
   Text
    Now obtain the data about such random polynomials from the sample (that is, the actual sample in the statistical sense) as follows:
   Example
    N=2;
    mySample = sample(myModel,N);
    peek mySample
  SeeAlso
   sample
   statistics
///
 
doc ///
  Key
    ModelName
  Headline
    name of the model used to generate a given sample
  Description
    Text 
      An object of type Sample contains several pieces of information about the random sample, namely: 
      the name of the model used to generate the sample,
      values of the model's parameters used to generate the sample, 
      the size of the sample (that is, the number of data points in it), and the data itself. 
      As each of these is a hash table entry, one obtains the model name by using the hash key ModelName: 
    Example
      (sample(ER(2,2,0.5),2)).ModelName
  SeeAlso
    Sample
///

doc ///
  Key
    Parameters
  Headline
    values of the model parameters that were used to generate a given sample
  Description
    Text 
      An object of type Sample contains several pieces of information about the random sample, namely: 
      the name of the model used to generate the sample,
      values of the model's parameters used to generate the sample, 
      the size of the sample (that is, the number of data points in it), and the data itself. 
      As each of these is a hash table entry, one obtains the value of the parameters that were used to generate
      the given sample via the hash key Parameters: 
    Example
      (sample(ER(2,2,0.5),2)).Parameters
  SeeAlso
    sample
///


doc ///
  Key
    Generate
  Headline
    model construct function
  Description
    Text
      An object of type Model consists of a model name, 
      a set of parameters, and a generating function, stored under the corresponding keys in the hash table. 
      The key Generate points to the function that is used to generate random elements according to the given model. 
    Example
      myModel = ER(2,2,0.5)
  SeeAlso
    Model
///

doc ///
  Key
    SampleSize
  Headline
    size of the random sample
  Description
    Text 
      An object of type Sample contains several pieces of information about the random sample, namely: 
      the name of the model used to generate the sample,
      values of the model's parameters used to generate the sample, 
      the size of the sample (that is, the number of data points in it), and the data itself. 
      As each of these is a hash table entry, one obtains the sample size by using the hash key SampleSize: 
    Example
      s = sample(ER(1,1,0.0),10)
      s.SampleSize
  SeeAlso
    Sample
///

doc ///
  Key
    writeSample
    (writeSample,Sample,String)
  Headline
    write sample data to a directory
  Usage
    writeSample(Sample,String)
  Inputs
    S: Sample
      to be written to a file
    DirName: String
      name of the directory where the sample data should be stored
  Description
    Text
      Write a random sample to a directory on disk. This function creates 
      a directory in which the model and data are stored: one text file contains the information 
      about the model used to generate the sample, and another text file contains the 
      information about the sample itself. 
    Example
      writeSample(sample(ER(2,3,0.1),5), "testDirectory")
      mySample = sample("testDirectory")
      peek mySample 
  SeeAlso
    Sample
    Model
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
    Text 
      An object of type Sample contains several pieces of information about the random sample, namely: 
      the name of the model used to generate the sample,
      values of the model's parameters used to generate the sample, 
      the size of the sample (that is, the number of data points in it), and the data itself. 
      The function getData is used to extract the sample data. 
      
      In the example below, we obtain a sample of size $5$ from the ER model with parameter values $(3,4,0.1)$
    Example
      s = sample(ER(3,4,0.1),5)
      getData s      
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
      of real numbers whose i-th entry is the probability of selecting a monomial of degree i, 
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
      of real numbers whose i-th entry is the probability of selecting a monomial of degree i, 
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
    statistics(S,f)
  Inputs
    S: Sample
      containing randomly generated objects from an object of type @TO Model@
    f: Function
      that is computed for each data point in the sample S
  Outputs
    : HashTable
      containing the basic statistics for the function f applied to the sample s
  Description
    Text
      This function generates statistics for the sample by applying the given function to each sample point and computing
      some basic summary statistics. Specifically, the function $f$ is applied
      to each element in the sample, and -- provided that the function has output of type either numerical (ZZ) or BettiTally --
      its result is then used to calculate a mean, standard deviation, and a histogram.
    Example
      s=sample(ER(6,3,0.2),15);
      statistics(s, degree@@ideal)
    Text
      The output above shows the histogram of the degrees of ideals in the sample, as well as mean degree and its standard deviation.
      The same kind of output is produced by the following statistics: 
    Example
      s=sample(ER(2,2,0.8),10)
      statistics(s,betti@@gens@@ideal)
    Text
      In the example above, the entry Mean is the average - entry-wise - of the Betti tables of the random ideals in the sample. 
      An adventurous user my wish to get statistics of other functions applied to the sample. 
      If the output of f is not ZZ or BettiTally, the method will tally the sample data: 
    Example 
      statistics(s,mingens@@ideal)
  Caveat 
    In fact, anything that can be run through "tally" can be used as the input function f to this method. 
///

doc ///
  Key
    Mean
  Headline
    a summary statistic for a list of objects
  Description
    Text
      A sample mean is one of the basic summary statistics for a sample of randomly generated objects 
      (or any list of objects in general). 
      The mean value statistic for some property of a list of objects is stored under the key Mean in 
      the hash table returned by the function @TO statistics@.  
    Example
      s=sample(ER(6,3,0.2),100);
      myStats = statistics(s, degree@@ideal);
      myStats.Mean 
    Text
      The last line returns the average degree of the 100 ideals in the sample $s$. 
  SeeAlso
    statistics 
///

doc ///
  Key
    StdDev
  Headline
    a summary statistic for a list of objects
  Description
    Text
      A sample standard deviation is -- along with sample mean -- 
      one of the basic summary statistics for a sample of randomly generated objects 
      (or any list of objects in general). 
      The standard deviation value statistic for some property of a list of objects is stored under the key StdDev in 
      the hash table returned by the function @TO statistics@.  
    Example
      s=sample(ER(6,3,0.2),100);
      myStats = statistics(s, degree@@ideal);
      myStats.StdDev 
    Text
      The last line returns the standard deviation of the degrees of the 100 ideals in the sample $s$. 
      Note that it is more meaningful to look at this value along with the mean value rather than as a stand-alone statistic.
  SeeAlso
    Mean
    statistics
///

doc ///
  Key
    Histogram
  Headline
    a summary statistic for a list of objects
  Description
    Text
      The symbol Histogram is used as a key in the hash table returned by the function @TO statistics@. 
      It one of the summary statistics for a given data sample. 
    Example
      s=sample(ER(6,3,0.2),100);
      myStats = statistics(s, dim@@ideal);
      myStats.Histogram 
  SeeAlso
    statistics
    ShowTally
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
    -- Check all possible values are returned with a probability of 1
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
  assert (0.===C_1)
  p={0.0,1.0,0.0};
  D = randomMonomialIdeals(n,D,p,N);
  E = mingenStats(D);
  assert (10.===E_0)
  assert (0.===E_1)
///

TEST ///
  -- check average degree complexity
  n=3; D=5; p=1.0; N=5;
  B = randomMonomialIdeals(n,D,p,N);
  C = mingenStats(B);
  assert(1.===C_2)
  assert(0.===C_3)
  p={0.0,0.0,0.0,0.0,1.0};
  D = randomMonomialIdeals(n,D,p,N);
  E = mingenStats(D);
  assert(5.===E_2)
  assert(0.===E_3)
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
  assert(0.==(pdimStats(listOfIdeals))_0)
  assert(0.==(pdimStats(listOfIdeals))_1)
  listOfIdeals={monomialIdeal(R_0,R_1,R_2)};
  assert(3.==(pdimStats(listOfIdeals))_0)
  assert(0.==(pdimStats(listOfIdeals))_1)
  listOfIdeals={monomialIdeal(0_R),monomialIdeal(R_0*R_1^2,R_1^3,R_2)};
  assert(1.5==(pdimStats(listOfIdeals))_0)
  assert(1.5==(pdimStats(listOfIdeals))_1)
  listOfIdeals={monomialIdeal(R_0^2*R_1,R_2)};
  assert(2.==(pdimStats(listOfIdeals))_0)
  assert(0.==(pdimStats(listOfIdeals))_1)
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
