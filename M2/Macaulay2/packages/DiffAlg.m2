-- -*- coding: utf-8 -*-

newPackage (
	"DiffAlg",
	Version => "1.5",
	Date => "October, 2018",
	Authors => {
		{ Name => "Manuel Dubinsky",
		  Email => "manudubinsky@gmail.com",
		  HomePage => ""},
		{ Name => "Cesar Massri",
		  Email => "cmassri@caece.edu.ar",
		  HomePage => ""},
		{ Name => "Ariel Molinuevo",
		  Email => "amoli@dm.uba.ar",
		  HomePage => ""},
		{ Name => "Federico Quallbrunn",
		  Email => "fquallb@dm.uba.ar",
		  HomePage => ""}
	},
	Headline => "specialized routines for differential forms",
	Keywords => {"Commutative Algebra"},
	Configuration => { 
		"BaseRing" => null,
		"VariableName" => "x",
		"DiffName" => "d",
		"FieldName" => "a"
	},
	Certification => {
	     "journal name" => "The Journal of Software for Algebra and Geometry",
	     "journal URI" => "http://j-sag.org/",
	     "article title" => "DiffAlg: a Differential algebra package",
	     "acceptance date" => "19 November 2018",
	     "published article URI" => "https://msp.org/jsag/2019/9-1/p02.xhtml",
	     "published article DOI" => "10.2140/jsag.2019.9.11",
	     "published code URI" => "https://msp.org/jsag/2019/9-1/jsag-v9-n1-x02-DiffAlg.m2",
	     "repository code URI" => "http://github.com/Macaulay2/M2/blob/master/M2/Macaulay2/packages/DiffAlg.m2",
	     "release at publication" => "fb0887a15f6ff5ec7f940f60ad46f738412924cd",	    -- git commit number in hex
	     "version at publication" => "1.5",
	     "volume number" => "9",
	     "volume URI" => "https://msp.org/jsag/2019/9-1/"
	     }
)

export {
	"newForm",
	"moduliIdeal",
	"singularIdeal",
	"logarithmicForm",
	"newField",
	"dist",
	"isInvolutive",
	"linearComb",
	"radial",
	"genKer",
	"genIm",
	"DiffAlgElement",
	"DiffAlgForm",
	"DiffAlgField",
	"DiffAlgDistribution",
	"projectivize"
	
}

DiffAlgElement = new Type of HashTable;
DiffAlgForm = new Type of DiffAlgElement;
DiffAlgField = new Type of DiffAlgElement;
DiffAlgDistribution = new Type of List;

LL := (options DiffAlg).Configuration#"BaseRing";
VAR := (options DiffAlg).Configuration#"VariableName";
VARD := (options DiffAlg).Configuration#"DiffName";
VARA := (options DiffAlg).Configuration#"FieldName";

QQi := QQ["i"];
if LL === null then LL = toField (QQi / (QQi_0^2+1))

net DiffAlgElement := Net =>  w -> pretty w#"f"
toString DiffAlgElement := String => w -> toString w#"f"
ring DiffAlgElement := Ring => w -> ring w#"f"
sub (DiffAlgElement,Ring) := RingElement => (w,R) -> sub(w#"f",R)
degree(DiffAlgElement) := List => w -> (
	deg := if w#"f" == 0 then {0,0} else degree(w#"f");
	n := (numgens ring w) - 1;
	if class w === DiffAlgForm then {n,deg_0,deg_1} else {n,deg_1}
)

RingElement * DiffAlgElement := DiffAlgElement => (c,e) -> new class e from {"f"=> sub(c,ring e#"f")*(e#"f")}
ZZ * DiffAlgElement := DiffAlgElement => (c,w) -> sub(c,ring w#"f")*w
QQ * DiffAlgElement := DiffAlgElement => (c,w) -> sub(c,ring w#"f")*w
DiffAlgElement * ZZ := DiffAlgElement => (w,c) -> c*w
DiffAlgElement * QQ := DiffAlgElement => (w,c) -> c*w
DiffAlgElement * RingElement := DiffAlgElement => (w,c) -> c*w
DiffAlgElement / ZZ := DiffAlgElement => (w,c) -> (1/c)*w
DiffAlgElement / QQ := DiffAlgElement => (w,c) -> (1/c)*w
DiffAlgElement / RingElement := DiffAlgElement => (w,c) -> (1/c)*w
- DiffAlgElement := DiffAlgElement => w -> (-1)*w
DiffAlgElement + DiffAlgElement := DiffAlgElement => (w,e) -> add(w,e)
DiffAlgElement - DiffAlgElement := DiffAlgElement => (w,e) -> w + (-e)
String | DiffAlgElement := String => (t,e) -> t|"("|toString e|")"
DiffAlgElement | String := String => (e,t) -> "("|toString e|")"|t

DiffAlgForm ^ DiffAlgForm := DiffAlgForm => (w,e) -> wedge(w,e)
DiffAlgForm * DiffAlgForm := DiffAlgForm => (w,e) -> w^e
DiffAlgField | DiffAlgField := DiffAlgField => (X,Y) -> bracket(X,Y)
List * DiffAlgForm := DiffAlgForm => (L,w) -> pullback(L,w)
DiffAlgForm _ DiffAlgField := DiffAlgForm => (w,X) -> contraction(w,X)
DiffAlgField _ DiffAlgForm := DiffAlgForm => (X,w) -> w_X

newElement = method();
newElement(List,String,Boolean) := DiffAlgElement => (L,varName,isForm) -> (
	n := L_0;
	r := L_1;
	d := L_2;
	x := getSymbol VAR;
	dx := getSymbol (if isForm then VARD|VAR else VARA|VAR);
	a := getSymbol varName;
	C := LL[a_0 .. a_(binomial(n+d,d) * binomial(n+1,r)-1)];
	W := C[x_0 .. x_n][dx_0 .. dx_n, SkewCommutative => isForm];
	w := ((basis(d,coefficientRing W) ** basis(r,W)) * transpose(vars(C)))_(0,0);
	new DiffAlgElement from {"f" => w}
)

newElement(String,Boolean) := DiffAlgElement => (expr,isForm) -> (
	x := getSymbol VAR;
	dx := getSymbol (if isForm then VARD|VAR else VARA|VAR);
	aux1 := separateRegexp ("[*+^()-/ i]",expr);
	aux2 := select(aux1,s->match(VAR|"_",s));
	aux2 = apply(aux2,s->replace(VAR,"",s));
	aux2 = apply(aux2,s->replace(VARD,"",s));
	aux2 = apply(aux2,s->replace(VARA,"",s));
	aux2 = apply(aux2,s->replace("_","",s));
	n := max (apply(aux2,value) | {0});
	varList := select(aux1,s->(not match("^[0-9]",s)) and (not match(VAR|"_",s)));
	varList = toList set select(varList,s->#s>0);

	T := (if #varList > 0 then LL[apply(varList,value)][x_0 .. x_n][dx_0 .. dx_n, SkewCommutative => isForm]
	else LL[x_0 .. x_n][dx_0 .. dx_n, SkewCommutative => isForm]);

	new DiffAlgElement from {"f" => sub(value(expr),T)}
)

newForm = method();
newForm(ZZ,ZZ,ZZ,String) := DiffAlgForm => (n,r,d,varName) -> new DiffAlgForm from newElement({n,r,d},varName,true)
newForm String := DiffAlgForm => expr -> new DiffAlgForm from newElement(expr,true)

newField = method();
newField(ZZ,ZZ,String) := DiffAlgField => (n,d,varName) -> new DiffAlgField from newElement({n,1,d},varName,false)
newField String := DiffAlgField => expr -> new DiffAlgField from newElement(expr,false)

wedge = method();
wedge(DiffAlgForm,DiffAlgForm) := DiffAlgForm => (w, e) -> (
	T := extendRing(w,e);
	new DiffAlgForm from {"f"=>sub(w#"f",T) * sub(e#"f",T)}
)

add = method();
add(DiffAlgElement,DiffAlgElement) := DiffAlgElement => (w, e) -> (
	if not uniform {w,e} then (print "ERROR: Add";return 0);
	T := extendRing(w,e);
	new class w from {"f" => sub(w#"f",T) + sub(e#"f",T)}
)

diff DiffAlgForm := DiffAlgForm => form -> (
	w := form#"f";
	x := getSymbol VAR;
	dx := getSymbol (VARD|VAR);
	n := numgens ring w;
	dw := for i in (flatten entries monomials w) list
		for j in 0..(n-1) list
			diff((x_j)_(ring w),coefficient(i,w))*(dx_j)_(ring w)*i;
	new DiffAlgForm from {"f"=>sub(sum flatten dw,ring w)}
)

logarithmicForm = method(Options => {Projective => false});
logarithmicForm(ZZ,List,String) := DiffAlgForm => o -> (n,l,varName) -> (
	F := for i in 0..#l-1 list newForm(n,0,l_i, varName | (i+1));
	x := getSymbol VAR;
	dx := getSymbol (VARD|VAR);
	a := getSymbol (varName | 0);
	C := LL[a_0..a_(#l-1)];
	R := C[x_0..x_(n-1)][dx_0..dx_(n-1),SkewCommutative => true];

	term := new DiffAlgForm from {"f"=>(a_0)_R};
	for j in drop(0..#l-1,{0,0}) do term = term ^ (F_j);
	out := term ^ (diff(F_0));
	for i in 1..#l-1 do (
		term := new DiffAlgForm from {"f"=>(a_i)_R};
		for j in drop(0..#l-1,{i,i}) do term = term ^ (F_j);
		out = out + term ^ (diff(F_i));
	);

	if o#Projective then (
		proj := 0_C;
		for i in 1..#l-1 do proj = proj + (a_i)_C*l_i/l_0;
		ccr := coefficientRing coefficientRing ring out;
		newForm toString (sub(out#"f",{(a_0)_ccr => sub(-proj,ccr)}))
	) else out
)


linearComb = method();
linearComb(List,String) := DiffAlgElement => (L,varName) -> (
	if not all(L,s->instance(s,DiffAlgElement)) or not uniform L then (print "ERROR: linearComb";return 0;);

	x := getSymbol VAR;
	dx := getSymbol (if class L_0 === DiffAlgForm then VARD|VAR else VARA|VAR);
	a := getSymbol varName;
	n := max apply(L,s->(degree s)_0);

	eAux := newField(#L-1,0,varName);
	T := extendRing(L|{eAux});
	C := coefficientRing coefficientRing ring eAux;

	new class L_0 from {"f" => sum apply(gens C,L,(i,j)->sub(i,T)*sub(j,T))}
)

pullback = method();
pullback(List,DiffAlgForm) := DiffAlgForm => (L,w) -> (
	x := getSymbol VAR;
	dx := getSymbol (VARD|VAR);
	if not uniform L or class L_0 =!= DiffAlgForm or 
		numgens ring w != #L or sum apply(L,s->(degree s)_1) != 0 then (print "ERROR: Pull-Back";return 0;);
	T := extendRing(flatten {w,L});

	hash := flatten for i in 0..#L-1 list
		{(x_i)_(coefficientRing T)=>sub(L_i,T), (dx_i)_T => sub(diff(L_i),T)};

	new DiffAlgForm from {"f" => sub(sub(w,T),hash)}
)

extendRing = method();
extendRing(DiffAlgElement,DiffAlgElement) := Ring => (w,e) -> extendRing({w,e})
extendRing List := Ring => L -> (
	x := getSymbol VAR;
	dx := getSymbol (if class L_0 === DiffAlgForm then VARD|VAR else VARA|VAR);

	ccr := coefficientRing coefficientRing ring L_0#"f";
	varsT := gens ccr;
	maxGens := numgens ring L_0#"f";
	for j in 1..#L-1 do (
		varsJ := gens coefficientRing coefficientRing ring L_j#"f";
		if (maxGens < numgens ring L_j#"f") then maxGens = numgens ring L_j#"f";
		for i in varsJ do if sub(i,LL[varsT]) == 0 then varsT = append(varsT,i);
	);

	if #varsT == 0 then LL[x_0 .. x_(maxGens-1)][dx_0 .. dx_(maxGens-1), SkewCommutative => class L_0 === DiffAlgForm]
	else LL[varsT][x_0 .. x_(maxGens-1)][dx_0 .. dx_(maxGens-1), SkewCommutative => class L_0 === DiffAlgForm]
)

radial = method();
radial ZZ := DiffAlgField => n -> (
	x := getSymbol VAR;
	ax := getSymbol (VARA|VAR);
	W := LL[x_0 .. x_n][ax_0 .. ax_n];
	new DiffAlgField from {"f" => (basis(1,coefficientRing W)*transpose(basis(1,W)))_(0,0)}
)

contraction = method();
contraction(DiffAlgForm,DiffAlgField) := DiffAlgForm => (wF,X) -> ( 
	x := getSymbol VAR;  
	dx := getSymbol (VARD|VAR);
	ax := getSymbol (VARA|VAR);
	T := extendRing(wF,X);
	maxGens := numgens T;
	aux :=	newField("ax_" | (maxGens - 1));
	X = X + aux - aux;
	w := sub(wF#"f",T);
	out := 0_T;
	for j in (flatten entries monomials w) do (
		monomialExp := (listForm(j))_0_0;
		sign := 1_T;
		for i in 0..maxGens-1 do
			if (monomialExp_i==1) then (
				newTerm := sub(j,{(dx_i)_T => sub(coefficient((ax_i)_(ring X),X#"f"),T)});
				out = out + coefficient(j,w)*newTerm*sign;
				sign = sign * (-1)_T;
			);
	);
	new DiffAlgForm from {"f" => out}
)

bracket = method();
bracket(DiffAlgField,DiffAlgField) := DiffAlgField => (XF,YF) -> (
	x := getSymbol VAR;  
	ax := getSymbol (VARA|VAR);
	T := extendRing(XF,YF);
	maxGens := numgens T;
	X := sub(XF#"f",T);
	Y := sub(YF#"f",T);
	out := sum flatten for i in 0..maxGens-1 list
		for j in 0..maxGens-1 list
			(coefficient((ax_j)_(ring X),X)*diff((x_j)_(ring Y),coefficient((ax_i)_(ring Y),Y))-
			coefficient((ax_j)_(ring Y),Y)*diff((x_j)_(ring X),coefficient((ax_i)_(ring X),X)))*(ax_i)_(T);
	new DiffAlgField from {"f" => out}
)

random DiffAlgElement := DiffAlgElement => o -> elem -> random(elem,ZZ,Density => o#Density, Height => o#Height)
random(DiffAlgElement,Ring) := DiffAlgElement => o -> (elem,R) -> (
	C := coefficientRing coefficientRing ring elem;
	if numgens(C) == 0 then return elem;

	L := fillMatrix(mutableMatrix(R,1,numgens C), Density => o#Density, Height => o#Height) - 
		fillMatrix(mutableMatrix(R,1,numgens C), Density => o#Density, Height => o#Height);
	w := sub(elem#"f",apply(gens C,flatten entries L,(i,j)->i=>j));
	if w == 0 then (print "ERROR: Random";);

	n := numgens ring elem;
	x := getSymbol VAR;
	dx := getSymbol (if class elem === DiffAlgForm then VARD|VAR else VARA|VAR);
	T := LL[x_0..x_(n-1)][dx_0..dx_(n-1),SkewCommutative => class elem === DiffAlgForm];

	new class elem from {"f" => sub(w,T)}
)


singularIdeal = method();
singularIdeal DiffAlgElement := Ideal => elem -> (
	w := elem#"f";
	l := for i in (flatten entries monomials w) list coefficient(i,w);
	sub(ideal flatten l,coefficientRing ring w)
)

moduliIdeal = method();
moduliIdeal DiffAlgElement := Ideal => elem -> (
	w := elem#"f";
	l := for i in (flatten entries monomials w) list
		for j in (flatten entries monomials(coefficient(i,w))) list
			coefficient(j,coefficient(i,w));
	sub(ideal flatten l,coefficientRing coefficientRing ring w)
)


homogenize DiffAlgElement := DiffAlgElement => e -> ( 
	n := (degree e)_0;
	x := getSymbol VAR;
	dx := getSymbol (if class e === DiffAlgForm then VARD|VAR else VARA|VAR);
	R := (coefficientRing coefficientRing ring e)[dx_0..dx_(n+1),x_0..x_(n+1)];
	c := max apply(terms sub(e,R),s->(degree s)_0);

	s := concatenate (for i in terms(sub(e,R)) list "+("| toString i | ")*"|VAR|"_"|n+1|"^"|c-((degree i)_0));

        if class e === DiffAlgForm then (
                newForm s
        ) else newField(s)
)

projectivize = method();
projectivize DiffAlgElement := DiffAlgElement => e -> ( 
	n := (degree e)_0;
	x := getSymbol VAR;
	dx := getSymbol (if class e === DiffAlgForm then VARD|VAR else VARA|VAR);
	R := (coefficientRing coefficientRing ring e)[dx_0..dx_(n+1),x_0..x_(n+1)];
	c := max apply(terms sub(e,R),s->(degree s)_0);

	s := concatenate (for i in terms(sub(e,R)) list "+("| toString i | ")*"|VAR|"_"|n+1|"^"|c-((degree i)_0));

        if class e === DiffAlgForm then (
		r := radial (n+1);
		aux := (newForm s)_r;
		if aux#"f" == 0 then newForm s else
			newForm(VAR|"_"|n+1|"*("|s|")-"|VARD|VAR|"_"|n+1|"*"|aux)
	) else newField(s)
)


isHomogeneous DiffAlgElement := Boolean => e-> (
	n := (degree e)_0;
	x := getSymbol VAR;
	dx := getSymbol (if class e === DiffAlgForm then VARD|VAR else VARA|VAR);
	R := (coefficientRing coefficientRing ring e)[dx_0..dx_(n+1),x_0..x_(n+1)];
	c := max apply(terms sub(e,R),s->(degree s)_0);
	all(terms(sub(e,R)),i -> (degree i)_0 == c)
)


genKer = method();
genKer (DiffAlgElement,DiffAlgElement) := List => (expr,var) -> (
	x := getSymbol VAR;
	dx := getSymbol (if class var === DiffAlgForm then VARD|VAR else VARA|VAR);

	C := coefficientRing coefficientRing ring expr;
	B := coefficientRing coefficientRing ring var;
	otherVars := reverse sort toList (set(gens C) - set(apply(gens B,s->sub(s,C))));
	cT := (if #otherVars > 0 then LL[otherVars] else LL);

	n := numgens ring expr;
	R := cT[x_0..x_(n-1)][dx_0..dx_(n-1),SkewCommutative => class var === DiffAlgForm];
	T := R[gens B];
	TAux := LL[gens ring expr|gens coefficientRing ring expr][gens coefficientRing coefficientRing ring expr];
	if (expr#"f" == 0) or ((degree sub(expr,TAux))_0 != 1) then ( 
		print "ERROR: genKer, expression must be linear and non-zero";
		return {};
	);

	I := sub(moduliIdeal expr,T);
	if ideal gens gb I == 1 then (
		print "ERROR: genKer, the system has no solutions";
		return {};
	);
	Bt := apply(gens B,s->sub(s,T));
	L := apply(Bt,s -> s => sub(s % I,T));
	L0 := apply(Bt,s -> s => sub(s % I,T) % (ideal Bt)); 
	vAux := sub(sub(var,T),L);
	vP := sub(vAux,L0);
	v := vAux - vP;

	J := ideal apply(Bt, s -> coefficient(s,v));
	d := degree var;
	if #d == 2 then d = {d_0,1,d_1};
	A := apply(flatten entries super basis({d_1,d_2},J), s->new class var from {"f" => s});
	A = {(if #A == 0 then {var-var} else A), new class var from {"f" => sub(vP,R)}};
	if vP == 0 then first A else A
)

genIm = method();
genIm (DiffAlgElement,DiffAlgElement) := List => (expr,var) ->	(
	x := getSymbol VAR;
	dx := getSymbol (if class expr === DiffAlgForm then VARD|VAR else VARA|VAR);

	C := coefficientRing coefficientRing ring expr;
	B := coefficientRing coefficientRing ring var;
	otherVars := reverse sort toList (set(gens C) - set(apply(gens B,s->sub(s,C))));
	cT := (if #otherVars > 0 then LL[otherVars] else LL);

	n := numgens ring expr;
	R := cT[x_0..x_(n-1)][dx_0..dx_(n-1),SkewCommutative => class expr === DiffAlgForm];
	T := R[gens B];
	wAux := expr#"f";
	for i in gens B do wAux = sub(wAux,{sub(i,C) => 0});
	if (wAux != 0) or (expr#"f" == 0) or ((degree sub(expr,T))_0 != 1) then (
		print "ERROR: genIm, expression must be non-zero and homogeneous";
		return {};
	);

	J := ideal apply(gens B, s -> coefficient(sub(s,T),sub(expr,T)));
	d := degree expr;
	if #d == 2 then d = {d_0,1,d_1};
	A := apply(flatten entries super basis({d_1,d_2},J), s->new class expr from {"f" => s});
	if #A == 0 then {expr-expr} else A
)

dist = method();
dist List := DiffAlgDistribution => L -> (
	new DiffAlgDistribution from if (not uniform L or class L_0 =!= DiffAlgField) then (
		print "ERROR: dist, the list must contain vector fields.";
		{}
	) else L
)

isInvolutive = method();
isInvolutive DiffAlgDistribution := Boolean => L -> (
	T := extendRing L;
	I := ideal apply(L, s->sub(s,T));
	all(L,L, (i,j) -> (sub(i|j,T)%I) == 0)
)


rank DiffAlgDistribution := ZZ => L -> (
	T := extendRing L;
	v := flatten entries vars T;
	mat := matrix for i in L list (
		w := sub(i,T);
		for j in v list coefficient(j,w)
	);
	dr := #L;
	while (minors(dr,mat) == 0) and (dr > 1) do dr = dr - 1;
	dr
)


-------------------
---DOCUMENTATION---
-------------------

beginDocumentation()

document {
  Key => DiffAlg,
  Headline => "differential algebra",
  PARA {TO DiffAlg, " is a differential algebra package. It can compute the usual operations with polynomial differential forms and vector fields. Its main purpose is to associate algebraic objects to differential operators in the exterior algebra of differential forms."},
  PARA {"The simplest way to load the package is with the command:"},
  TT {"loadPackage \"DiffAlg\""},
  PARA {"Then, one can define a linear differential 1-form, ", TT "w", ", and the radial vector field, ", TT "R", ", in  3-dimensional space as:"},
  EXAMPLE lines ///
	w = newForm(2,1,1,"a")
	R = radial 2
	ring w
	ring R
  ///,
  BR{},
  PARA {"All possible options to call the package can be given with the command:"},
  TT {"loadPackage (\"DiffAlg\",Configuration => {\"BaseRing\" => aRing, \"VariableName\" => varSymbol, \"DiffName\" => difSymbol, \"FieldName\" => derSymbol})"},
  PARA {"where:"},
  UL {{TT {"aRing"},", a ", TO Ring, ", the base ring. Default ", TT {"QQ[i]"}},{TT {"varSymbol"}, ", a ", TO String, ", the name of the affine coordinates. Default ", TT{"x"}},{TT {"difSymbol"}, ", a ", TO String, ", the symbol to denote the differential of a coordinate. Default ", TT {"d"}},{TT {"derSymbol"}, ", a ", TO String, ", the symbol to denote the partial derivative of a coordinate. Default ", TT {"a"}}},
  BR{},
  Caveat => PARA {"It is recommended to operate in low degrees and dimensions because of the computational time needed to handle the number of variables generated in every degree."},
  SeeAlso => {newForm, newField}
}

document {
  Key => DiffAlgElement,
  Headline => "the class of all differential forms and vector fields",
}
document {
  Key => DiffAlgForm,
  Headline => "the class of all differential forms",
  SeeAlso => {DiffAlgField}
}
document {
  Key => DiffAlgField,
  Headline => "the class of all vector fields",
  SeeAlso => {DiffAlgForm}
}
document {
  Key => DiffAlgDistribution,
  Headline => "the class of distributions of vector fields",
}

document {
  Key => {newField,(newField,ZZ,ZZ,String)},
  Headline => "constructor of a vector field",
  Usage => "newField(n,d,varName)",
  Inputs => {
	"n" => ZZ => {"number of variables minus one"},
	"d" => ZZ => {"degree of the homogeneous polynomial coefficients"},
	"varName" => {"name of the generic scalar coefficients"}
  },
  Outputs => {
	DiffAlgField => {"a homogeneous vector field in (n+1)-dimensional affine space with generic scalar coefficients"}
  },
  PARA {"This function defines homogeneous vector fields with generic scalar coefficients. By default, the affine coordinates will be ", TT {"x_0,...,x_n"}, " and the partial derivatives are denoted as ", TT {"ax_0,...,ax_n"}, ", respectively."},
  BR{},
  PARA {"In this example we define a homogeneous vector field with linear polynomial coefficients in 3 variables. The scalar coefficients are chosen to be defined with the variable a. The index of the scalar coefficients will always start with 0."},
  EXAMPLE lines ///
	X = newField(2,2,"a")
	ring X
  ///,
  Caveat => {"The coefficient ", TT {"i"}, " is the imaginary unit."},
  SeeAlso => {(newField,String),newForm}
}

document {
  Key => (newField,String),
  Usage => "newField(expression)",
  Inputs => {
	"expression" => String => {"the expression to be evaluated"}
  },
  Outputs => {
	DiffAlgField => {"the vector field written in expression"}
  },
  PARA {"This function defines the particular vector field written in the given expression as elements of type ", TO DiffAlgField, ". If any parameters are founded in the given expression, they are automatically included in the ring of scalar coefficients."},
  BR{},
  PARA {"In the following example we define two particular vector fields, ", TT {"X"}, " and ", TT {"Y"}, ", and compute the addition ", TT {"X+Y"}, ". Notice that in the definition of ", TT {"X"}, " we are introducing a scalar parameter named ", TT {"a"}, ", also the variable ", TT {"x_2"}, " is missing from the ring of ", TT {"X"}, ". When computing ", TT {"X+Y"}, ", the rings of both vector fields are automatically merged."},  
  EXAMPLE lines ///
	X = newField("2*a*x_0*ax_1")
	ring X
	Y = newField("x_0*ax_2")
	ring Y
	X+Y
	ring (X+Y)
  ///,
  PARA {"In this example we show that the variables will always start from the index 0 and go up to the highest index encountered in the expression defining the vector field."},  
  EXAMPLE lines ///
	Z = newField("ax_5")
	ring Z
  ///,
  Caveat => {"By default, the affine coordinates will be ", TT {"x_0,...,x_n"}, " and the partial derivatives are denoted as", TT {" ax_0,...ax_n"}, ", respectively. The coefficient ", TT {"i"}, " is the imaginary unit."},
  SeeAlso => {(newField,ZZ,ZZ,String), newForm}
}

document {
  Key => {newForm,(newForm,ZZ,ZZ,ZZ,String)},
  Headline => "constructor of a differential form",
  Usage => "newForm(n,r,d,varName)",
  Inputs => {
	"n" => ZZ => {"number of variables minus one"},
	"r" => ZZ => {"degree of the differential form"	},
	"d" => ZZ => {"degree of the polynomial coefficients of the differential form"},
	"varName" => String => {"name of the generic scalar coefficients of the differential form"}
  },
  Outputs => {
	DiffAlgForm => {"a homogeneous differential r-form in (n+1)-dimensional  affine space with polynomial coefficients of degree ", TT {"d"}, ""}
  },
  PARA {"This function defines homogeneous differential forms with generic scalar coefficients. By default, the affine coordinates will be ", TT {"x_0,...,x_n"}, " and their exterior derivatives are denoted as ", TT {"dx_0,...,dx_n"}, ", respectively."},
  BR{},
  PARA {"In this example we define a homogeneous differential 1-form with linear polynomial coefficients in 3 variables. The scalar coefficients are chosen to be defined with the variable ", TT {"a"}, ". The index of the scalar coefficients will always start with 0."},
  EXAMPLE lines /// 
	w = newForm(2,1,1,"a")
	ring w
  ///,
  Caveat => {"The coefficient ", TT {"i"}, " is the imaginary unit."},
  SeeAlso => {(newForm,String), newField}
}

document {
  Key => (newForm,String),
  Usage => "newForm(expression)",
  Inputs => {
	"expression" => String => {"the expression to be evaluated"}
  },
  Outputs => {
	DiffAlgForm => {"the differential form written in expression"}
  },
  PARA {"This function defines the particular differential form written in the given expression as elements of type ", TO DiffAlgForm, ". Notice that the exterior product must be written as the ordinary product of variables ", TT {"*"}, ". If any parameters are founded in the given expression, they are automatically included in the ring of scalar coefficients."},
  BR{},
  PARA {"In the following example we define two particular differential forms, ", TT {"w"}, " and ", TT {"z"}, ", and compute the exterior product ", TT {"w^z"}, ", see ", TO "DiffAlgForm ^ DiffAlgForm", ". In the definition of ", TT {"w"}, " we are introducing a scalar parameter named ", TT {"a"}, ". Notice that the variable ", TT {"x_2"}, " is missing from the ring of ", TT {"w"}, ". But when computing ", TT {"w^z"}, ", the rings of both vector fields are automatically merged."},  
  EXAMPLE lines ///
	w = newForm("a * x_1 * dx_0 * dx_1")
	ring w
	z = newForm("x_0^2 * dx_2 - x_2^2 * dx_0")
	ring z
	w ^ z
	ring (w+z)
  ///,
  PARA {"In this example we show that the variables will always start from the index 0 and go up to the highest index encountered in the expression defining the differential form."},  
  EXAMPLE lines ///
	v = newForm("dx_5")
	ring v
  ///,
  Caveat => {"By default, the affine coordinates will be ", TT {"x_0,...,x_n"}, " and the differentials are denoted as ", TT {"dx_0,...,dx_n"}, ", respectively. The coefficient ", TT {"i"}, " is the imaginary unit."},
  SeeAlso => {newField, (newForm,ZZ,ZZ,ZZ,String)}
}

document {
  Key => {moduliIdeal,(moduliIdeal,DiffAlgElement)}, 
  Headline => "ideal generated by the coefficients of a differential form or vector field",
  Usage => "moduliIdeal(e)",
  Inputs => {
	"e" => DiffAlgElement => "a differential form or vector field",
	},
  Outputs => {
	Ideal => {"the ideal generated by the scalar coefficients of ", TT {"e"}, ""}
  },
  PARA {"Given a differential form or vector field, this routine returns the ideal generated by the scalar coefficients of such element."},
  BR{},
  PARA {"In this example we compute the equations that the scalar coefficients of a closed differential 1-form must satisfy."},
  EXAMPLE lines ///
	w = newForm(2,1,2,"a")
	diff w
	moduliIdeal(diff w)
  ///,
  SeeAlso => {singularIdeal}
}

document {
  Key => {singularIdeal,(singularIdeal,DiffAlgElement)}, 
  Headline => "ideal generated by the polynomial coefficients of a differential form or vector field",
  Usage => "singularIdeal(e)",
  Inputs => {
	"e" => DiffAlgElement => "a differential form or vector field"
  },
  Outputs => {
	Ideal => {"the ideal generated by the polynomial coefficients of ", TT {"e"}, ""}
  },
  PARA {"Given a differential form or vector field, this routine returns the ideal generated by the polynomial coefficients of such element."},
  BR{},
  PARA {"In this example we compute the singular locus of a differential form ", TT {"w"}, "."},
  EXAMPLE lines ///
	w = random newForm(2,1,2,"a")
	singularIdeal(w)
  ///,
  PARA {"This routine is usefull to obtain the ", TO RingElement, " representing a 0-form"},
  EXAMPLE lines ///
	w = random newForm(2,1,2,"a");
	r = radial 2;
	F = r_w
	degree F
	(gens singularIdeal F)_0_0
  ///,
  SeeAlso => {moduliIdeal}
}

document {
  Key => {logarithmicForm, (logarithmicForm,ZZ,List,String)},
  Headline => "creates a logarithmic form",
  Usage => "logarithmicForm(n,L,varName)",
  Inputs => {
	"n" => ZZ => {"number of variables minus one"},
	"L" => List => {"list of degrees"},
	"varName" => String => {"name of the coefficients"},
	Projective => Boolean => {"whether to create a logarithmic form that descends to projective space"}
  },
  Outputs => {
	DiffAlgForm => {"a generic logarithmic form"}
  },
  PARA {"A logarithmic form of type ", TT {"(d_0,...,d_n)"}, " is a differential 1-form ", TT {"w"}, " that can be written as ", TT {"w=(prod f_i)sum df_i/f_i"}, ", where ", TT {"f_i"}, " is a polynomial of degree ", TT {"d_i"}, ". This routine creates such a logarithmic form using homogeneous polynomials. When using a list ", TT {"L"}, " of length two, the differential form is called rational."},
  BR{},
  PARA {"In this example we generate a random logarithmic form in affine 3-dimensional space with degrees ", TT {"(1,1,2)"}, "."},
  EXAMPLE lines ///
	random logarithmicForm(2,{1,1,2},"a")
  ///,
  PARA {"In this example we generate a generic rational form in the projective plane of type ", TT {"(1,1)"}, "."},
  EXAMPLE lines ///
	logarithmicForm(2,{1,1},"a",Projective => true)
  ///,
  PARA {"In the following example, we produce a logarithmic form that descends to projective space."},
  EXAMPLE lines ///
	l = random logarithmicForm(2,{1,1},"a",Projective => true)
	(radial 2)_l
  ///
}

document {
  Key => [logarithmicForm, Projective],
  Headline => "a boolean option to produce a projective logarithmic form",
  Usage => "logarithmicForm(..., Projective => b)",
  Inputs => {
	"b" => Boolean => "if true, it return a generic projective logarithmic form"
  },
  EXAMPLE lines ///
	l = logarithmicForm(2,{1,1},"a",Projective => true)
	(radial 2)_l
  ///
}

document {
  Key => {(random,DiffAlgElement,Ring),(random,DiffAlgElement)},
  Headline => "replaces the variables of the coefficient ring of a differential form or a vector field with random values",
  Usage => "random(e,R)",
  Inputs => {
	"e" => DiffAlgElement => "a differential form or vector field with generic coefficients",
	"R" => Ring => {"the ring where the random values are taken from. Default value is ",ofClass ZZ},
  },
  Outputs => {
	DiffAlgElement => {"the differential form or vector field ", TT {"e"}, " whose variables of the coefficient ring were evaluated at random values"}
  },
  EXAMPLE lines ///
	random newForm(2,2,1,"a")
	random(newField(2,2,"a"),QQ)
  ///,
  PARA {"Options ", TT "Density", " and ", TT "Height", " are implemented"},
  EXAMPLE lines ///
	random(newForm(2,2,1,"a"),Density => .2)
	random(newForm(2,2,1,"a"),Height => 100)
  ///,
  Caveat => {"This routine depends on the Macaulay2 method ", TO{random}, " which is not implemented in some rings. The option ", TO{[random,Density]}, " applied to a non-generic form (such us 2a logarithmic form) may return a form equal to 0"}
}

document {
  Key => {dist,(dist,List)},
  Headline => "produces a DiffAlgDistribution from a list",
  Usage => "dist(L)",
  Inputs => {
	"L" => List => {"list of vector fields, see ", TO DiffAlgField},
  },
  Outputs => {
	DiffAlgDistribution => {"generated by the vector fields in the given list."}
  },
  PARA {"This command checks that all the elements in the list are vector fields."},
  EXAMPLE lines ///
	X = newField("3*x_0*ax_0+x_1*ax_1")
	Y = radial 3
	dist {X,Y}
  ///,
  SeeAlso => {newField, radial, (rank,DiffAlgDistribution), isInvolutive}
}

document {
  Key => {isInvolutive,(isInvolutive,DiffAlgDistribution)},
  Headline => "tests if a distribution is involutive",
  Usage => "isInvolutive(L)",
  Inputs => {
	"L" => DiffAlgDistribution => {"as given by the output of ", TO dist},
  },
  Outputs => {
	Boolean => {"true if the distribution L is involutive"}
  },
  PARA {"If ", TT {"L"}, " is a list of vector fields, this routine tests the involutivity of ", TT {"L"}, "."},
  BR{},
  PARA {"In this example we test the involutivity of two vector fields."},
  EXAMPLE lines ///
	X = newField("3*x_0*ax_0+x_1*ax_1")
	Y = radial 3
	isInvolutive dist {X,Y}
  ///,
  PARA {"In this example we compute a basis of the annihilator of a random projective logarithmic differential 1-form. Then we verify that it is an involutive distribution."},
  EXAMPLE lines ///
	w = random logarithmicForm(2,{1,2},"a",Projective => true)
	X = newField(2,2,"a")
	D = genKer(X_w,X);
	#D
	isInvolutive dist D
  ///,
  SeeAlso => {newField,radial,(rank,DiffAlgDistribution)}
}


document {
  Key => {(isHomogeneous,DiffAlgElement)},
  Headline => "tests if a form (or field) is homogeneous",
  Usage => "isHomogeneous(e)",
  Inputs => {
	"e" => DiffAlgElement => {"representing a form or a field"},
  },
  Outputs => {
	Boolean => {"true if the form (or field) is homogeneous"}
  },
  PARA {"In this example we test if a vector field is homogeneous,"},
  EXAMPLE lines ///
	X = newField("3*ax_0+x_1*ax_1")
	isHomogeneous X
  ///,
  PARA {"In this example we test if a projective logarithmic differential 1-form is homogeneous,"},
  EXAMPLE lines ///
	w = random logarithmicForm(2,{1,2},"a",Projective => true)
	isHomogeneous w
  ///,
  SeeAlso => {homogenize}
}

document {
  Key => (rank,DiffAlgDistribution),
  Headline => "rank of the given distribution",
  Usage => "rank(L)",
  Inputs => {
	"L" => DiffAlgDistribution => {"list of vector fields, see ", TO dist}
  },
  Outputs => {
	ZZ => {"the rank of the distribution generated by L"}
  },
  PARA {"This routine returns the rank of the distribution ", TT "L","."},
  BR{},
  PARA {"In this example we generate two random vector fields in three variables with polynomial coefficients of degree 2. Then we compute the rank of some distributions generated with them."},
  EXAMPLE lines ///
	X = random newField(2,2,"a")
	Y = random newField(2,2,"a")
	rank dist {X,Y}
	rank dist {X,Y,X+Y,X-Y}
	rank dist {X,Y,X|Y}
  ///,
  SeeAlso => {newField,radial,isInvolutive}
}

document {
  Key => {linearComb,(linearComb,List,String)},
  Headline => "generic linear combination of elements",
  Usage => "linearComb(L,varName)",
  Inputs => {
        "L" => List => {"list of vector fields or differential forms, see ", TO DiffAlgField, " or ", TO DiffAlgForm},
        "varName" => String => {"name of the generic coefficient"},
  },
  Outputs => {
        DiffAlgElement => {"generic linear combination of the list ", TT {"L"}, ""}
  },
  PARA {"This routine produce a generic linear combination of the elements in ", TT {"L"}, ". It can be used together with ", TO genKer, " or ", TO genIm, " to solve a system of homogeneous linear equations."},
  BR{},
  PARA {"In this example we compute a generic and a particular linear combination of two particular differential 2-forms."},
  EXAMPLE lines ///
        w = random newForm(2,1,2,"a")
        h = random newForm(2,1,2,"a")
        linearComb({w,h},"a")
        random oo
  ///,
  PARA {"In this example we compute a generic differential 1-form that descends to the projective plane. Then, we impose another linear condition."},
  EXAMPLE lines ///
        w = newForm(2,1,2,"a");
	h = random newForm(2,2,1,"a");
        L = genKer( (radial 2) _ w,w)
        wr = linearComb(L,"a")
	genKer(h ^ wr, wr)
  ///,
  SeeAlso => {newField,newForm,random,genKer,genIm}
}

document {
  Key => {radial,(radial,ZZ)},
  Headline => "defines the radial vector field",
  Usage => "radial n",
  Inputs => {
	"n" => ZZ => "number of variables minus one"
  },
  Outputs => {
	DiffAlgField => {"the radial vector field in the n-dimensional projective space"}
  },
  PARA {"This function defines the radial field vector field in (n+1)-variables."},
  EXAMPLE lines ///
	radial 2
  ///
}


document {
  Key => {genKer,(genKer,DiffAlgElement,DiffAlgElement)},
  Headline => "basis of the kernel of a linear expression",
  Usage => "gerKer(expr,var)",
  Inputs => {
	"expr" => DiffAlgElement => {"an expression linear in the variable ", TT "var"},
	"var" => DiffAlgElement => {"this is the variable of the linear expression, it must have free and linear scalar coefficients"}
  },
  Outputs => {
	List => {"basis of the kernel of the linear expression"}
  },
  SeeAlso => {genIm},
  PARA {"This routine returns a basis of the kernel of ", TT "expr",", an homogeneous expression linear in ", TT "var","."},
  BR{},
  PARA {"In the case of a non-homogeneous linear expression, this routine returns a pair having in the first coordinate a basis of the kernel of the associated homogeneous linear expression and in the secod coordinate a particular solution."},
  BR{},
  PARA {"In the first example, we compute a basis of projective differential 1-forms in projective 3-space with polynomial coefficients of degree 1. Then, we define a random rational differential form of type ", TT {"(1,1)"}, " and compute its tangent directions using the generic projective form defined before."},
  BR{},
  PARA {"In the second example, we compute a particular solution of a non-homogeneous linear expression."},
  EXAMPLE lines ///
	h = newForm(4,1,1,"a")
	R = radial 4
	T = genKer(R _ h,h)
	H = linearComb(T,"a")
	w = random logarithmicForm(4,{1,1},"a", Projective => true)
	genKer(w ^ (diff H) + (diff w) ^ H,H)
  ///,
  EXAMPLE lines ///
        w1 = random newForm(4,1,1,"a");
        w2 = random newForm(4,1,1,"a");
        w3 = w1 ^ w2;
        h = newForm(4,1,1,"a");
        last genKer(w1 ^ h - w3,h)
  ///
}

document {
  Key => {genIm,(genIm, DiffAlgElement,DiffAlgElement)},
  Headline => "a basis of the image of a linear expression",
  Usage => "genIm(expr,var)",
  Inputs => {
	"expr" => DiffAlgElement => {"an expression linear in the variable ", TT "var"},
	"var" => DiffAlgElement => {"this is the variable of the linear expression, it must have free and linear scalar coefficients"}
  },
  Outputs => {
	List => {"a basis of the image of the linear expression"}
  },
  SeeAlso => {genKer},
  PARA {"This routine returns a basis of the image of ", TT "expr",", an homogeneous expression linear in ", TT "var", "."},
  BR{},
  PARA {"In this example we compute a basis of the image of the derivative of a projective differential 1-forms."},
  EXAMPLE lines ///
	h = newForm(2,1,2,"a")
	R = radial 2
	H = linearComb(genKer(R _ h, h),"a")
	genIm(diff H,H)
  ///,
  PARA {"It is possible to get a linearly independent set of elements using this routine:"},
  EXAMPLE lines ///
        w1=random newForm(2,1,2,"a");
        w2=random newForm(2,1,2,"a");
        w3=w1+w2;
        u=linearComb({w1,w2,w3},"a");
        genIm(u,u)
        #oo
  ///
}

document {
  Key => (symbol +, DiffAlgElement, DiffAlgElement),
  Headline => "addition",
  Usage => "w + h",
  Inputs => {
	"w" => DiffAlgElement => "a differential form or vector field",
	"h" => DiffAlgElement => "a differential form or vector field"
  },
  Outputs => {
	DiffAlgElement => {"the addition of ", TT {"w"}, " and ", TT {"h"}}
  },
  EXAMPLE lines ///
	h = radial 2
	w = random newField(3,1,"a")
	w + h
  ///
}
document {
  Key => (symbol -, DiffAlgElement, DiffAlgElement),
  Headline => "subtraction",
  Usage => "w - h",
  Inputs => {
	"w" => DiffAlgElement => "a differential form or vector field",
	"h" => DiffAlgElement => "a differential form or vector field",
  },
  Outputs => {
	DiffAlgElement => {"the subtraction of ", TT {"w"}, " and ", TT {"h"}}
  },
  EXAMPLE lines ///
	w = newForm(2,1,1,"a")
	h = newForm(3,2,1,"b")
	w - h
  ///
}
document {
  Key => {(symbol ^, DiffAlgForm, DiffAlgForm),(symbol *, DiffAlgForm, DiffAlgForm)},
  Headline => "exterior product",
  Usage => "w ^ h",
  Inputs => {
	"w" => DiffAlgForm => "a differential form",
	"h" => DiffAlgForm => "a differential form"
  },
  Outputs => {
	DiffAlgForm => {"the exterior product of ", TT {"w"}, " and ", TT {"h"}}
  },
  PARA {"This function computes the exterior product of two differential forms."},
  EXAMPLE lines ///
	w = newForm(2,1,2,"a")
	h = newForm(2,1,1,"b")
	w ^ h
  ///
}

document {
  Key => (symbol *, List, DiffAlgForm),
  Headline => "pull-back of a differential form by a rational map",
  Usage => "L * w",
  Inputs => {
	"L" => List => {"the rational map represented by a list of polynomials as 0-forms, see ", TO DiffAlgForm},
	"w" => DiffAlgForm => "a differential form"
  },
  Outputs => {
	DiffAlgForm => {"the pull-back of ", TT {"w"}, " via ", TT {"L"}}
  },
  PARA {"Given a list of polynomials ", TT {"F = (F_0,...,F_n)"}, " and a differential form ", TT {"w"}, " on  n+1 variables, the pull-back ", TT {"F*w"}, " is defined as the composition ", TT {"w(F)"}, "."},
  BR{},
  PARA {"In this example we compute the pull-back of the 1-differential form ", TT {"w"}, " with respect to the mapping ", TT {"F = (F_0,F_1,F_2)"}, "."},
  EXAMPLE lines ///
	F_0 = random newForm(1,0,1,"a");
	F_1 = random newForm(1,0,2,"a");
	F_2 = random newForm(1,0,1,"a");
	w = random newForm(2,2,1,"a")
	{F_0,F_1,F_2}*w
  ///
}

document {
  Key => {(symbol _, DiffAlgField, DiffAlgForm),(symbol _, DiffAlgForm, DiffAlgField)},
  Headline => "contraction of a differential form with respect to a vector field",
  Usage => "X _ w",
  Inputs => {
	"X" => DiffAlgField => "a vector field",
	"w" => DiffAlgForm => "a differential form"
  },
  Outputs => {
	DiffAlgForm => {"the contraction of ", TT {"w"}, " with respect to ", TT {"X"}}
  },
  PARA {"Given a vector field ", TT {"X"}, " and a differential form ", TT {"w"}, ", this function returns the contraction of ", TT {"w"}, " with respect to ", TT {"X"}, ". The function can be called as ", TT {"X _ w"}, "  or as ", TT {"w _ X"}, "."},
  BR{},
  PARA {"In this example we compute the contraction of a simple differential form and a vector field."},
  EXAMPLE lines ///
	w = newForm("dx_0 * dx_1")
	Y = newField("ax_0")
	Y _ w
  ///
}

document {
  Key => (symbol |, DiffAlgField, DiffAlgField),
  Headline => "Lie bracket",
  Usage => "X | Y",
  Inputs => {
	"X" => DiffAlgField => "a vector field",
	"Y" => DiffAlgField => "a vector field"
  },
  Outputs => {
	DiffAlgField => {"the Lie bracket of ", TT {"X"}, " and ", TT {"Y"}}
  },
  PARA {"This function computes the Lie bracket of two vector fields."},
  EXAMPLE lines ///
	X = random newField(2,1,"a")
	Y = random newField(2,1,"b")
	X | Y
  ///,
  SeeAlso => {isInvolutive}
}

document {
  Key => {(symbol *, DiffAlgElement, RingElement),
		(symbol *, RingElement, DiffAlgElement),
		(symbol *, DiffAlgElement, ZZ),
		(symbol *, ZZ, DiffAlgElement),
		(symbol *, DiffAlgElement, QQ),
		(symbol *, QQ, DiffAlgElement)},
  Headline => "scalar multiplication",
  Usage => "e * n",
  Inputs => {
	"e" => DiffAlgElement => "a differential form or a vector field",
	"n" => RingElement => "scalar"
  },
  Outputs => {
	DiffAlgElement => {"the product of ", TT {"e"}, " and ", TT {"n"}}
  }
}

document {
  Key => {(symbol /, DiffAlgElement, RingElement),
		(symbol /, DiffAlgElement, ZZ),
		(symbol /, DiffAlgElement, QQ)},
  Headline => "scalar division",
  Usage => "e / n",
  Inputs => {
	"e" => DiffAlgElement => "a differential form or a vector field",
	"n" => RingElement => "scalar"
  },
  Outputs => {
	DiffAlgElement => {"the quotient of ", TT {"e"}, " by ", TT {"n"}}
  }
}

document {
  Key => (symbol -, DiffAlgElement),
  Headline => "negation of a differential form or vector field",
  Usage => "- e",
  Inputs => {
	"e" => DiffAlgElement => "a differential form or a vector field"
  },
  Outputs => {
	DiffAlgElement => {"the negation of ", TT {"e"}}
  }
}

document {
  Key => {(symbol |, DiffAlgElement, String), (symbol |, String, DiffAlgElement)}, 
  Headline => "concatenate a string with a differential form or vector field",
  Usage => "e | text",
  Inputs => {
	"e" => DiffAlgElement => "a differential form or a vector field",
	"text" => String => "a string to be concatenated"
  },
  Outputs => {
	String => {"concatenation of ", TT{"text"}, " with the string representation of ", TT {"e"}}
  },
  EXAMPLE lines ///
	w=newForm(2,1,2,"a")
	newForm ("b*4-"|w|"+4*dx_3")
  ///
}


document {
  Key => (homogenize, DiffAlgElement),
  Headline => "homogenize a differential form or vector field",
  Usage => "homogenize e",
  Inputs => {
	"e" => DiffAlgElement => "a form or a vector field",
  },
  Outputs => {
	DiffAlgElement => {"the homogenization of ", TT {"e"}, " with respect to a new variable. The resulting form or vector field is homogeneous."}
  },
  SeeAlso => {projectivize, isHomogeneous},
  EXAMPLE lines ///
	w = newForm("2*x_0*dx_0+x_1^2*dx_1")
	homogenize w
  ///,
  EXAMPLE lines ///
	homogenize newField ("ax_0+x_1*ax_2+a*ax_1")  
  ///,
  Caveat => "The homogenization process of a form adds one variable to the given element."
}

document {
  Key => {projectivize,(projectivize, DiffAlgElement)},
  Headline => "projectivize a differential form or vector field",
  Usage => "projectivize e",
  Inputs => {
	"e" => DiffAlgElement => "a form or a vector field",
  },
  Outputs => {
	DiffAlgElement => {"the projectivization of ", TT {"e"}, " with respect to a new variable. The resulting form or vector field descends to projective space."}
  },
  PARA {"This returns the unique differential form that extends the given one from affine space to projective space."},
  SeeAlso => {homogenize, isHomogeneous},
  EXAMPLE lines ///
	w = newForm("2*x_0*dx_0+x_1^2*dx_1")
	r = radial 2
	projectivize w
	r_oo
  ///,
  EXAMPLE lines ///
	projectivize newField ("ax_0+x_1*ax_2+a*ax_1")  
  ///,
  Caveat => "The projectivization process of a form increases the polynomial degree by one if the original element did not descend to projective space."
}
document {
  Key => (diff, DiffAlgForm),
  Headline => "exterior differential",
  Usage => "diff w",
  Inputs => {
	"w" => DiffAlgForm => "a form",
  },
  Outputs => {
	DiffAlgForm => {"the exterior differential of ", TT {"w"}}
  },
  PARA {"This function computes the exterior differential of a given differential form."},
  EXAMPLE lines ///
	w = newForm(2,1,2,"a")
	diff w
  ///
}

document {
  Key => (degree, DiffAlgElement),
  Headline => "degree of a differential form or a vector field",
  Usage => "degree e",
  Inputs => {
	"e" => DiffAlgElement => "a differential form or vector field",
  },
  Outputs => {
	List => {"If ", TT {"e"}, " is a vector field it returns ", TT {"{n,d}"}, "; if ", TT {"e"}," is a differential form it returns ", TT {"{n,r,d}"}, "."},
  },
  PARA {"This function returns the degree of a homogeneous differential form or vector field."},
  UL {
	{TT "n", ", ", ofClass ZZ, ", is the number of variables minus one"},
	{TT "r", ", ", ofClass ZZ, ", is the degree of the differential form or empty if ", TT "e", " is a vector field"}, 
	{TT "d", ", ", ofClass ZZ, ", is the degree of the polynomial coefficients"}
  },

  BR{},
  PARA {"In the following example we compute the degree of a differential form and a vector field."},
  EXAMPLE lines ///
	w = newForm(2,1,3,"a")
	degree(w)
	X = newField(2,2,"b")
	degree X
  ///,
  Caveat => PARA {"If the ", TO DiffAlgElement, " is non-homogeneous the function returns the highest degrees ", TT "{n,r,d}", " of each homogeneous component in the given expression. For example, if the degree of ", TT "w", " is ", TT "{2,1,3}", ", then ", TT "degree(w + (diff w))", " returns, ", TT "{2,2,3}"}
}

document {
  Key => (ring,DiffAlgElement),
  Headline => "ring of the differential form or vector field",
  Usage => "ring e",
  Inputs => {
	"e" => DiffAlgElement => "a differential form or vector field",
  },
  Outputs => {
	Ring => {"the ring of ", TT {"e"}}
  },
  PARA {"This function returns the ring where the given differential form of vector field is defined."},
  EXAMPLE lines ///
	w = newForm(2,1,2,"a")
	ring w
  ///
}

undocumented {(net,DiffAlgElement)}

undocumented { (toString,DiffAlgElement)}


document {
  Key => (sub,DiffAlgElement,Ring),
  Headline => "gets the RingElement of a differential form or vector field in a ring",
  Usage => "sub(e,R)",
  Inputs => {
	"e" => DiffAlgElement => "a differential form or vector field",
	"R" => Ring => "the target ring"
  },
  Outputs => {
	RingElement => {"an element of ", TT {"R"}, " representing ", TT {"e"}}
  },
  PARA {"By its nature, the package ", TO DiffAlg, " is constantly changing the rings where its differential forms and vector fields are defined. This function is useful to get information of ", TO DiffAlg, " out to some common polynomial rings and work with the rest of Macaulay2 packages."},
  BR{},
  PARA {"In this example we get the singular locus of a logarithmic form and compute its Hilbert polynomial."},
  EXAMPLE lines ///
	w = random logarithmicForm(2,{1,1},"a",Projective => true)
	I = singularIdeal w
	S = QQ[gens ring I]
	hilbertPolynomial (sub(I,S))
  ///
}


----------
---TEST---
----------

TEST ///
w = newForm(3,1,2,"a")
e = diff diff w
assert(e#"f" == 0)
///

TEST ///
X = newField(2,2,"a")
Y = newField(2,1,"b")
Z = newField(2,2,"c")
e = (X|(Y|Z)) + (Z|(X|Y)) + (Y|(Z|X))
assert(e#"f" == 0)
///

TEST ///
w = newForm(3,1,2,"a")
R = radial 3
assert((R_(diff w) + diff(R _ w) - 3*w)#"f" == 0) 
///

TEST ///
w = newForm(3,1,2,"a")
assert( (w^w)#"f" == 0)
///

TEST ///
w = newForm(3,1,2,"a")
h = newForm(3,1,3,"b")
assert((w^h + h^w)#"f" == 0)
///

TEST ///
X = newField(3,0,"a")
Y = newField(3,0,"b")
assert(isInvolutive dist {X,Y})
///

TEST ///
w = logarithmicForm(3,{1,2,1}, "a")
assert( (w ^ (diff w))#"f" == 0) 
///

TEST ///
w = random newForm(3,2,2,"a")
h = newForm(3,1,1,"b")
L = genKer(w^(diff h) + h ^ (diff w), h)
assert( (w^(diff (L_0)) + (L_0) ^ (diff w))#"f" == 0)
///

TEST ///
w = random newForm(3,2,2,"a")
h = newForm(3,1,1,"b")
L = genKer(w^(diff h) + h ^ (diff w), h)
M = genIm(w^(diff h) + h ^ (diff w), h)
assert(#L + #M == 16)
///

TEST ///
w = newForm "(576*x_0^3+1656*x_0^2*x_1+1134*x_0*x_1^2+1944*x_0^2*x_2+3456*x_0*x_1*x_2+972*x_1^2*x_2+2610*x_0*x_2^2+2268*x_1*x_2^2+1296*x_2^3)*dx_0+(1080*x_0^3+3042*x_0^2*x_1+2016*x_0*x_1^2+3582*x_0^2*x_2+6228*x_0*x_1*x_2+1728*x_1^2*x_2+4752*x_0*x_2^2+4032*x_1*x_2^2+2304*x_2^3)*dx_1+(1080*x_0^3+3042*x_0^2*x_1+2016*x_0*x_1^2+3582*x_0^2*x_2+6228*x_0*x_1*x_2+1728*x_1^2*x_2+4752*x_0*x_2^2+4032*x_1*x_2^2+2304*x_2^3)*dx_2"
h = newForm(2,1,3,"b")
K = genKer(w^(diff h) + h ^ (diff w), h)
assert(#K == 12)
///

TEST ///
w = (radial 2)_(newForm (2,2,1,"a"))
F = newForm(3,0,1,"b")
G = newForm(3,0,1,"c")
H = newForm(3,0,1,"d")
h = {F,G,H}*w
assert((h^(diff h))#"f" == 0)
///

TEST ///
w = diff newForm(2,1,2,"a")
F = newForm(4,0,1,"b")
G = newForm(4,0,1,"c")
H = newForm(4,0,1,"d")
h = diff ({F,G,H} * w)
assert(h#"f" == 0)
///


TEST ///
f = random newForm(2,2,1,"a")
h = newForm(2,1,2,"b")
L = genIm(f*h,h)
I = sum apply(L,singularIdeal)
J = singularIdeal(f^(random h));
assert(isSubset(sub(J,ring I),I))
///

TEST ///
r = radial 2
h = newForm(2,1,2,"a")
w = random newForm(2,1,1,"a")
h1 = linearComb (genKer(r_h,h), "a")
L1 = genKer((diff w)^h1,h1)
h2 = linearComb (genKer((diff w)^h,h), "a")
L2 = genKer(r_h2,h2)
m1 = matrix{for i in L1 list transpose gens singularIdeal i}
m2 = matrix{for i in L2 list transpose gens singularIdeal i}
m2 = sub(m2,ring m1)
assert (image m1 == image m2)
///

TEST ///
X = newField("x_0^2*ax_0+ x_1^2*ax_1+ x_2^2*ax_2+ x_3^2*ax_3");
Y = newField("x_5*ax_0+ x_4*ax_1+ x_3*ax_2+ x_2*ax_3+ x_1*ax_4+ x_0*ax_5");
D_0 = {X,Y};
for b in 1..3 do (for a in D_(b-1) do (D_b=join(D_(b-1),{a|Y,a|X})));
assert ({rank dist D_0, rank dist D_1, rank dist D_2, rank dist D_3} == {2, 3, 5, 6})
///

end


Jou = newForm "dx_0*(2*x_0*x_3^2*x_6^2+2*x_0*x_3^2*x_7^2+2*x_0*x_3^2*x_8^2+2*x_0*x_4^2*x_6^2+2*x_0*x_4^2*x_7^2+2*x_0*x_4^2*x_8^2+2*x_0*x_5^2*x_6^2+2*x_0*x_5^2*x_7^2+2*x_0*x_5^2*x_8^2)+dx_1*(2*x_1*x_3^2*x_6^2+2*x_1*x_3^2*x_7^2+2*x_1*x_3^2*x_8^2+2*x_1*x_4^2*x_6^2+2*x_1*x_4^2*x_7^2+2*x_1*x_4^2*x_8^2+2*x_1*x_5^2*x_6^2+2*x_1*x_5^2*x_7^2+2*x_1*x_5^2*x_8^2)+dx_2*(2*x_2*x_3^2*x_6^2+2*x_2*x_3^2*x_7^2+2*x_2*x_3^2*x_8^2+2*x_2*x_4^2*x_6^2+2*x_2*x_4^2*x_7^2+2*x_2*x_4^2*x_8^2+2*x_2*x_5^2*x_6^2+2*x_2*x_5^2*x_7^2+2*x_2*x_5^2*x_8^2)+dx_3*(2*i*x_0^2*x_3*x_6^2+2*i*x_0^2*x_3*x_7^2+2*i*x_0^2*x_3*x_8^2+2*i*x_1^2*x_3*x_6^2+2*i*x_1^2*x_3*x_7^2+2*i*x_1^2*x_3*x_8^2+2*i*x_2^2*x_3*x_6^2+2*i*x_2^2*x_3*x_7^2+2*i*x_2^2*x_3*x_8^2)+dx_4*(2*i*x_0^2*x_4*x_6^2+2*i*x_0^2*x_4*x_7^2+2*i*x_0^2*x_4*x_8^2+2*i*x_1^2*x_4*x_6^2+2*i*x_1^2*x_4*x_7^2+2*i*x_1^2*x_4*x_8^2+2*i*x_2^2*x_4*x_6^2+2*i*x_2^2*x_4*x_7^2+2*i*x_2^2*x_4*x_8^2)+dx_5*(2*i*x_0^2*x_5*x_6^2+2*i*x_0^2*x_5*x_7^2+2*i*x_0^2*x_5*x_8^2+2*i*x_1^2*x_5*x_6^2+2*i*x_1^2*x_5*x_7^2+2*i*x_1^2*x_5*x_8^2+2*i*x_2^2*x_5*x_6^2+2*i*x_2^2*x_5*x_7^2+2*i*x_2^2*x_5*x_8^2)+dx_6*(-(1+i)*x_0^2*x_3^2*x_6-(1+i)*x_0^2*x_4^2*x_6-(1+i)*x_0^2*x_5^2*x_6-(1+i)*x_1^2*x_3^2*x_6-(1+i)*x_1^2*x_4^2*x_6-(1+i)*x_1^2*x_5^2*x_6-(1+i)*x_2^2*x_3^2*x_6-(1+i)*x_2^2*x_4^2*x_6-(1+i)*x_2^2*x_5^2*x_6)+dx_7*(-(1+i)*x_0^2*x_3^2*x_7-(1+i)*x_0^2*x_4^2*x_7-(1+i)*x_0^2*x_5^2*x_7-(1+i)*x_1^2*x_3^2*x_7-(1+i)*x_1^2*x_4^2*x_7-(1+i)*x_1^2*x_5^2*x_7-(1+i)*x_2^2*x_3^2*x_7-(1+i)*x_2^2*x_4^2*x_7-(1+i)*x_2^2*x_5^2*x_7)+dx_8*(-(1+i)*x_0^2*x_3^2*x_8-(1+i)*x_0^2*x_4^2*x_8-(1+i)*x_0^2*x_5^2*x_8-(1+i)*x_1^2*x_3^2*x_8-(1+i)*x_1^2*x_4^2*x_8-(1+i)*x_1^2*x_5^2*x_8-(1+i)*x_2^2*x_3^2*x_8-(1+i)*x_2^2*x_4^2*x_8-(1+i)*x_2^2*x_5^2*x_8)"

