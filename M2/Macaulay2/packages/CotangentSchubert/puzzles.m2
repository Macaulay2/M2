-- there are various types of states:
-- * d-step d=0...4
-- * Separated
-- actually states are (almost) never used anywhere => no need to send back to main file, ultimately
--
-- there are various types of puzzle pieces:
-- * Equivariant/not
-- * Ktheory/Ktheory'/Generic/not
-- * Separated/not
-- though not all combinations implemented yet
--
-- there are various type of fugacities ~ more or less same as puzzle pieces
--

Puzzle = new Type of HashTable;

puzzleDir := CotangentSchubert#"source directory"|"CotangentSchubert/puzzles/";
myload = x -> load (puzzleDir|x)
myget = memoize(x -> first(
	value get (puzzleDir|x), -- ewww
	if debugLevel>0 then << x << " loaded" << endl
	))

puzzleOpts := opts ++ {Generic => true, Steps => null, Ktheory' => false, Labels => true, Paths => false, Separation => null};
export {"Steps", "Ktheory'", "Length", "Labels", "Paths"};
-- lots of global variables, not thread-safe!
upTriangles=downTriangles=rhombi={};
apply({rhombusStyle,downTriStyle,upTriStyle},protect);

myload "basic.m2"
myload "sepdesc.m2";
myload "almostsepdesc.m2";
myload "K.m2";
myload "equiv.m2";
myload "generic.m2";
myload "generic-equiv.m2";

Kstyle = "fill"=>"LightPink";
equivstyle = "fill"=>"LightGray";

curPuzzleOpts := null;
tiles := o -> (
    newPuzzleOpts := apply(keys puzzleOpts, k -> o#k);
    if newPuzzleOpts === curPuzzleOpts then return;
    if debugLevel>0 then << "rebuilding tiles" << newline;
    d := o.Steps;
    if o#Separation =!= null then (
	-- for ordinary (resp. almost) sep desc, k is a half-integer (resp. integer)
        (upTriangles,downTriangles,rhombi) = if instance(o#Separation,QQ) then
	sepdesc(d,lift(o.Separation-1/2,ZZ),o.Ktheory,o.Ktheory',o.Generic,o.Equivariant)
	else
	almostsepdesc(d,o.Separation,o.Ktheory,o.Ktheory',o.Generic,o.Equivariant);
        )
    else if o#Generic then (
        upTriangles = downTriangles = allTriangles d;
        rhombi = if o#Equivariant then allRhombi d else {};
        ) else (
        upTriangles = downTriangles = basicTriangles d;
        if o.Ktheory then (
            (KUp,KDown) := KTriangles d;
            upTriangles = upTriangles | apply(KUp, x -> append(x,Kstyle));
            downTriangles = downTriangles | apply(KDown, x -> append(x,Kstyle));
            );
        if o#Ktheory' then (
            (KUp,KDown) = KTriangles d;
            upTriangles = upTriangles | apply(KDown, x -> append(x,Kstyle));
            downTriangles = downTriangles | apply(KUp, x -> append(x,Kstyle));
            );
        rhombi = if o#Equivariant then apply(equivRhombi d, x -> append(x,equivstyle)) else {};
        );
    curPuzzleOpts = newPuzzleOpts;
    )

new List from Puzzle := (T,p) -> apply(p.Length,i->apply(p.Length-i,j->apply(3,k->p#(i,j,k))));

-- ascii art for puzzles (inspired by Rui Xiong's sage code)
trisize:=10;
netTri := (b,a,c) -> (
    if a=="_" then a="/";
    if b=="_" then b="\\";
    if c=="_" then c="-";
    "    /\\    "
    || "   /  \\  "
    || concatenate (
	i1:=trisize//3-(1+width a)//2;
	i3:=trisize//3-(1+width b)//2;
	i2:=trisize-i1-i3-width a-width b;
	-- need to test if numbers negative
	(i1:" ",a,i2:" ",b,i3:" ")
	)
    || " /      \\ "
    || concatenate if c=="" then trisize:" " else (
	i1=trisize//2-(1+width c)//2;
	i2=trisize-i1-width c;
	(if i1>=1 then " ",(i1-1):"-",c,(i2-1):"-",if i2>=1 then " ")
	)
    )

net Puzzle := p -> (
    stack apply(p.Length,i->horizontalJoin prepend(
	    (trisize//2)*(p.Length-1-i):" ",
	    apply(i+1,r->netTri(p#(i-r,r,0),p#(i-r,r,1),p#(i-r,r,2)))
            )))

puzzleSize := (options CotangentSchubert).Configuration#"PuzzleSize"

vgFontSize = n -> 1.7/(4.+n)
vgTextOpts := s -> { "dominant-baseline" => "middle", "text-anchor" => "middle", FontSize => vgFontSize (width s), "stroke" => "none", "fill" => "black", "font-family" => "helvetica" };
vgOpts := k -> { Size => k*puzzleSize, TransformMatrix => matrix{{-.5,.5,0,0},{-.5*sqrt 3,-.5*sqrt 3,0,0},{0,0,1,0},{0,0,0,1}}, "stroke-width" => 0.02, "fill" => "white" }

cols:={"red","green","blue","yellow","magenta","cyan","orange","black"};
strk:=0.01*puzzleSize;

vg = p -> gList toSequence (
    n:=p.Length;
    flatten apply(n, i -> flatten apply(n-i, j -> (
                a := try p#(i,j,0) else break;
                b := try p#(i,j,1) else break;
		c := try p#(i,j,2) else break;
                deepSplice {
                    local kk;
                    adj := (dir,a,x,y) -> (
                        r := regex(kk,a);
                        if r === null then return a; -- shouldn't happen
                        r=#a-1-r#0#0*2; cf:=0.08/#a;
                        if dir == 0 then [x,y+cf*r]
                        else if dir === 1 then [x+cf*r,y]
                        else [x+cf*r,y-cf*r]
                        );
                    if c != "" then (
			opts := {{[i+1,j],[i,j],[i,j+1]}}; if p#?(i,j,upTriStyle) then opts=append(opts,p#(i,j,upTriStyle));
                        Polygon opts,
                        if (i+j<n-1) then (
			    opts = {{[i+1,j],[i,j+1],[i+1,j+1]}};
			    if p#?(i,j,downTriStyle) then opts=append(opts,p#(i,j,downTriStyle));
			    Polygon opts
			    ),
                        if p#Paths then (
                            if i+j<n-1 then (
                                aa := p#(i+1,j,0);
                                bb := p#(i,j+1,1);
                                );
                            apply(0..p#Steps, k -> (
                                    kk=toString k;
				    (
                                        if match(kk,a) and match(kk,b) and match(kk,c) then (
                                            Line{adj(0,a,i,j+.5),[i+.333,j+.333],"stroke"=>cols#k,"stroke-width"=>strk},
                                            Line{adj(1,b,i+.5,j),[i+.333,j+.333],"stroke"=>cols#k,"stroke-width"=>strk},
                                            Line{adj(2,c,i+.5,j+.5),[i+.333,j+.333],"stroke"=>cols#k,"stroke-width"=>strk}
                                            )
                                        else if match(kk,a) and match(kk,b) then Line{adj(0,a,i,j+.5),adj(1,b,i+.5,j),"stroke"=>cols#k,"stroke-width"=>strk}
                                        else if match(kk,a) and match(kk,c) then Line{adj(0,a,i,j+.5),adj(2,c,i+.5,j+.5),"stroke"=>cols#k,"stroke-width"=>strk}
                                        else if match(kk,c) and match(kk,b) then Line{adj(2,c,i+.5,j+.5),adj(1,b,i+.5,j),"stroke"=>cols#k,"stroke-width"=>strk},
                                        if i+j<n-1 then
                                        if match(kk,aa) and match(kk,bb) and match(kk,c) then (
                                            Line{adj(0,aa,i+1,j+.5),[i+.666,j+.666],"stroke"=>cols#k,"stroke-width"=>strk},
                                            Line{adj(1,bb,i+.5,j+1),[i+.666,j+.666],"stroke"=>cols#k,"stroke-width"=>strk},
                                            Line{adj(2,c,i+.5,j+.5),[i+.666,j+.666],"stroke"=>cols#k,"stroke-width"=>strk}
                                            )
                                        else if match(kk,aa) and match(kk,bb) then Line{adj(0,aa,i+1,j+.5),adj(1,bb,i+.5,j+1),"stroke"=>cols#k,"stroke-width"=>strk}
                                        else if match(kk,aa) and match(kk,c) then Line{adj(0,aa,i+1,j+.5),adj(2,c,i+.5,j+.5),"stroke"=>cols#k,"stroke-width"=>strk}
                                        else if match(kk,c) and match(kk,bb) then Line{adj(2,c,i+.5,j+.5),adj(1,bb,i+.5,j+1),"stroke"=>cols#k,"stroke-width"=>strk}
                                        )
                                    )
                                )
                            ),
                        if p#Labels then (
                            if a!="_" then GraphicsText ({[i,j+.5],a} | vgTextOpts a),
                            if b!="_" then GraphicsText ({[i+.5,j],b} | vgTextOpts b),
                            if c!="_" then GraphicsText ({[i+.5,j+.5],c} | vgTextOpts c)
                            )
                        ) else (
			opts = {{[i+1,j],[i,j],[i,j+1],[i+1,j+1]}};
			if p#?(i,j,rhombusStyle) then opts=append(opts,p#(i,j,rhombusStyle));
                        Polygon opts,
                        if p#Paths then (
                            aa = p#(i+1,j,0);
                            bb = p#(i,j+1,1);
                            apply(0..p#Steps, k -> (
                                    kk=toString k;
                                    (
                                        if match(kk,a) and match(kk,aa) then Line{adj(0,a,i,j+.5),adj(0,aa,i+1,j+.5),"stroke"=>cols#k,"stroke-width"=>strk}
                                        else if match(kk,a) and match(kk,bb) then Line {adj(0,a,i,j+.5),adj(1,bb,i+.5,j+1),"stroke"=>cols#k,"stroke-width"=>strk},
                                        if match(kk,b) and match(kk,bb) then Line{adj(1,b,i+.5,j),adj(1,bb,i+.5,j+1),"stroke"=>cols#k,"stroke-width"=>strk}
                                        else if match(kk,b) and match(kk,aa) then Line {adj(1,b,i+.5,j),adj(0,aa,i+1,j+.5),"stroke"=>cols#k,"stroke-width"=>strk}
                                        )
                                    ))),
                        if p#Labels then (
                            if a!="_" then GraphicsText ({[i,j+.5],a} | vgTextOpts a),
                            if b!="_" then GraphicsText ({[i+.5,j],b} | vgTextOpts b)
                            )
                        )
                    }
                ))) | vgOpts n )

html Puzzle := p -> html vg p
tex Puzzle := texMath Puzzle := p -> tex vg p

digit := s -> #s==1 -- should be enough
valid := (x,y) -> x === y or (x === "#" and digit y) or x#0 === "*";

Puzzle ++ HashTable := (opts1, opts2) -> merge(opts1,opts2,last)
Puzzle ++ List := (opts1, opts2) -> opts1 ++ new class opts1 from opts2 -- cf similar method for OptionTable

Puzzle == Puzzle := (p,q) -> (new List from p) == (new List from q)

digitvals := l -> apply(select(flatten apply(l,ascii),i->i>=48 and i<58),i->i-48)

initPuzzle = true >> o -> args -> (
    if debugLevel>0 then << "initializing puzzle" << newline;
    args = apply(args, a -> new LabelList from a);
    if length unique apply(args,length) != 1 then error "inputs should have the same length";
    n := #(args#0);
    new Puzzle from pairs o | { Length=>n,
	if o.Separation === null and any(join args, s -> s==="_") then (
	    -- determine whether sep desc or almost sep desc
	    m := apply(2,i->(d:=digitvals(args#i); min d,max d));
	    if m#0#0 > m#1#1 then
	    Separation => 1/2 + m#1#1 -- sep desc
	    else if m#0#1 <= m#1#0 then
	    Separation => m#0#1 -- almost sep desc
	    else error "invalid arguments"
	    ),
        if o.Steps === null then Steps => max digitvals(join args)
        } | flatten flatten apply(n, i ->
        apply(n-i, j -> {
                (i,j,0) => if i==0 then args#1#j else "*",
                (i,j,1) => if j==0 then args#0#(n-1-i) else "*",
                (i,j,2) => if i+j==n-1 then if #args == 3 then args#2#j else "*" else "**" -- ** means even nothing (rhombus diagonal)
                }
            )
        )
    )

puzzle = puzzleOpts >> o -> args -> (
    if not instance(args,Puzzle) and (not instance(args,Sequence) or #args<2 or #args>3) then error "wrong number of arguments";
    puz0 := if instance(args,Sequence) then initPuzzle(args,o) else args ++ pairs o;
    n := puz0.Length;
    d := puz0.Steps;
    if d<0 then error "Please specify Steps or at least one digit";
    tiles puz0;
    lst := new MutableList;
    recurse := (i,j,o,p) -> ( -- o=0/1: up/down triangle needs filling
        if i == n then (
            lst#(#lst)=p;
	    if debugLevel>0 then << "one puzzle completed" << endl;
            ) else if o==0 then (
            -- up triangles
            scan(upTriangles, x -> if valid(p#(i,j,0),x#0) and valid(p#(i,j,1),x#1) and valid(p#(i,j,2),x#2) then (
                    recurse append(if i+j==n-1 then (i+1,0,0) else (i,j,1),p++{(i,j,0)=>x#0,(i,j,1)=>x#1,(i,j,2)=>x#2,if #x>3 then (i,j,upTriStyle)=>x#3});
                    ));
            -- rhombi
            if p#(i,j,2) === "**" then (
                scan(rhombi, x -> if valid(p#(i,j,0),x#0) and valid(p#(i,j,1),x#1)
                    and valid(p#(i+1,j,0),x#2) and valid(p#(i,j+1,1),x#3) then (
                        recurse(i,j+1,0,p++{(i,j,0)=>x#0,(i,j,1)=>x#1,(i,j,2)=>"",(i+1,j,0)=>x#2,(i,j+1,1)=>x#3,if #x>4 then (i,j,rhombusStyle)=>x#4});
                        )));
            ) else (
            -- down triangles
            scan(downTriangles, x -> if valid(p#(i+1,j,0),x#0) and valid(p#(i,j+1,1),x#1) and valid(p#(i,j,2),x#2) then (
                    recurse(i,j+1,0,p++{(i+1,j,0)=>x#0,(i,j+1,1)=>x#1,(i,j,2)=>x#2,if #x>3 then (i,j,downTriStyle)=>x#3});
                    )));
        );
    
    if debugLevel>0 then << "computing puzzles" << newline;
    recurse(0,0,0, puz0);
    new List from lst
    )

bottom = p -> new LabelList from apply(p.Length,i->p#(p.Length-1-i,i,2))

-- these last 2 are currently not exported

nwside = p -> new LabelList from apply(p.Length,i->p#(p.Length-1-i,0,1))

neside = p -> new LabelList from apply(p.Length,i->p#(0,i,0))

-- computation of (d<=3) equivariant fugacities
myload "fugacity.m2"

export { "puzzle", "bottom", "fugacity", "fugacityTally", "fugacityVector", "Puzzle" }
end

-- ex of use

puzzle({0,2,0,1},{0,1,0,2},Equivariant=>true)

puzzle("103213","103213","323011",Ktheory=>true)

puzzle("0123","3210",Generic=>true,Equivariant=>true)

#puzzle("#######","0101212","#######") -- # is any single-digit, * is anything

puzzle("5_4_3_","210___",Generic=>false,Equivariant=>false)

-- a complicated example: app C1 d=3

p=puzzle("2103","0321","2301",Generic=>true,Equivariant=>true);
f=fugacity\p;
sum f_{0..4}
sum f_{10..14}
sum f_{15..19} -- lots of cancellations
sum f
