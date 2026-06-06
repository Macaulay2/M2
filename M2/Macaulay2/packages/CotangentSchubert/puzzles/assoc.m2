puzzleDir := CotangentSchubert#"source directory"|"CotangentSchubert/puzzles/";
puzzleSize := (options CotangentSchubert).Configuration#"PuzzleSize"
debug VectorGraphics
-- move to appropriate files
svgUse = new MarkUpType of HypertextParagraph
svgUse.qname="use"
addAttribute(svgUse,svgAttr|{"xlink:href","x","y","data-col","data-typ"})
-- a different graphical rep of puzzles is used.
-- lines become 45 deg squares (rec triangles on boundary)
-- they get an additional decoration which describes which type of line they are:
-- 4 quarters of circle -> a little circle one needs to break
cols:={"red","green","blue","yellow"};
-- raw SVG
pthstyle:="fill:cyan;";
rad:=.3;
pth:=(svgElement Path) {"d"=>"M .5 .5 L "|toString(.5-rad)|" .5 C "|toString(.5-rad)|" "|toString(.5-.6*rad)|" "|toString(.5-.6*rad)|" "|toString(.5-rad)|" .5 "|toString(.5-rad)|" Z"};
ply:=(svgElement Polygon) {"points"=>"-.5,-.5 -.5,.5 .5,.5 .5,-.5"};
onelab:=(svgElement GraphicsText) {"1","x"=>"-0.3","y"=>"0.1","style"=>"stroke:none"};
zerolab:=(svgElement GraphicsText) {"0","x"=>"0","y"=>"0.1","style"=>"stroke:none"};
-- boundary variants
pthb:=(svgElement Path) {"d"=>"M .5 .5 L "|toString(.5-rad)|" .5 L "|toString(.5-rad/sqrt 2)|" "|toString(.5-rad/sqrt 2)|" Z","style"=>pthstyle}; -- SE
plyb:=(svgElement Polygon) {"points"=>"-.5,-.5 -.5,.5 .5,.5"};
sty:={"","transform:scalex(-1)","transform:scale(-1)","transform:scaley(-1)"};
l:={{-0.3,0.3},{0,0.3},{0,0},{-0.3,0}};
onelabb:=apply(l,p->
    (svgElement GraphicsText) {"1","x"=>toString p#0,"y"=>toString p#1,"style"=>"stroke:none"});
zerolabb:=apply(l,p->
    (svgElement GraphicsText) {"0","x"=>toString p#0,"y"=>toString p#1,"style"=>"stroke:none"});
--
chN := hashTable { "0" => 1, "1" => 2, "10" => 3 } -- hcol N side
chS := hashTable { "0" => 3, "1" => 0, "10" => 1 } -- hcol S side
cvN := hashTable { "0" => 2, "1" => 3, "10" => 1 } -- hcol S side
cvS := hashTable { "0" => 0, "1" => 1, "10" => 3 } -- hcol S side
--
DoublePuzzle = new SelfInitializingType of BasicList
html DoublePuzzle := html @@ hypertext
hypertext DoublePuzzle := A -> (
    (P1,P2) := toSequence A;
    n:=P1#Length;
    if P2#Length != n or P1#Steps != 1 or P2#Steps !=1 then error "wrong puzzles";
    hcol := table(n+1,n,(i,j)->if i+j<n then chN#(P1#(i,j,0)) else chS#(P2#(n-i,n-1-j,0)));
    vcol := table(n,n+1,(i,j)->if i+j<n then cvN#(P1#(i,j,1)) else cvS#(P2#(n-1-i,n-j,1)));
    htyp:=table(n+1,n,(i,j)->if i+j<n then 0 else 2);
    vtyp:=table(n,n+1,(i,j)->if i+j<n then 1 else 3);
    -- colors of labels are such that 0 invisible on 1, 1 on 0
    mydef:=apply(4,i->graphicsId());
    mydefb:=apply(4,i->graphicsId());
    defs:=svgDefs (
    	apply(4,i->(svgElement GraphicsList){
        	ply,
        	append(pth,"style"=>pthstyle|sty#i),
        	append(zerolab,"style"=>"fill:"|cols#((i+2)%4)),
        	append(onelab,"style"=>"fill:"|cols#((i+1)%4)),
        	"id"=>mydef#i
        	})
    	|
    	apply(4,i->(svgElement GraphicsList){
        	append(plyb,"style"=>sty#i),
        	append(pthb,"style"=>pthstyle|sty#i),
        	append(zerolabb#i,"style"=>"fill:"|cols#((i+2)%4)),
        	append(onelabb#i,"style"=>"fill:"|cols#((i+1)%4)),
        	"id"=>mydefb#i
        	})
	);
    lst1:=(svgElement GraphicsList) apply(n+1,i->(svgElement GraphicsList) apply(n,j->
            svgUse{
            	"xlink:href" => "#"|(if i==0 or i==n then mydefb else mydef)_(htyp#i#j),
            	"x"=>toString(j-i),"y"=>toString(i+j),
            	"class"=>"assoc",
            	"data-col"=>toString hcol#i#j,
            	"data-typ"=>toString htyp#i#j
            	}
            ));
    lst2:=(svgElement GraphicsList) apply(n+1,j->(svgElement GraphicsList) apply(n,i->
            svgUse{
            	"xlink:href" => "#"|(if j==0 or j==n then mydefb else mydef)_(vtyp#i#j),
            	"x"=>toString(j-i-1),"y"=>toString(i+j),
            	"class"=>"assoc",
            	"data-col"=>toString vcol#i#j,
            	"data-typ"=>toString vtyp#i#j
            	}
            )); -- note the i/j inversion to facilitate motion
    -- x from -n to n-1, y from 0 to 2n-1
    Sid:=graphicsId();
    sz:=toString(puzzleSize*n)|"em";
    -- add style and a switch
    SPAN{
	STYLE get (puzzleDir|"assoc.css"),
    	SVG {"xmlns"=>"http://www.w3.org/2000/svg",
    	    "id" => Sid,
	    "class" => "M2Svg",
    	    "style"=>"width:"|sz|";height:"|sz|";stroke:black;stroke-width:0.01",
    	    "viewBox"=>toString(-n-1)|" -1 "|toString(2*n+1)|" "|toString(2*n+1),
    	    lst1,lst2,defs,
    	    "onclick"=>get (puzzleDir|"assoc.js")}, -- , "class" => "labels"
    	INPUT{"type"=>"checkbox","id"=>"labels","onclick"=>Sid|".classList.toggle('labels')"},
    	LABEL{"for"=>"labels","Labels"}
    	}
    )

flp := s -> replace("X","\\\\",replace("\\\\","/",replace("/","X",s)))
mir := p -> ( n:=p.Length; applyKeys(p, a -> if class a =!= Sequence then a else (a#1,a#0,(4-a#2)%3)) )

net DoublePuzzle := A -> (
    (P1,P2) := toSequence A;
    stack(unstack net P1 | apply(drop(reverse unstack net mir P2,1),flp))
    )

puzzleOpts := opts ++ {Generic => true, Steps => null, Ktheory' => false, Separation => null};
doublePuzzle = puzzleOpts >> o -> (a,b,c,d) -> (
    P := puzzle(a,b,o); -- Equivariant should be false!
    flatten apply(P,p->apply(puzzle(c,d,reverse bottom p,o),q->DoublePuzzle(p,q)))
    )

-- if topLevelMode === WebApp then print STYLE get (puzzleDir|"assoc.css")
