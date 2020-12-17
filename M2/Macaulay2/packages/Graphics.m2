-- -*- coding: utf-8 -*-
-- licensed under GPL, any version
newPackage(
	"Graphics",
	Version => "0.2",
	Date => "February 26, 2010",
	Authors => {
		{Name => "Baptiste Calmès",
		HomePage => "http://www.math.uni-bielefeld.de/~bcalmes/"},
                {Name => "Viktor Petrov"}
		},
	Headline => "create graphics",
	Keywords => {"Graphics"},
	DebuggingMode => false)

-- Put here the name of functions that should be visible to users
export{
"Point2D", "Point3D", "Segment2D", "Segment3D", "Polygon2D", "Polygon3D", "Circle", "Sphere", "TextTag", "GraphicPrimitive", 
"FormattedGraphicPrimitives", "Picture",
"point", "segment", "polygon", "circle", "sphere", "textTag", "formatGraphicPrimitives", "picture",
"pictureZone", 
"mergeFormattedGraphicPrimitives",
"svgObject", "svgPicture", "defaultSVGOpening",
"pgfObject", "pgfPicture",
"viewPicture"
}

-- Variables that can be modified by the user
exportMutable{
"possibleSVGOptions", "defaultSVGOptions", "defaultSVGValues", "defaultSVGHeading", "defaultSVGClosing",
"possiblePGFOptions", "defaultPGFOptions", "defaultPGFValues"
}

-- Package code 

--Defining the class of a GraphicPrimitive. It will have child types like Points, etc. 
GraphicPrimitive = new Type of BasicList 

--Defining the class of a FormattedGraphicPrimitives. It is a basic list that contains a list of GraphicPrimitives and a hash table of formatting options (like color, thickness, width, etc.)
FormattedGraphicPrimitives = new Type of BasicList 

--Defining the class of a FormattedGraphicPrimitive. It is a basic list that contains a GraphicPrimitive and a hash table of formatting options (like color, thickness, width, etc.)
--FormattedGraphicPrimitive = new Type of BasicList 

--Defining the class of a Picture. It is a basic list containing FormattedGraphicPrimitives. Some functions should be written to optimize it as far as options are concerned. The order of the FGPs should not be changed, for reasons of what should appear above what on screen.
Picture = new Type of BasicList

--Defining the class of a point (each point is at least two coordinates)
Point2D = new Type of GraphicPrimitive 

--Defining the class of a point (each point is at least two coordinates)
Point3D = new Type of GraphicPrimitive 

--Defining the class of a line segment (two points)
Segment2D = new Type of GraphicPrimitive

--Defining the class of a line segment (two points)
Segment3D = new Type of GraphicPrimitive

--Defining the class of a polygon (set of points)
Polygon2D = new Type of GraphicPrimitive

--Defining the class of a polygon (set of points)
Polygon3D = new Type of GraphicPrimitive

--Defining the class of a sphere (a center point and a radius)
Circle = new Type of GraphicPrimitive 

--Defining the class of a sphere (a center point and a radius)
Sphere = new Type of GraphicPrimitive 

--Defining the class of a text tag (a string and a position where to put it) 
TextTag = new Type of GraphicPrimitive 

--Making a Point (2D or 3D)
point = method()

point(ZZ,ZZ) := (x,y) -> new Point2D from {x,y}

point(RR,RR) := (x,y) -> new Point2D from {round(x),round(y)}

point(ZZ,ZZ,ZZ) := (x,y,z) -> new Point3D from {x,y,z}

point(RR,RR,RR) := (x,y,z) -> new Point3D from {round(x),round(y),round(z)}

--Making a Segment from points 
segment = method()
segment(Point2D,Point2D) := (p,q) -> new Segment2D from {p,q}
segment(Point3D,Point3D) := (p,q) -> new Segment3D from {p,q}

--Making a polygon from a list points
polygon = method()
polygon(List) := (L) -> 
	(
	cl:=keys set(class\L);
	if cl === {Point2D} then return new Polygon2D from L;
	if cl === {Point3D} then return new Polygon3D from L;
	error "The list should be non empty, all elements in it should have the same type, either Point2D or Point3D."
	)

--Making a circle from a center point and a radius 
circle = method()
circle(Point2D,ZZ) := (p,r) -> new Circle from {p,r} 

circle(Point2D,RR) := (p,r) -> new Circle from {p,round(r)} 

--Making a sphere from a center point and a radius 
sphere = method()
sphere(Point3D,ZZ) := (p,r) -> new Sphere from {p,r} 

sphere(Point3D,RR) := (p,r) -> new Sphere from {p,round(r)} 

--Making a text tag from a point and a string 
textTag = method()
textTag(Point2D,String) := (p,s) -> new TextTag from {p,s} 

textTag(Point3D,String) := (p,s) -> new TextTag from {p,s} 

--Turning a GraphicPrimitive object into a formated one by specifying options. These options will be passed to the different methods to create pictures in different formats (ex: SVG, etc.) so they are not constrained here because I don't know what graphic formats will be supported in the future. 
formatGraphicPrimitives = method()
formatGraphicPrimitives(BasicList,HashTable) := (gplist, h) ->
	(
	if all(gplist,x->instance(x,GraphicPrimitive)) then new FormattedGraphicPrimitives from {gplist,h} else error "All elements of the list should be of the type GraphicPrimitive."
	)

--make a picture out of a list of FormattedGraphicPrimitives. No optimization is done on the options.
picture = method()
picture(BasicList) := (fgplist) ->
	(
	if all(fgplist,x->instance(x,FormattedGraphicPrimitives)) then new Picture from fgplist else error "All elements of the list should be of the type FormattedGraphicPrimitives."
	)

--merging two FormattedGraphicPrimitives
mergeFormattedGraphicPrimitives = method(Options => true)
mergeFormattedGraphicPrimitives(FormattedGraphicPrimitives,FormattedGraphicPrimitives) := {"keep"=>"1","priority"=>"1"} >> opts -> (fgp1,fgp2) ->
	(
	new FormattedGraphicPrimitives from {(fgp1#0)|(fgp2#0),mergeOptions(fgp1#1,fgp2#1,"keep"=>opts#"keep","priority"=>opts#"priority")}
	)

mergeOptions = {"keep"=>"1or2","priority"=>"1"} >> opts -> (h1,h2) ->
	(
	if opts#"keep" === "1" or opts#"keep" === "1and2" then h2 = applyPairs(h2,(k,v)->if (h1)#?k then (k,v)); 
	if opts#"keep" === "2" or opts#"keep" === "1and2" then h1 = applyPairs(h1,(k,v)->if (h2)#?k then (k,v)); 
	if opts#"priority" === "1" then return merge(h1,h2,(x,y)->x); 
	if opts#"priority" === "2" then return merge(h1,h2,(x,y)->y); 
	)

--determine the zone in which a GraphicPrimitive object lives. It is given as a list {xmin,xmax,ymin,ymax} for 2D objects and {xmin,xmax,ymin,ymax} for 3D objects.
pictureZone = method()

pictureZone(Point2D) := (GP) -> {GP#0,GP#0,GP#1,GP#1}

pictureZone(Point3D) := (GP) -> {GP#0,GP#0,GP#1,GP#1,GP#2,GP#2}

pictureZone(Segment2D) := (GP) -> {min(GP#0#0,GP#1#0),max(GP#0#0,GP#1#0),min(GP#0#1,GP#1#1),max(GP#0#1,GP#1#1)}

pictureZone(Segment3D) := (GP) -> {min(GP#0#0,GP#1#0),max(GP#0#0,GP#1#0),min(GP#0#1,GP#1#1),max(GP#0#1,GP#1#1),min(GP#0#2,GP#1#2),max(GP#0#2,GP#1#2)}

pictureZone(Circle) := (GP) -> {GP#0#0-GP#1,GP#0#0+GP#1,GP#0#1-GP#1,GP#0#1+GP#1}

pictureZone(Sphere) := (GP) -> {GP#0#0-GP#1,GP#0#0+GP#1,GP#0#1-GP#1,GP#0#1+GP#1,GP#0#2-GP#1,GP#0#2+GP#1}

pictureZone(Polygon2D) := (GP) -> 
	(
	Lx:=toList apply(GP,x->x#0);
	Ly:=toList apply(GP,x->x#1);
	{min Lx,max Lx, min Ly, max Ly}
	)

pictureZone(Polygon3D) := (GP) -> 
	(
	Lx:=toList apply(GP,x->x#0);
	Ly:=toList apply(GP,x->x#1);
	Lz:=toList apply(GP,x->x#2);
	{min Lx,max Lx, min Ly, max Ly,min Lz, max Lz}
	)

pictureZone(TextTag) := (GP) -> pictureZone(GP#0)

--determine the zone in which a list of GraphicPrimitives lie. It is supposed a nonempty list and to be consistent in containing only 2D objects or only 3D objects.
pictureZone(FormattedGraphicPrimitives) := (fgps) -> 
	(
	mylist:=apply(fgps#0,pictureZone);
	if #(mylist#0)==4 then
	{min apply(mylist,x->x#0), max apply(mylist,x->x#1), min apply(mylist,x->x#2), max apply(mylist,x->x#3)} 
	else if #(mylist#0)==6 then
	{min apply(mylist,x->x#0), max apply(mylist,x->x#1), min apply(mylist,x->x#2), max apply(mylist,x->x#3), min apply(mylist,x->x#4), max apply(mylist,x->x#5)} 
	)

--determine the zone in which a picture lies
pictureZone(Picture) := (pict) ->
	(
	mylist:=apply(toList(pict),pictureZone);
	if #(mylist#0)==4 then
	{min apply(mylist,x->x#0), max apply(mylist,x->x#1), min apply(mylist,x->x#2), max apply(mylist,x->x#3)} 
	else if #(mylist#0)==6 then
	{min apply(mylist,x->x#0), max apply(mylist,x->x#1), min apply(mylist,x->x#2), max apply(mylist,x->x#3), min apply(mylist,x->x#4), max apply(mylist,x->x#5)}
	)

-- SVG section
--
--Functions are defined to translate graphic primitives into SVG language.
--

--Default SVG values for coordinates, etc.
defaultSVGValues= hashTable{
	Point2D => hashTable{
		"cx"=>"0",
		"cy"=>"0",
		"r"=>"2",
		},
	Circle => hashTable{
		"cx"=>"0",
		"cy"=>"0",
		"r"=>"10",
		},
	Segment2D => hashTable{
		"x1"=>"0",
		"y1"=>"0",
		"x2"=>"0",
		"y2"=>"0",
		},
	Polygon2D => hashTable{
		"points"=>"0,0",
		},
	TextTag => hashTable{
		"x"=>"0",
		"y"=>"0",
		}
	}

--possible SVG options
possibleSVGOptions = hashTable{
	Point2D => set{"stroke","stroke-width","stroke-opacity","fill","fill-opacity"},
	Circle => set{"stroke","stroke-width","stroke-opacity","fill","fill-opacity"},
	Segment2D => set{"stroke","stroke-width","stroke-opacity"},
	Polygon2D => set{"stroke","stroke-width","stroke-opacity","fill","fill-opacity"},
	TextTag => set{"stroke","stroke-width","stroke-opacity","fill","fill-opacity","font-family","font-size"}
	}


--Default SVG options for each graphic primitive
defaultSVGOptions = hashTable{
	Point2D => hashTable{
		"stroke"=>"black",
		"stroke-width"=>"0",
		"stroke-opacity"=>"1",
		"fill"=>"black",
		"fill-opacity"=>"1"
		},
	Circle => hashTable{
		"stroke"=>"black",
		"stroke-width"=>"1",
		"stroke-opacity"=>"1",
		"fill"=>"black",
		"fill-opacity"=>"0"
		},
	Segment2D => hashTable{
		"stroke"=>"black",
		"stroke-width"=>"1",
		"stroke-opacity"=>"1",
		},
	Polygon2D => hashTable{
		"stroke"=>"black",
		"stroke-width"=>"1",
		"stroke-opacity"=>"1",
		"fill"=>"black",
		"fill-opacity"=>"1"
		},
	TextTag => hashTable{
		"stroke"=>"black",
		"stroke-width"=>"0",
		"stroke-opacity"=>"0",
		"fill"=>"black",
		"fill-opacity"=>"1",
		"font-family"=>"Verdana",
		"font-size"=>"12"
		}
	}

--default SVG document heading
defaultSVGHeading =
(///<?xml version="1.0" standalone="no"?>
<!DOCTYPE svg PUBLIC "-//W3C//DTD SVG 1.1//EN"
"http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd">

///)

--default SVG opening
defaultSVGOpening = method()
defaultSVGOpening(ZZ,ZZ) := (wid,hei) ->
	(///<svg width="///|toString(wid)|///" height="///|toString(hei)|///" version="1.1"
xmlns="http://www.w3.org/2000/svg">

///)

--default SVG closing 
defaultSVGClosing =
(///</svg>
///)

--Making an SVG object (stored in a string) from a Graphic Primitive and some options
svgObject = method()
svgObject(Point2D,HashTable) := (mypt,opts) ->
	(
	opts= mergeOptions(mergeOptions(opts,possibleSVGOptions#Point2D,"keep"=>"1and2","priority"=>"1"),defaultSVGOptions#Point2D);
	"<circle "
	|///cx="///|toString(mypt#0)|///" cy="///|toString(mypt#1)|/// r="///|defaultSVGValues#Point2D#("r")|///" ///
	|concatenate(apply(pairs(opts),x->(x#0|///="///|x#1|///" ///)))
	|"/>"
	)

svgObject(Circle,HashTable) := (mycirc,opts) ->
	(
	opts= mergeOptions(mergeOptions(opts,possibleSVGOptions#Circle,"keep"=>"1and2","priority"=>"1"),defaultSVGOptions#Circle);
	"<circle "
	|///cx="///|toString(mycirc#0#0)|///" cy="///|toString(mycirc#0#1)|///" r="///|toString(mycirc#1)|///" ///
	|concatenate(apply(pairs(opts),x->(x#0|///="///|x#1|///" ///)))
	|"/>"
	)

svgObject(Segment2D,HashTable) := (myseg,opts) ->
	(
	opts= mergeOptions(mergeOptions(opts,possibleSVGOptions#Segment2D,"keep"=>"1and2","priority"=>"1"),defaultSVGOptions#Segment2D);
	"<line "
	|///x1="///|toString(myseg#0#0)|///" y1="///|toString(myseg#0#1)|///" x2="///|toString(myseg#1#0)|///" y2="///|toString(myseg#1#1)|///" ///
	|concatenate(apply(pairs(opts),x->(x#0|///="///|x#1|///" ///)))
	|"/>"
	)

svgObject(Polygon2D,HashTable) := (mypoly,opts) ->
	(
	opts= mergeOptions(mergeOptions(opts,possibleSVGOptions#Polygon2D,"keep"=>"1and2","priority"=>"1"),defaultSVGOptions#Polygon2D);
	"<polygon "
	|///points="///|concatenate(apply(mypoly,x->toString(x#0)|","|toString(x#1)|" "))|///" ///
	|concatenate(apply(pairs(opts),x->(x#0|///="///|x#1|///" ///)))
	|"/>"
	)

svgObject(TextTag,HashTable) := (mytext,opts) ->
	(
	opts= mergeOptions(mergeOptions(opts,possibleSVGOptions#TextTag,"keep"=>"1and2","priority"=>"1"),defaultSVGOptions#TextTag);
	///<text id="TextElement" ///
	|///x="///|toString(mytext#0#0)|///" y="///|toString(mytext#0#1)|///" ///
	|concatenate(apply(pairs(opts),x->(x#0|///="///|x#1|///" ///)))
	|">"
	|mytext#1
	|"</text>"
	)

svgObject(FormattedGraphicPrimitives) := (fgp) -> concatenate(apply(fgp#0,x->svgObject(x,fgp#1)|newline|newline))
	
svgObject(Picture) := (pict) -> concatenate(apply(toList pict,svgObject))

--Making an SVG picture (stored in a string) from a Picture with prescribed size. It only accepts Pictures with 2D primitives.
svgPicture = method()
svgPicture(Picture,ZZ,ZZ) := (pict,w,h) ->
	(
	defaultSVGHeading
	|defaultSVGOpening(w,h)
	|svgObject(pict) 
	|defaultSVGClosing
	) 

--Making an SVG picture (stored in a string) from a Picture with size computed to be just the size of the picture. 
svgPicture(Picture) := (pict) ->
	(
	z:=pictureZone(pict);
	w:=z#1+100;
	h:=z#3+100;
	svgPicture(pict,w,h)
	) 

--Making an SVG picture from a Picture with prescribed size, and storing it in a file 
svgPicture(Picture,ZZ,ZZ,String) := (pict,w,h,s) ->
	(
	(if match(///\.svg\'///,s) then s else s|".svg") << svgPicture(pict,w,h) << endl << close
	) 

--Making an SVG picture from a Picture with size computed to be just the size of the picture, and storing it in a file 
svgPicture(Picture,String) := (pict,s) ->
	(
	(if match(///\.svg\'///,s) then s else s|".svg") << svgPicture(pict) << endl << close
	) 

--view a picture document (that is in SVG for the moment) in the default browser.
viewPicture = method()
viewPicture(String) := (filename) ->
	(
	if match(///\.svg\'///,filename) then show new URL from {filename}
	)

--
--PGF section
--containing functions to turn Pictures into the pgf/tikz language for latex
--

--Default PGF options for each graphic primitive
possiblePGFOptions = hashTable{
	Picture => set {"scale","x","y"},
	Point2D => set {"fill","draw","style","line width","fill opacity","draw opacity","opacity"},
	Circle => set{"fill","draw","style","line width","fill opacity","draw opacity","opacity","dashed"},
	Segment2D => set{"draw","style","line width","draw opacity","opacity","dashed"},
	Polygon2D => set{"fill","draw","style","line width","fill opacity","draw opacity","opacity","dashed"},
	TextTag => set{}
	}

--Default PGF values for coordinates
defaultPGFValues = hashTable{
	Point2D => hashTable{
		"cx"=>"0", 
		"cy"=>"0",
		"r"=>"0.1",
		},
	Circle => hashTable{
		"cx"=>"0", 
		"cy"=>"0",
		"r"=>"1",
		},
	Segment2D => hashTable{},
	Polygon2D => hashTable{},
	TextTag => hashTable{}
}

--Default PGF options for each graphic primitive
defaultPGFOptions = hashTable{
	Picture => hashTable{
		},
	Point2D => hashTable{
		"fill"=>"black"
		},
	Circle => hashTable{
		},
	Segment2D => hashTable{
		},
	Polygon2D => hashTable{
		"fill"=>"black",
		"fill opacity"=>"1"
		},
	TextTag => hashTable{
		}
	}

--making pdf code out of a graphic primitive
pgfObject = method()

pgfObject(Circle,HashTable) := (mycirc,opts)->
	(
	opts= mergeOptions(mergeOptions(opts,possiblePGFOptions#Circle,"keep"=>"1and2","priority"=>"1"),defaultPGFOptions#Circle);
	///\draw[///
	|(if #opts==0 then "" else fold(apply(pairs(opts),x->(if x#1===null then x#0 else x#0|"="|x#1)),(a,b)->a|","|b))
	|///] (///
	|mycirc#0#0|","|mycirc#0#1
	|///) circle (///
	|mycirc#1
	|///);///
	)

pgfObject(Point2D,HashTable) := (mypt,opts)->
	(
	opts= mergeOptions(mergeOptions(opts,possiblePGFOptions#Point2D,"keep"=>"1and2","priority"=>"1"),defaultPGFOptions#Point2D);
	///\draw[///
	|(if #opts==0 then "" else fold(apply(pairs(opts),x->(if x#1===null then x#0 else x#0|"="|x#1)),(a,b)->a|","|b))
	|///] ///
	|///(///|mypt#0|","|mypt#1|///) circle (///|defaultPGFValues#Point2D#"r"|///);///
	)

pgfObject(Segment2D,HashTable) := (myseg,opts)->
	(
	opts= mergeOptions(mergeOptions(opts,possiblePGFOptions#Segment2D,"keep"=>"1and2","priority"=>"1"),defaultPGFOptions#Segment2D);
	///\draw[///
	|(if #opts==0 then "" else fold(apply(pairs(opts),x->(if x#1===null then x#0 else x#0|"="|x#1)),(a,b)->a|","|b))
	|///] ///
	|///(///|myseg#0#0|","|myseg#0#1|///) -- (///|myseg#1#0|","|myseg#1#1|///);///
	)

pgfObject(Polygon2D,HashTable) := (mypoly,opts)->
	(
	opts= mergeOptions(mergeOptions(opts,possiblePGFOptions#Polygon2D,"keep"=>"1and2","priority"=>"1"),defaultPGFOptions#Polygon2D);
	///\draw[///
	|fold(apply(pairs(opts),x->(if x#1===null then x#0 else x#0|"="|x#1)),(a,b)->a|","|b)
	|///] ///
	|(if #opts==0 then "" else fold(apply(toList(mypoly),x->///(///|toString(x#0)|","|toString(x#1)|///)///),(a,b)->a|/// -- ///|b))
	|/// -- cycle;///
	)

pgfObject(TextTag,HashTable) := (mytext,opts)->
	(
	opts= mergeOptions(mergeOptions(opts,possiblePGFOptions#TextTag,"keep"=>"1and2","priority"=>"1"),defaultPGFOptions#TextTag);
	///\draw ///
	|///(///|mytext#0#0|","|mytext#0#1|///) ///
	|///node[///
	|(if #opts==0 then "" else fold(apply(pairs(opts),x->(if x#1===null then x#0 else x#0|"="|x#1)),(a,b)->a|","|b))
	|///] ///
	|///{///|mytext#1|///};///
	)

--This is bad. One should use a scope for each FormattedGraphicPrimitive. I will fix this later.
pgfObject(FormattedGraphicPrimitives) := (fgp) -> concatenate(apply(fgp#0,x->pgfObject(x,fgp#1)|newline))

--Here one could maybe accept global options for scope.
pgfObject(Picture) := (pict) -> concatenate(apply(toList pict,pgfObject))

--making a complete pgf picture out a Picture object and some global options
pgfPicture = method()
pgfPicture(Picture,HashTable) := (pict,opts) ->
	(
	opts= mergeOptions(mergeOptions(opts,possiblePGFOptions#Picture,"keep"=>"1and2","priority"=>"1"),defaultPGFOptions#Picture);
	///\begin{tikzpicture}[///
	|(if #opts==0 then "" else fold(apply(pairs(opts),x->(if x#1===null then x#0 else x#0|"="|x#1)),(a,b)->a|","|b))
	|///]///|newline
	|pgfObject(pict)
	|///\end{tikzpicture}///|newline
	)

pgfPicture(Picture) := (pict) ->
	(
	///\begin{tikzpicture}///|newline
	|pgfObject(pict)
	|///\end{tikzpicture}///|newline
	)

pgfPicture(Picture,HashTable,String) := (pict,opts,filename) ->
	(
	(if match(///\.tex\'///,filename) then filename else filename|".tex") << pgfPicture(pict,opts) << endl << close
	)

pgfPicture(Picture,String) := (pict,filename) ->
	(
	(if match(///\.tex\'///,filename) then filename else filename|".tex") << pgfPicture(pict) << endl << close
	)


--The rest of the file is documentation.

beginDocumentation()

doc ///
	Key
		Graphics
	Headline
		Graphics
	Description
		Text
			This package provides graphic primitives and functions to turn them into pictures in different formats. The formats supported include svg and pgf/tikz.
///

doc ///
	Key
		GraphicPrimitive	
	Headline
		the class of all graphic primitives	
///

doc ///
	Key
		FormattedGraphicPrimitives	
	Headline
		the class of lists of graphic primitives together with formatting options
///

doc ///
	Key
		Picture
	Headline
		the class of pictures containing several FormattedGraphicPrimitives
///

doc ///
	Key
		Point2D
	Headline
		the class of a point in a 2D space
///

doc ///
	Key
		Point3D
	Headline
		the class of a point in a 3D space
///

doc ///
	Key
		Segment2D
	Headline
		the class of a line segment in a 2D space
///

doc ///
	Key
		Segment3D
	Headline
		the class of a line segment in a 3D space
///

doc ///
	Key
		Polygon2D
	Headline
		the class of a polygon in a 2D space
///

doc ///
	Key
		Polygon3D
	Headline
		the class of a polygon in a 3D space
///

doc ///
	Key
		Circle
	Headline
		the class of a circle in a 2D space
///

doc ///
	Key
		Sphere
	Headline
		the class of a sphere in a 3D space
///

doc ///
	Key
		TextTag
	Headline
		the class of a text tag 
///

doc ///
	Key
		point
	Headline
		create a point 
///

doc ///
	Key
		(point,ZZ,ZZ)
	Headline
		a point from two coordinates	
	Usage
		point(x,y)	
	Inputs
		x: ZZ 
		y: ZZ 
	Outputs
		: Point2D
			the point with coordinates {\tt x} and {\tt y}	
	Description
		Example
			point(20, 30)
///

doc ///
	Key
		(point,RR,RR)
	Headline
		a point from two coordinates	
	Usage
		point(x,y)	
	Inputs
		x: RR 
		y: RR 
	Outputs
		: Point2D
			the point with coordinates {\tt x} and {\tt y}	
	Description
		Example
			point(2.1, 3.)
///

doc ///
	Key
		(point,ZZ,ZZ,ZZ)
	Headline
		a point from three coordinates	
	Usage
		point(x,y,z)	
	Inputs
		x: ZZ 
		y: ZZ 
		z: ZZ 
	Outputs
		: Point3D 
			the point with coordinates {\tt x}, {\tt y} and {\tt z}
	Description
		Example
			point(20,30,40)
///

doc ///
	Key
		(point,RR,RR,RR)
	Headline
		a point from three coordinates	
	Usage
		point(x,y,z)	
	Inputs
		x: RR 
		y: RR 
		z: RR
	Outputs
		: Point3D 
			the point with coordinates {\tt x}, {\tt y} and {\tt z}
	Description
		Example
			point(2.1, 3.,4.)
///

doc ///
	Key
		segment
	Headline
		create a line segment 
///

doc ///
	Key
		(segment,Point2D,Point2D)
	Headline
		a line segment from two points 
	Usage
		segment(p,q)	
	Inputs
		p: Point2D 
		q: Point2D
	Outputs
		: Segment2D 
			the line segment with endpoints {\tt p} and {\tt q}	
	Description
		Example
			p=point(2.1, 3.)
			q=point(.4, 4.6)
			segment(p,q)
///

doc ///
	Key
		(segment,Point3D,Point3D)
	Headline
		a line segment from two points 
	Usage
		segment(p,q)	
	Inputs
		p: Point3D 
		q: Point3D
	Outputs
		: Segment3D 
			the line segment with endpoints {\tt p} and {\tt q}	
	Description
		Example
			p=point(2.1, 3.,4.)
			q=point(.4, 4.6,5.3)
			segment(p,q)
///

doc ///
	Key
		polygon
	Headline
		create a polygon from a list of points 
///

doc ///
	Key
		(polygon,List)
	Headline
		a polygon from a list of points 
	Usage
		polygon(L)	
	Inputs
		L: List 
			of @TO Point2D@s or of @TO Point3D@s
	Outputs
		: Thing
			the polygon defined by the points in {\tt L}	
	Description
		Text
			All the points must be of the same class, either @TO Point2D@ or @TO Point3D@. 
		Example
			L={point(2.1, 3.),point(.4, 4.6),point(3.1,5.)}
			polygon(L)
///

doc ///
	Key
		circle
	Headline
		create a circle from a point and a radius 
///

doc ///
	Key
		(circle,Point2D,ZZ)
	Headline
		a circle from a center and a radius
	Usage
		circle(p,r)	
	Inputs
		p: Point2D 
			the center of the circle 
		r: ZZ 
			the radius of the circle 
	Outputs
		: Circle 
			the circle with center {\tt p} and radius {\tt r} 
	Description
		Example
			circle(point(20,30),10)
///

doc ///
	Key
		(circle,Point2D,RR)
	Headline
		a circle from a center and a radius
	Usage
		circle(p,r)	
	Inputs
		p: Point2D 
			the center of the circle 
		r: RR
			the radius of the circle 
	Outputs
		: Circle 
			the circle with center {\tt p} and radius {\tt r} 
	Description
		Example
			circle(point(20.1, 30.),30.)
///

doc ///
	Key
		sphere
	Headline
		create a sphere from a point and a radius 
///

doc ///
	Key
		(sphere,Point3D,ZZ)
	Headline
		a sphere from a center and a radius
	Usage
		sphere(p,r)	
	Inputs
		p: Point3D 
			the center of the sphere
		r: ZZ 
			the radius of the sphere
	Outputs
		: Sphere 
			the sphere with center {\tt p} and radius {\tt r} 
	Description
		Example
			sphere(point(20,30,100),10)
///

doc ///
	Key
		(sphere,Point3D,RR)
	Headline
		a sphere from a center and a radius
	Usage
		sphere(p,r)	
	Inputs
		p: Point3D 
			the center of the sphere
		r: RR
			the radius of the sphere
	Outputs
		: Sphere 
			the sphere with center {\tt p} and radius {\tt r} 
	Description
		Example
			sphere(point(20.1, 30.,100.),30.)
///

doc ///
	Key
		textTag
	Headline
		create a text tag at a position 
///

doc ///
	Key
		(textTag,Point2D,String)
	Headline
		a text tag at a position 
	Usage
		textTag(p,s)	
	Inputs
		p: Point2D 
			the position of the tag 
		s: String 
			the text of the tag 
	Outputs
		: TextTag 
			the {\tt TextTag} with text {\tt s} at position {\tt p}
	Description
		Example
			textTag(point(2.1, 3.),"My Tag")
///

doc ///
	Key
		(textTag,Point3D,String)
	Headline
		a text tag at a position 
	Usage
		textTag(p,s)	
	Inputs
		p: Point3D 
			the position of the tag 
		s: String 
			the text of the tag 
	Outputs
		: TextTag 
			the {\tt TextTag} with text {\tt s} at position {\tt p}
	Description
		Example
			textTag(point(2.1, 3.,4.),"My Tag")
///

doc ///
	Key
		formatGraphicPrimitives
	Headline
		create a FormattedGraphicPrimitives object
///

doc ///
	Key
		(formatGraphicPrimitives,BasicList,HashTable)
	Headline
		create a formated graphic primitive object
	Usage
		textTag(gplist,h)	
	Inputs
		gplist: BasicList
		h: HashTable 
			containing graphic options 
	Outputs
		: FormattedGraphicPrimitives 
	Description
		Example
			myPoint1=point(20.,30.)	
			myPoint2=point(30.,40.)	
			myOptions = hashTable {"fill"=>"black"}
			formatGraphicPrimitives({myPoint1,myPoint2},myOptions)
///

TEST ///
	mylist={point(20.,30.),point(30.,40.)};
	myOptions = hashTable {"fill"=>"black"};
	assert(formatGraphicPrimitives(mylist,myOptions)===new FormattedGraphicPrimitives from {mylist,myOptions})
///

doc ///
	Key
		picture
	Headline
		create a Picture object
///

doc ///
	Key
		(picture,BasicList)
	Headline
		create a picture object
	Usage
		picture(fgplist,h)	
	Inputs
		fgplist: BasicList
			a list of FormattedGraphicPrimitives
	Outputs
		: Picture 
	Description
		Example
			myfgp1=formatGraphicPrimitives({point(20.,30.),point(30.,40.)},hashTable {"fill"=>"black"})
			myfgp2=formatGraphicPrimitives({circle(point(20.,30.),10)},hashTable {"fill"=>"red"})
			picture({myfgp1,myfgp2})
///

doc ///
	Key
		pictureZone
	Headline
		find the zone in which a picture lies
///

doc ///
	Key
		(pictureZone,Point2D)
	Headline
		find the zone that contains the point 
	Usage
		pictureZone(p)	
	Inputs
		p: Point2D 
	Outputs
		: List
			of the form {xmin,xmax,ymin,ymax}
	Description
		Example
			pictureZone(point(20,30))
///

TEST ///
	assert(pictureZone(point(20,30))==={20,20,30,30})
///

doc ///
	Key
		(pictureZone,Point3D)
	Headline
		find the zone that contains the point 
	Usage
		pictureZone(p)	
	Inputs
		p: Point3D 
	Outputs
		: List
			of the form {xmin,xmax,ymin,ymax,zmin,zmax}
	Description
		Example
			pictureZone(point(20,30,40))
///

TEST ///
	assert(pictureZone(point(20,30,40))==={20,20,30,30,40,40})
///

doc ///
	Key
		(pictureZone,Segment2D)
	Headline
		find the zone that contains the line segment 
	Usage
		pictureZone(l)	
	Inputs
		l: Segment2D 
	Outputs
		: List
			of the form {xmin,xmax,ymin,ymax}
	Description
		Example
			pictureZone(segment(point(20,30),point(30,40)))
///

TEST ///
	assert(pictureZone(segment(point(20,30),point(30,40)))==={20,30,30,40})
///

doc ///
	Key
		(pictureZone,Segment3D)
	Headline
		find the zone that contains the line segment 
	Usage
		pictureZone(l)	
	Inputs
		l: Segment3D 
	Outputs
		: List
			of the form {xmin,xmax,ymin,ymax,zmin,zmax}
	Description
		Example
			pictureZone(segment(point(20,30,40),point(30,40,50)))
///

TEST ///
	assert(pictureZone(segment(point(20,30,40),point(30,40,50)))==={20,30,30,40,40,50})
///

doc ///
	Key
		(pictureZone,Circle)
	Headline
		find the zone that contains the circle 
	Usage
		pictureZone(c)
	Inputs
		c: Circle 
	Outputs
		: List
			of the form {xmin,xmax,ymin,ymax}
	Description
		Example
			pictureZone(circle(point(20,30),10))
///

TEST ///
	assert(pictureZone(circle(point(20,30),10))==={10,30,20,40})
///

doc ///
	Key
		(pictureZone,Sphere)
	Headline
		find the zone that contains the sphere 
	Usage
		pictureZone(s)	
	Inputs
		s: Sphere
	Outputs
		: List
			of the form {xmin,xmax,ymin,ymax,zmin,zmax}
	Description
		Example
			pictureZone(sphere(point(20,30,40),10))
///

TEST ///
	assert(pictureZone(sphere(point(20,30,40),10))==={10,30,20,40,30,50})
///

doc ///
	Key
		(pictureZone,Polygon2D)
	Headline
		find the zone that contains the polygon 
	Usage
		pictureZone(p)	
	Inputs
		p: Polygon2D 
	Outputs
		: List
			of the form {xmin,xmax,ymin,ymax}
	Description
		Example
			pictureZone(polygon({point(20,30),point(25,30),point(20,35)}))
///

TEST ///
	assert(pictureZone(polygon({point(20,30),point(25,30),point(20,35)}))==={20,25,30,35})
///

doc ///
	Key
		(pictureZone,Polygon3D)
	Headline
		find the zone that contains the polygon 
	Usage
		pictureZone(p)	
	Inputs
		p: Polygon3D 
	Outputs
		: List
			of the form {xmin,xmax,ymin,ymax,zmin,zmax}
	Description
		Example
			pictureZone(polygon({point(20,30,40),point(25,30,40),point(20,35,45)}))
///

TEST ///
	assert(pictureZone(polygon({point(20,30,40),point(25,30,40),point(20,35,40)}))==={20,25,30,35,40,40})
///

doc ///
	Key
		(pictureZone,TextTag)
	Headline
		find the zone that contains the tag 
	Usage
		pictureZone(t)	
	Inputs
		t: TextTag 
	Outputs
		: List
			of the form {xmin,xmax,ymin,ymax} or {xmin,xmax,ymin,ymax,zmin,zmax}
	Description
		Example
			pictureZone(textTag(point(20,30),"my tag"))
			pictureZone(textTag(point(20,30,40),"my tag"))
///

TEST ///
	assert(pictureZone(textTag(point(20,30),"my tag"))==={20,20,30,30})
	assert(pictureZone(textTag(point(20,30,40),"my tag"))==={20,20,30,30,40,40})
///

doc ///
	Key
		(pictureZone,FormattedGraphicPrimitives)
	Headline
		find the zone that contains the whole list of graphic primitive
	Usage
		pictureZone(fgp)	
	Inputs
		fgp: FormattedGraphicPrimitives 
	Outputs
		: List
			of the form {xmin,xmax,ymin,ymax} (or {xmin,xmax,ymin,ymax,zmin,zmax} for 3D objects) describing a zone containing the object fgp
	Description
		Example
			pictureZone(formatGraphicPrimitives({circle(point(20,30),10)},hashTable {"fill"=>"black"}))
///

TEST ///
	assert(pictureZone(formatGraphicPrimitives({circle(point(20,30),10),point(50,40)},hashTable {"fill"=>"black"}))==={10,50,20,40})
///

doc ///
	Key
		(pictureZone,Picture)
	Headline
		find the zone that contains the whole picture 
	Usage
		pictureZone(mypict)	
	Inputs
		mypict: Picture 
	Outputs
		: List
			of the form {xmin,xmax,ymin,ymax} (or {xmin,xmax,ymin,ymax,zmin,zmax} for 3D objects) describing a zone containing the object fgp
	Description
		Example
			myfgp1=formatGraphicPrimitives({point(20.,30.),point(30.,40.)},hashTable {"fill"=>"black"})
			myfgp2=formatGraphicPrimitives({circle(point(20.,30.),10)},hashTable {"fill"=>"red"})
			mypict=picture({myfgp1,myfgp2})
			pictureZone(mypict)
///

TEST ///
	myfgp1=formatGraphicPrimitives({point(20.,30.),point(30.,40.)},hashTable {"fill"=>"black"});
	myfgp2=formatGraphicPrimitives({circle(point(20.,30.),10)},hashTable {"fill"=>"red"});
	mypict=picture({myfgp1,myfgp2});
	assert(pictureZone(mypict)==={10,30,20,40})
///

doc ///
	Key
		mergeFormattedGraphicPrimitives
	Headline
		merge two lists of graphic primitives with options	
///

doc ///
	Key
		(mergeFormattedGraphicPrimitives,FormattedGraphicPrimitives,FormattedGraphicPrimitives)
	Headline
		merge two lists of graphic primitives with options
	Usage
		mergeFormattedGraphicPrimitives(fgp1,fgp2)	
	Inputs
		fgp1: FormattedGraphicPrimitives 
		fgp2: FormattedGraphicPrimitives 
	Outputs
		: FormattedGraphicPrimitives
	Description
		Text
			The list of graphic primitives are concatenated and the graphic options are merged according to the value of two options {\tt "keep"} (either "1or2", "1" or "2") and {\tt "priority"} (either "1" or "2").
		Example
			myoptions1= new HashTable from {"fill"=>"black"}
			myoptions2= new HashTable from {"fill"=>"blue","stroke"=>"red"}
			fgp1=formatGraphicPrimitives({point(20,30),point(40,50)},myoptions1)
			fgp2=formatGraphicPrimitives({circle(point(30,40),10)},myoptions2)
			mergeFormattedGraphicPrimitives(fgp1,fgp2)
			mergeFormattedGraphicPrimitives(fgp1,fgp2,"keep"=>"2","priority"=>"1")
///

doc ///
	Key
		"defaultSVGOptions"
	Headline
		the default SVG options
	Description
		Text
			It's a hash table containing the default SVG options for all graphic primitives that can be used to create an SVG document.
///

doc ///
	Key
		"defaultSVGValues"
	Headline
		the default SVG values of coordinates 
	Description
		Text
			It's a hash table containing the default SVG values for some graphic primitives. They can be used while creating an SVG document.
///

doc ///
	Key
		"possibleSVGOptions"
	Headline
		the possible SVG options
	Description
		Text
			It's a hash table containing the possible SVG options for graphic primitives that can be used to create an SVG picture. Options not listed here will usually be discarded when the primitive is turned into an SVG object.
///

doc ///
	Key
		"defaultSVGHeading"
	Headline
		the default SVG heading 
	Description
		Text
			It's a string containing the default SVG heading used to create SVG pictures. 
///

doc ///
	Key
		defaultSVGOpening
	Headline
		the default SVG opening 
///

doc ///
	Key
		(defaultSVGOpening,ZZ,ZZ)
	Headline
		default SVG opening for a picture of a given size
	Usage
		defaultSVGOpening(w,h)	
	Inputs
		w: ZZ 
		h: ZZ
	Outputs
		: String
	Description
		Text
			It returns a string containing the default SVG opening of the form <svg etc. used to create an SVG picture.
///

doc ///
	Key
		"defaultSVGClosing"
	Headline
		the default SVG closing 
	Description
		Text
			It's a string containing the default SVG closing </svg> used to create SVG pictures. 
///

doc ///
	Key
		svgObject
	Headline
		create an SVG object 
///

doc ///
	Key
		(svgObject,FormattedGraphicPrimitives)
	Headline
		create a string describing in SVG a list of formatted graphic primitive object
	Usage
		svgObject(fgp)	
	Inputs
		fgp: FormattedGraphicPrimitives 
	Outputs
		: String
			containing the description of an object in SVG 
	Description
		Text
			The options that are not given in the hash table part of {\tt fgp} are set at the default values contained in {\tt defaultSVGOptions}.
		Example
			myoptions = hashTable {"fill"=>"black"}
			myfgp= formatGraphicPrimitives({point(20,30)},myoptions)
			svgObject(myfgp)
///

doc ///
	Key
		(svgObject,Picture)
	Headline
		create a string describing the picture in SVG 
	Usage
		svgObject(mypict)	
	Inputs
		mypict: Picture 
	Outputs
		: String
			containing the description of the picture in SVG 
	Description
		Example
			myfgp1=formatGraphicPrimitives({point(20.,30.),point(30.,40.)},hashTable {"fill"=>"black"})
			myfgp2=formatGraphicPrimitives({circle(point(20.,30.),10)},hashTable {"fill"=>"red"})
			mypict=picture({myfgp1,myfgp2})
			svgObject(mypict)
	SeeAlso
		svgPicture	
		viewPicture
///

doc ///
	Key
		(svgObject,Point2D,HashTable)
	Headline
		create the string describing a point in SVG 
	Usage
		svgObject(mypoint,myoptions)	
	Inputs
		mypoint: Point2D 
		myoptions: HashTable
			giving values to some options (see @TO "defaultSVGOptions"@).
	Outputs
		: String
			containing the description of the point in SVG 
	Description
		Text
			The options that are not given in {\tt myoptions} are set at the default values contained in @TO "defaultSVGOptions"@.
		Example
			myoptions = hashTable {"fill"=>"black"}
			svgObject(point(20,30),myoptions)
///

doc ///
	Key
		(svgObject,Segment2D,HashTable)
	Headline
		create the string describing a line segment in SVG 
	Usage
		svgObject(mysegment,myoptions)	
	Inputs
		mysegment: Segment2D 
		myoptions: HashTable
			giving values to some options (see @TO "defaultSVGOptions"@).
	Outputs
		: String
			containing the description of the segment in SVG 
	Description
		Text
			The options that are not given in {\tt myoptions} are set at the default values contained in @TO "defaultSVGOptions"@.
		Example
			myoptions = hashTable {"stroke-width"=>"2"}
			svgObject(segment(point(20,30),point(40,50)),myoptions)
///

doc ///
	Key
		(svgObject,Circle,HashTable)
	Headline
		create the string describing a circle in SVG 
	Usage
		svgObject(mycircle,myoptions)	
	Inputs
		mycircle: Circle 
		myoptions: HashTable
			giving values to some options (see @TO "defaultSVGOptions"@).
	Outputs
		: String
			containing the description of the circle in SVG 
	Description
		Text
			The options that are not given in {\tt myoptions} are set at the default values contained in @TO "defaultSVGOptions"@.
		Example
			myoptions = hashTable {"fill"=>"black"}
			svgObject(circle(point(20,30),10),myoptions)
///

doc ///
	Key
		(svgObject,Polygon2D,HashTable)
	Headline
		create the string describing a circle in SVG 
	Usage
		svgObject(mypolygon,myoptions)	
	Inputs
		mypolygon: Polygon2D 
		myoptions: HashTable
			giving values to some options (see @TO "defaultSVGOptions"@).
	Outputs
		: String
			containing the description of the polygon in SVG 
	Description
		Text
			The options that are not given in {\tt myoptions} are set at the default values contained in @TO "defaultSVGOptions"@.
		Example
			myoptions = hashTable {"fill"=>"black"}
			svgObject(polygon({point(20,30),point(40,50),point(10,40)}),myoptions)
///

doc ///
	Key
		(svgObject,TextTag,HashTable)
	Headline
		create the string describing a circle in SVG 
	Usage
		svgObject(mytext,myoptions)	
	Inputs
		mytext: TextTag 
		myoptions: HashTable
			giving values to some options (see @TO "defaultSVGOptions"@).
	Outputs
		: String
			containing the description of the text tag in SVG 
	Description
		Text
			The options that are not given in {\tt myoptions} are set at the default values contained in @TO "defaultSVGOptions"@.
		Example
			myoptions = hashTable {"font-family"=>"verdana"}
			svgObject(textTag(point(20,30),"Hello world"),myoptions)
///

doc ///
	Key
		svgPicture
	Headline
		create an SVG picture 
///

doc ///
	Key
		(svgPicture,Picture,ZZ,ZZ)
	Headline
		create an SVG picture from a Picture object 
	Usage
		svgPicture(mypict,w,h)	
	Inputs
		mypict: Picture 
		w: ZZ
			the width of the picture
		h: ZZ
			the height of the picture
	Outputs
		: String
			containing the SVG picture 
	Description
		Text
			The heading used is stored in @TO "defaultSVGHeading"@ and the SVG opening bracket (resp. closing) used is stored in @TO "defaultSVGOpening"@ (resp. @TO "defaultSVGClosing"@).
		Example
			myfgp1 = formatGraphicPrimitives({point(20,30)},hashTable {"fill"=>"black"})
			myfgp2 = formatGraphicPrimitives({circle(point(20,30),10),point(50,60)},hashTable {"fill"=>"red","fill-opacity"=>"0.2"})
			mypict = picture {myfgp1,myfgp2}
			svgPicture(mypict,100,100)
	SeeAlso
		(svgPicture,Picture)
		(svgPicture,Picture,ZZ,ZZ,String)
		(svgPicture,Picture,String)
		(pgfPicture,Picture)
///

doc ///
	Key
		(svgPicture,Picture)
	Headline
		create an SVG picture from a Picture object 
	Usage
		svgPicture(mypict)	
	Inputs
		mypict: Picture 
	Outputs
		: String
			containing the SVG picture 
	Description
		Text
			The heading used is stored in @TO "defaultSVGHeading"@ and the SVG opening bracket (resp. closing) used is stored in @TO "defaultSVGOpening"@ (resp. @TO "defaultSVGClosing"@).
		Example
			myfgp1 = formatGraphicPrimitives({point(20,30)},hashTable {"fill"=>"black"})
			myfgp2 = formatGraphicPrimitives({circle(point(20,30),10),point(50,60)},hashTable {"fill"=>"red","fill-opacity"=>"0.2"})
			mypict = picture {myfgp1,myfgp2}
			svgPicture(mypict)
	SeeAlso
		(svgPicture,Picture,ZZ,ZZ)
		(svgPicture,Picture,ZZ,ZZ,String)
		(svgPicture,Picture,String)
		(pgfPicture,Picture)
///

doc ///
	Key
		(svgPicture,Picture,String)
	Headline
		create an SVG picture from a Picture object and store it in a file
	Usage
		svgPicture(mypict,filename)	
	Inputs
		mypict: Picture 
		filename: String
			the name of the storage file
	Description
		Text
			If the filename is without a .svg suffix, it will be added.
		Text
			The heading used is stored in @TO "defaultSVGHeading"@ and the SVG opening bracket (resp. closing) used is stored in @TO "defaultSVGOpening"@ (resp. @TO "defaultSVGClosing"@).
	SeeAlso
		(svgPicture,Picture)
		(pgfPicture,Picture)
///

doc ///
	Key
		(svgPicture,Picture,ZZ,ZZ,String)
	Headline
		create an SVG picture from a Picture object and store it in a file
	Usage
		svgPicture(mypict,w,h,filename)	
	Inputs
		mypict: Picture 
		w: ZZ
			the width of the SVG picture
		h: ZZ
			the height of the SVG picture
		filename: String
			the name of the storage file
	Description
		Text
			If the filename is without a .svg suffix, it will be added.
		Text
			The heading used is stored in @TO "defaultSVGHeading"@ and the SVG opening bracket (resp. closing) used is stored in @TO "defaultSVGOpening"@ (resp. @TO "defaultSVGClosing"@).
	SeeAlso
		(svgPicture,Picture)
		(pgfPicture,Picture)

///

doc ///
	Key
		"defaultPGFOptions"
	Headline
		the default pgf options
	Description
		Text
			It's a hash table containing the default pgf options for graphic primitives that can be used to create a pgf picture.
///

doc ///
	Key
		"possiblePGFOptions"
	Headline
		the possible pgf options
	Description
		Text
			It's a hash table containing the possible pgf options for graphic primitives that can be used to create a pgf picture. Options not listed here will usually be discarded.
///

doc ///
	Key
		"defaultPGFValues"
	Headline
		the default pgf values
	Description
		Text
			It's a hash table containing the default pgf values for some graphic primitives that are used to create a pgf picture.
///

doc ///
	Key
		pgfObject
	Headline
		create a pgf object 
///

doc ///
	Key
		(pgfObject,Point2D,HashTable)
	Headline
		create the string describing a point in pgf 
	Usage
		pgfObject(mypoint,myoptions)	
	Inputs
		mypoint: Point2D 
		myoptions: HashTable
			giving values to some options
	Outputs
		: String
			containing the description of the point in pgf 
	Description
		Text
			The options that are not given in {\tt myoptions} are set at their default values if they are contained in @TO "defaultPGFOptions"@. Options not listed in @TO "possiblePGFOptions"@ are discarded. The size of the point comes from @TO "defaultPGFValues"@.
		Example
			myoptions = hashTable {"fill"=>"red"}
			pgfObject(point(20,30),myoptions)
///

doc ///
	Key
		(pgfObject,Segment2D,HashTable)
	Headline
		create the string describing a line segment in pgf 
	Usage
		pgfObject(mysegment,myoptions)	
	Inputs
		mysegment: Segment2D 
		myoptions: HashTable
			giving values to some options
	Outputs
		: String
			containing the description of the segment in pgf 
	Description
		Text
			The options that are not given in {\tt myoptions} are set at their default values if they are contained in @TO "defaultPGFOptions"@. Options not listed in @TO "possiblePGFOptions"@ are discarded. 
		Example
			myoptions = hashTable {"fill"=>"black"}
			pgfObject(segment(point(20,30),point(40,50)),myoptions)
///

doc ///
	Key
		(pgfObject,Circle,HashTable)
	Headline
		create the string describing a circle in pgf 
	Usage
		pgfObject(mycircle,myoptions)	
	Inputs
		mycircle: Circle 
		myoptions: HashTable
			giving values to some options
	Outputs
		: String
			containing the description of the circle in pgf 
	Description
		Text
			The options that are not given in {\tt myoptions} are set at their default values if they are contained in @TO "defaultPGFOptions"@. Options not listed in @TO "possiblePGFOptions"@ are discarded. 
		Example
			myoptions = hashTable {"fill"=>"black"}
			pgfObject(circle(point(20,30),10),myoptions)
///

doc ///
	Key
		(pgfObject,Polygon2D,HashTable)
	Headline
		create the string describing a circle in pgf 
	Usage
		svgObject(mypolygon,myoptions)	
	Inputs
		mypolygon: Polygon2D 
		myoptions: HashTable
			giving values to some options
	Outputs
		: String
			containing the description of the polygon in pgf 
	Description
		Text
			The options that are not given in {\tt myoptions} are set at their default values if they are contained in @TO "defaultPGFOptions"@. Options not listed in @TO "possiblePGFOptions"@ are discarded. 
		Example
			myoptions = hashTable {"fill"=>"black"}
			pgfObject(polygon({point(20,30),point(40,50),point(10,40)}),myoptions)
///

doc ///
	Key
		(pgfObject,TextTag,HashTable)
	Headline
		create the string describing a circle in pgf 
	Usage
		pgfObject(mytext,myoptions)	
	Inputs
		mytext: TextTag 
		myoptions: HashTable
			giving values to some options
	Outputs
		: String
			containing the description of the text tag in pgf 
	Description
		Text
			The options that are not given in {\tt myoptions} are set at their default values if they are contained in @TO "defaultPGFOptions"@. Options not listed in @TO "possiblePGFOptions"@ are discarded. 
		Example
			myoptions = hashTable {"fill"=>"black"}
			pgfObject(textTag(point(20,30),"Hello world"),myoptions)
///

doc ///
	Key
		(pgfObject,FormattedGraphicPrimitives)
	Headline
		create the string describing a series of graphic primitives in pgf 
	Usage
		pgfObject(FGP)	
	Inputs
		FGP: FormattedGraphicPrimitives
	Outputs
		: String
			containing the description of all the graphic primitives contained in the FormattedGraphicPrimitives in pgf 
	Description
		Text
			The options that are not given in the hash table part of FGP are set at their default values if they are contained in @TO "defaultPGFOptions"@. Options not listed in @TO "possiblePGFOptions"@ are discarded. 
		Example
			myoptions = hashTable {"fill"=>"black"}
			myfgp= formatGraphicPrimitives({point(20,30)},myoptions)
			pgfObject(myfgp)
///

doc ///
	Key
		(pgfObject,Picture)
	Headline
		create the string describing a series of graphic primitives in pgf 
	Usage
		pgfObject(pict)	
	Inputs
		pict: Picture 
	Outputs
		: String
			containing the description of all the graphic primitives contained in the Picture in pgf 
	Description
		Text
			The options that are not given in the hash table parts of the various FormattedGraphicPrimitives in the picture are set at their default values if they are contained in @TO "defaultPGFOptions"@. Options not listed in @TO "possiblePGFOptions"@ are discarded. 
		Example
			myfgp1=formatGraphicPrimitives({point(20.,30.),point(30.,40.)},hashTable {"fill"=>"black"})
			myfgp2=formatGraphicPrimitives({circle(point(20.,30.),10)},hashTable {"fill"=>"red"})
			mypict=picture({myfgp1,myfgp2})
			pgfObject(mypict)
///

doc ///
	Key
		pgfPicture
	Headline
		create a pgf picture 
///

doc ///
	Key
		(pgfPicture,Picture)
	Headline
		create a pgf picture from a Picture object 
	Usage
		pgfPicture(mypict)	
	Inputs
		mypict: Picture 
	Outputs
		: String
			containing the pgf picture 
	Description
		Example
			myfgp1 = formatGraphicPrimitives({point(20,30)},hashTable {"fill"=>"black"})
			myfgp2 = formatGraphicPrimitives({circle(point(20,30),10),point(50,60)},hashTable {"fill"=>"red","fill-opacity"=>"0.2"})
			mypict = picture {myfgp1,myfgp2}
			pgfPicture(mypict)
	SeeAlso
		(pgfPicture,Picture,HashTable)
		(svgPicture,Picture)

///

doc ///
	Key
		(pgfPicture,Picture,HashTable)
	Headline
		create a pgf picture from a Picture object with some options 
	Usage
		pgfPicture(mypict,myoptions)	
	Inputs
		mypict: Picture 
		myoptions: HashTable
	Outputs
		: String
			containing the picture in pgf
	Description
		Example
			myfgp1 = formatGraphicPrimitives({point(20,30)},hashTable {"fill"=>"black"})
			myfgp2 = formatGraphicPrimitives({circle(point(20,30),10),point(50,60)},hashTable {"fill"=>"red","fill-opacity"=>"0.2"})
			mypict = picture {myfgp1,myfgp2}
			myoptions = hashTable{"scale"=>"0.5","x"=>"0.1cm","y"=>"0.2cm"}
			pgfPicture(mypict,myoptions)
	SeeAlso
		(pgfPicture,Picture)
		(svgPicture,Picture)

///

doc ///
	Key
		(pgfPicture,Picture,String)
	Headline
		create a pgf picture from a Picture object and store it in a file
	Usage
		pgfPicture(mypict,filename)	
	Inputs
		mypict: Picture 
		filename: String
			the name of the storage file
	Description
		Text
			If the filename is without a .tex suffix, it will be added.
	SeeAlso
		(pgfPicture,Picture,HashTable,String)
		(pgfPicture,Picture)
		(pgfPicture,Picture,HashTable)
		(svgPicture,Picture)
///

doc ///
	Key
		(pgfPicture,Picture,HashTable,String)
	Headline
		create a pgf picture from a Picture object with options and store it in a file
	Usage
		pgfPicture(mypict,opts,filename)	
	Inputs
		mypict: Picture 
		opts: HashTable
			containing the options
		filename: String
			the name of the storage file
	Description
		Text
			If the filename is without a .tex suffix, it will be added.
	SeeAlso
		(pgfPicture,Picture,String)
		(pgfPicture,Picture)
		(pgfPicture,Picture,HashTable)
		(svgPicture,Picture)
///

doc ///
	Key
		viewPicture
	Headline
		view a picture in a browser
///

doc ///
	Key
		(viewPicture,String)
	Headline
		view a Picture in a browser
	Usage
		viewPicture(filename)
	Inputs
		filename: String
			the name of the file (possibly with path) containing the picture
	Caveat
		For the moment, it only works with SVG pictures and browsers supporting them.
///

