-- -*- coding: utf-8 -*-
newPackage(
        "VectorGraphics",
        Version => "0.92",
        Date => "May 18, 2018",
        Authors => {{Name => "Paul Zinn-Justin",
                  Email => "pzinn@unimelb.edu.au",
                  HomePage => "http://http://blogs.unimelb.edu.au/paul-zinn-justin/"}},
        Headline => "A package to produce SVG graphics",
	Keywords => {"Graphics"},
        DebuggingMode => false,
	AuxiliaryFiles => true,
	PackageImports => {"Text"},
	PackageExports => {"Text"}
        )

export{"GraphicsType", "GraphicsObject", "GraphicsPoly",
    "GraphicsList", "Circle", "Light", "Ellipse", "Path", "Polygon", "Polyline", "GraphicsText", "Line", "GraphicsHtml",
    "gList", "viewPort", "is3d", "distance", "rotation", "translation", "linearGradient", "radialGradient", "arrow", "plot",
    "Contents", "TextContent", "HtmlContent", "OneSided", "RadiusX", "RadiusY", "Specular", "Point1", "Point2", "Point", "SizeX", "SizeY", "ViewPort",
    "Perspective", "FontSize", "AnimMatrix", "TransformMatrix", "Points", "Radius",
    "Blur", "Static", "PathList", "Axes", "Margin", "Mesh",
    "SVG", "SVGElement"
    }

protect Filter
protect Distance
protect Is3d
protect Anim
protect CurrentMatrix
protect ScaledRadius
protect ScaledRadiusX
protect ScaledRadiusY
protect GraphicsId

debug Core

-- for now data-* need entering manually
htmlData={ "data-matrix","data-dmatrix","data-pmatrix","data-center","data-r","data-rx","data-ry","data-coords","data-onesided","data-origin","data-point","data-point1","data-point2","data-fontsize"}
svgAttr= htmlAttr | htmlData | { "transform", "filter" } -- what else ?

GraphicsObject = new Type of HashTable -- ancestor type

new GraphicsObject from List := (T,l) -> hashTable append(l,symbol cache => new CacheTable); -- every Graphics object should have a cache
new GraphicsObject := T -> new T from {};
new GraphicsObject from OptionTable := (T,o) -> o ++ {symbol cache => new CacheTable};
GraphicsObject ++ List := (opts1, opts2) -> merge(opts1,new class opts1 from opts2,
    (x,y) -> if instance(x,Matrix) and instance(y,Matrix) then y*x else y -- for TransformMatrix and AnimMatrix
    ) -- cf similar method for OptionTable

-- a bunch of options are scattered throughout the code:
-- * all dimensions are redefined as dimensionless quantities: Radius, FontSize, etc
-- * TransformMatrix for static transformation
-- * AnimMatrix for animation transformation
-- * OneSided for 3d paths, polygons
-- * Static for objects that can't be rotated
--   i.e, they or their contents can rotate/autorotate, but the rotations of their ancestors won't affect them
--   useful for lights
-- * Blur (amount of blurriness relative to the size of the object)
-- GLOBAL options (only work if in outermost object)
-- * SizeY / SizeX for picture sizes
-- * ViewPort for manual range of viewing window
-- * Perspective for 3d: can be a number or a whole 4d matrix (ideally, there'd be a function to translate...)
--   the matrix should be such that after transformation, the coordinates are (x,y,z,z/p) where the viewer is at (0,0,0) and the screen at z=-p
-- * Margin (leave blank around picture)
-- * Axes (draw axes)

-- 3d: turns on lights, axes are diff

GraphicsType = new Type of Type -- all usable Graphics objects are ~ self-initialized

gParseFlag := false;
gParse := method(Dispatch=>Thing)
gParse Sequence := x -> gParse vector(toList x)
gParse VisibleList := x -> apply(x,gParse)
gParse HashTable := x -> applyValues(x,gParse)
gParse CacheTable := identity
gParse Option := x -> x#0 => gParse x#1
gParse Thing := identity
gParse Matrix := x -> (
    if rank source x =!= rank target x or rank source x < 2 or rank source x > 4 then error "wrong matrix";
    if rank source x == 2 then x++1++1 else if rank source x == 3 then x++1 else x
    )
gParse Vector := x -> (
    if rank class x < 2 or rank class x > 4 then error "wrong coordinates";
    if rank class x === 2 then x || vector {0,1.} else (
	 gParseFlag=true; if rank class x === 3 then x || vector {1.} else if rank class x === 4 then x)
     )
gParse GraphicsObject := identity

GraphicsType List := (T,opts) -> (
    opts0 := T.Options;
    -- scan the first few arguments in case we skipped the keys for standard arguments. also, parse
    gParseFlag = false;
    temp := gParse(opts0 | apply(#opts, i -> if i < #opts0 and class opts#i =!= Option then opts0#i#0 => opts#i else opts#i));
    new T from (if gParseFlag then append(temp,symbol Is3d => true) else temp)
)

perspective = g -> (
    persp := if g.?Perspective then g.Perspective else 1000.; -- some arbitrary number
    if instance(persp,Matrix) then persp else matrix {{1,0,0,0},{0,1,0,0},{0,0,-1,persp},{0,0,0,persp}} -- output is {x,y,p-z,p}
    -- note in particular that distance : p = p-z : p = z' : w'
)

viewPort = g -> (
    if not g.cache.?ViewPort then svg g; -- need to be rendered
    g.cache.ViewPort
    )
viewPort1 := method() -- returns [xmin,ymin],[xmax,ymax]
viewPort1 GraphicsObject := x -> null

-- Is3d=false has two effects:
-- * the data-* stuff is lightened (can be recreated from the normal parameters)
-- * the event listeners for 3d rotating the object with the mouse are deactivated
-- * lighting is deactivated
is3d = x -> if x.?Is3d then x.Is3d else false;

distance = g -> (
    if not g.cache.?Distance then svg g; -- need to be rendered
    g.cache.Distance
    )
distance1 := method()
distance1 GraphicsObject := x -> 0_RR

scale := x -> x_3/x_2
project2d := x -> (scale x)*x^{0,1}
project2d' := x -> (scale x)*vector {x_0,-x_1} -- annoying sign

updateGraphicsCache := g -> (
    g.cache.ViewPort = viewPort1 g; -- update the range
    g.cache.Distance = distance1 g; -- update the distance
    if g.?OneSided and g.OneSided then determineSide g;
    -- bit of a hack: 2d objects Circle, Ellipse get scaled in a 3d context
    if instance(g,Circle) then (
	sc := scale(g.cache.CurrentMatrix*g.Center);
	g.cache.ScaledRadius=max(0,g.Radius*sc);
	) else if instance(g,Ellipse) then (
	sc = scale(g.cache.CurrentMatrix*g.Center);
	g.cache.ScaledRadiusX=max(0,g.RadiusX*sc);
	g.cache.ScaledRadiusY=max(0,g.RadiusY*sc);
	) else if instance(g,GraphicsText) then ( -- same for GraphicsText
	-- choose font size
	f := if g.?FontSize then g.FontSize else 14.;
	sc = scale(g.cache.CurrentMatrix*g.Point);
	f = max(0,f*sc);
	g.cache#"font-size"= toString f|"px";
	if instance(g,GraphicsHtml) then ( -- hack
	    g.cache#"overflow"="visible"; -- makes width/height irrelevant
	    g.cache#"width"=g.cache#"height"="100%"; -- but still needed otherwise webkit won't render
	    );
	);
    )

new GraphicsType of GraphicsObject from VisibleList := (T,T2,x) -> (
    g:=new MutableHashTable;
    g.Options=x#1; -- TODO: should it be an actual table? then have to suppress the BS syntax
    g.SVGElement = new MarkUpType of Hypertext;
    addAttribute(g.SVGElement,svgAttr | if #x>=3 then x#2 else {});
    g.SVGElement.qname = x#0;
    g)


Circle = new GraphicsType of GraphicsObject from ( "circle",
    { symbol Center => vector {0.,0.}, symbol Radius => 50. },
    { "r", "cx", "cy" }
    )
viewPort1 Circle := g -> (
    p := g.cache.CurrentMatrix * g.Center;
    r:=g.Radius*(scale p);
    p=project2d p;
    r = vector {r,r};
    { p - r, p + r }
    )
distance1 Circle := g -> (
    y := g.cache.CurrentMatrix * g.Center;
    y_2/y_3
    )

Ellipse = new GraphicsType of GraphicsObject from ( "ellipse",
    { symbol Center => vector {0.,0.}, symbol RadiusX => 50., symbol RadiusY => 50. },
    { "rx", "ry", "cx", "cy" }
    )
viewPort1 Ellipse := g -> (
    p := g.cache.CurrentMatrix * g.Center;
    sc := scale p;
    rx:=g.RadiusX*sc; ry:=g.RadiusY*sc;
    p=project2d p;
    r := vector {rx,ry};
    { p - r, p + r }
    )
distance1 Ellipse := g -> (
    y := g.cache.CurrentMatrix * g.Center;
    y_2/y_3
    )

GraphicsText = new GraphicsType of GraphicsObject from ( "text",
    { Point => vector {0.,0.}, symbol TextContent => "" },
    { "x", "y" }
    )
viewPort1 GraphicsText := g -> (
    f := if g.?FontSize then g.FontSize else 14.;
    p := g.cache.CurrentMatrix * g.Point;
    f=f*scale p;
    p=project2d p;
    r := vector { f*0.6*length g.TextContent, 0.8*f }; -- width/height. very approximate TODO properly
    pp := p + vector {
	if g#?"text-anchor" then (if g#"text-anchor" == "middle" then -0.5*r_0 else if g#"text-anchor" == "end" then -r_0 else 0) else 0,
	if g#?"dominant-baseline" then (if g#"dominant-baseline" == "middle" then 0.5*r_1 else if g#"dominant-baseline" == "hanging" then -r_1 else 0) else 0
	};
    {pp,pp+r}
    )

Line = new GraphicsType of GraphicsObject from ( "line",
    { Point1 => vector {0.,0.}, Point2 => vector {50.,50.}},
    { "x1", "y1", "x2", "y2" }
    )
viewPort1 Line := g -> (
    p1 := project2d(g.cache.CurrentMatrix * g.Point1);
    p2 := project2d(g.cache.CurrentMatrix * g.Point2);
    p := transpose{entries p1,entries p2};
    { vector(min\p), vector(max\p) }
    )
distance1 Line := g -> (
    p1 := g.cache.CurrentMatrix * g.Point1;
    p2 := g.cache.CurrentMatrix * g.Point1;
    0.5*(p1_2/p1_3+p2_2/p2_3)
    )

GraphicsPoly = new Type of GraphicsObject;

Polyline = new GraphicsType of GraphicsPoly from ( "polyline", { symbol Points => {} }, { "points" } )
Polygon = new GraphicsType of GraphicsPoly from ( "polygon", { symbol Points => {} }, { "points" } )
Path = new GraphicsType of GraphicsPoly from ( "path", { symbol PathList => {} }, { "d" } )
viewPort1 GraphicsPoly := g -> ( -- relative coordinates *not* supported, screw this
    if instance(g,Path) then s := select(g.PathList, x -> instance(x,Vector)) else s = g.Points;
    s = transpose apply(s, x -> entries project2d (g.cache.CurrentMatrix*x));
    {vector(min\s), vector(max\s)}
    )

-- to make lists of them
GraphicsList = new GraphicsType of GraphicsObject from ( "g", { symbol Contents => {} } )
-- slightly simpler syntax: gList (a,b,c, opt=>xxx) rather than GraphicsList { {a,b,c}, opt=>xxx }, plus updates Is3d correctly
gList = x -> (
    x=flatten toList sequence x;
    x1 := select(x, y -> instance(y,GraphicsObject));
    x2 := select(x, y -> instance(y,Option));
    if any(x1,is3d) then x2 = append(x2, Is3d => true);
    GraphicsList append(x2,symbol Contents => x1)
    )
viewPort1 GraphicsList := x -> (
    s := nonnull apply(x.Contents, y->y.cache.ViewPort);
    if #s===0 then null else (
	s = transpose s;
	mn := transpose (entries \ s#0);
	mx := transpose (entries \ s#1);
	{vector (min\mn), vector(max\mx)}
    )
)

GraphicsHtml = new GraphicsType of GraphicsText from ( "foreignObject",
    { Point => vector {0.,0.}, symbol HtmlContent => null },
    { "x", "y" }
    )
viewPort1 GraphicsHtml := g -> (
    p := project2d (g.cache.CurrentMatrix * g.Point);
    { p, p } -- TODO properly
    )

--
anim := method()
anim GraphicsObject := x -> x.?AnimMatrix
anim GraphicsList := x -> (
    if not x.cache.?Anim then x.cache.Anim = x.?AnimMatrix or any(x.Contents,anim);
    x.cache.Anim
    )

SVG = new MarkUpType of Hypertext
addAttribute(SVG,svgAttr|{"height","preserveAspectRatio","viewBox","width","x","xmlns"=>"http://www.w3.org/2000/svg","y","zoomAndPan"})

--
stableSort = x -> if #x <= 1 then x else (
xx := transpose {x,toList(0..#x-1)};
(transpose sort xx)#0
)

-- for javascript stuff
jsString := method(Dispatch=>Thing)
jsString Thing := toString
jsString String := x -> "'" | x |"'"
jsString Matrix := x -> "matrix(" | jsString entries x | ")"
jsString Vector := x -> "vector(" | jsString entries x | ")"
jsString VisibleList := x -> "[" | demark(",",jsString\x) | "]"
--jsString HashTable := x -> "{" | demark(",",apply(pairs x, (key,val) -> jsString key | ":" | jsString val)) | "}"
jsString Option := x -> "times(" | jsString x#0 | "," | jsString x#1 | ")"

updateTransformMatrix := (g,m,p) -> ( -- (object,matrix,perspective matrix)
    g.cache.CurrentMatrix = if g.?Static and g.Static then p else m; -- if static reset to perspective matrix
    if g.?TransformMatrix then g.cache.CurrentMatrix = g.cache.CurrentMatrix*g.TransformMatrix;
    )

svgLookup := hashTable { -- should be more systematic
    symbol TransformMatrix => (x,m) -> "data-matrix" => jsString x,
    symbol AnimMatrix => (x,m) -> "data-dmatrix" => jsString x,
    symbol Center => (x,m) -> (
	x = project2d' (m*x);
	"cx" => toString x_0,
	"cy" => toString x_1
	),
    symbol ScaledRadius => (x,m) ->  "r" => toString x,
    symbol ScaledRadiusX => (x,m) ->  "rx" => toString x,
    symbol ScaledRadiusY => (x,m) ->  "ry" => toString x,
    symbol PathList => (x,m) -> "d" => demark(" ", flatten apply(x, y -> if instance(y,Vector) then apply(entries project2d'(m*y),toString) else y)),
    symbol Points => (x,m) -> "points" => demark(" ", flatten apply(x, y -> apply(entries project2d'(m*y),toString))),
    symbol Point => (x,m) -> (
	x = project2d' (m*x);
	"x" => toString x_0,
	"y" => toString x_1
	),
    symbol Point1 => (x,m) -> (
	x = project2d' (m*x);
	"x1" => toString x_0,
	"y1" => toString x_1
	),
    symbol Point2 => (x,m) -> (
	x = project2d' (m*x);
	"x2" => toString x_0,
	"y2" => toString x_1
	),
    symbol Static => (x,m) -> if x then "data-pmatrix" => jsString m,
    symbol GraphicsId => (x,m) -> "id" => x,
    symbol Filter => (x,m) -> "filter" => toString x,
    symbol Contents => (x,m) -> (
	x = toSequence stableSort x;
	apply(x, y -> y.cache.SVGElement)
	),
    symbol TextContent => (x,m) -> x,
    symbol HtmlContent => (x,m) -> html x
    }

svg3dLookup := hashTable { -- should be more systematic
    symbol Center => x -> "data-center" => jsString x,
    symbol Radius => x -> "data-r" => jsString x,
    symbol RadiusX => x -> "data-rx" => jsString x,
    symbol RadiusY => x -> "data-ry" => jsString x,
    symbol PathList => x -> "data-coords" => jsString x,
    symbol Points => x -> "data-coords" => jsString x,
    symbol Point => x -> "data-point" => jsString x,
    symbol Point1 => x -> "data-point1" => jsString x,
    symbol Point2 => x -> "data-point2" => jsString x,
    symbol OneSided => x -> "data-onesided" => jsString x,
    symbol FontSize => x -> "data-fontsize" => jsString x,
    }

-- produces SVG element hypertext
svg = method()
svg (GraphicsObject,Matrix,Matrix,List) := (g,m,p,l) -> ( -- (object,current matrix,perspective matrix,lights)
    if not (class g).?SVGElement then return;
    updateTransformMatrix(g,m,p);
    if g.?Contents then scan(g.Contents, x -> svg(x,g.cache.CurrentMatrix,p,l));
    updateGraphicsCache g;
    filter(g,l);
    full := new OptionTable from merge(g,g.cache,last);
    args := deepSplice apply(select(keys full,key->svgLookup#?key), key -> svgLookup#key(full#key,g.cache.CurrentMatrix));
    if is3d g then args = args | deepSplice apply(select(keys full,key -> svg3dLookup#?key), key -> svg3dLookup#key full#key);
    if hasAttribute(g,ReverseDictionary) then args = append(args, TITLE toString getAttribute(g,ReverseDictionary));
    g.cache.SVGElement = style((class g).SVGElement args,full)
    )

svg (GraphicsObject,Matrix,Matrix) := (g,m,p) -> svg(g,m,p,{})

svg (GraphicsObject,List) := (g,l) -> (
    p := perspective g;
    svg(g,p,p,l)
)

svg GraphicsObject := g -> svg(g,{})

--htmlWithTex GraphicsObject := html

globalAssignment GraphicsObject
toString GraphicsObject := g -> if hasAttribute(g,ReverseDictionary) then toString getAttribute(g,ReverseDictionary) else (lookup(toString,HashTable)) g
net GraphicsObject := g -> if hasAttribute(g,ReverseDictionary) then net getAttribute(g,ReverseDictionary) else (lookup(net,HashTable)) g
expression GraphicsObject := hold

distance1 GraphicsPoly := g -> (
    if instance(g,Path) then s := select(g.PathList, x -> instance(x,Vector)) else s = g.Points;
    sum(s,x->(xx:=g.cache.CurrentMatrix*x;xx_2/xx_3)) / #s
    )
distance1 GraphicsList := g -> (
    if #(g.Contents) == 0 then 0_RR else sum(g.Contents, distance) / #(g.Contents)
    )
GraphicsObject ? GraphicsObject := (x,y) -> (distance y) ? (distance x)
distance1 GraphicsText := g -> (
    y := g.cache.CurrentMatrix*g.Point;
    y_2/y_3
    )

graphicsIdCount := 0;
graphicsId := () -> (
    graphicsIdCount=graphicsIdCount+1;
    "Graphics_" | toString currentTime() | "_" | toString graphicsIdCount
    )

-- defs
svgDefs = new MarkUpType of Hypertext
svgDefs.qname="defs"
addAttribute(svgDefs,svgAttr)

scanDefs := g -> (
    lst := select(values g | values g.cache, y->instance(y,HypertextInternalLink));
    if g.?Contents then lst = lst | flatten apply(g.Contents,scanDefs);
    lst
    )


-- full SVG with the headers
new SVG from GraphicsObject := (S,g) -> (
    p := perspective g;
    lights := if is3d g then setupLights(g,p,p) else {};
    main := svg(g,p,p,lights); -- run this first because it will compute the ranges too
    if main === null then return {};
    if g.?ViewPort then r := g.ViewPort else r = g.cache.ViewPort; -- should be cached at this stage
    if r === null or r#0 == r#1 then (g.cache.SizeX=g.cache.SizeY=0.; return {}); -- nothing to draw
    r = apply(r,numeric);
    rr := r#1 - r#0;
    if rr_0 == 0 then (
	rr = vector { rr_1 * 16/10, rr_1 };
	r = { vector { r#0_0 - 0.5*rr_0, r#0_1 }, vector { r#1_0 + 0.5*rr_0, r#1_1 } };
	);
    if rr_1 == 0 then (
	rr = vector { rr_0, rr_0 * 10/16 };
	r = { vector { r#0_0, r#0_1 - 0.5*rr_1 }, vector {  r#1_0, r#1_1 + 0.5*rr_1 } };
	);
    -- axes
    axes:=null; axeslabels:=null; defsList:={};
    if g.?Axes and g.Axes =!= false then (
	arr := arrow();
	-- determine intersection of viewport with axes
	xmin := (r#0_0-p_(0,3))/p_(0,0);
	xmax := (r#1_0-p_(0,3))/p_(0,0);
	if xmax < xmin then ( temp:=xmin; xmin=xmax; xmax=temp; );
	ymin := (r#0_1-p_(1,3))/p_(1,1);
	ymax := (r#1_1-p_(1,3))/p_(1,1);
	if ymax < ymin then ( temp2:=ymin; ymin=ymax; ymax=temp2; );
	if is3d g then (
	    zmax := 0.25*(xmax-xmin+ymax-ymin);
	    zmin := -zmax;
	    );
	axes = gList(
	    Line { Point1 => vector {xmin,0,0,1}, Point2 => vector {xmax,0,0,1}, "marker-end" => arr },
	    Line { Point1 => vector {0,ymin,0,1}, Point2 => vector {0,ymax,0,1}, "marker-end" => arr },
	    if is3d g then Line { Point1 => vector{0,0,zmin,1}, Point2 => vector {0,0,zmax,1}, "marker-end" => arr },
	    "stroke"=>"black", "stroke-width"=>0.01*min(rr_0,rr_1)
	    );
	axeslabels = gList(
	    -- we use GraphicsHtml here despite limitations of ForeignObject. could use GraphicsText instead
	    GraphicsHtml { Point => vector {xmax*1.06,0,0,1}, HtmlContent => if instance(g.Axes,List) and #g.Axes>0 then g.Axes#0 else local x, FontSize => 0.08*min(rr_0,rr_1)},
	    GraphicsHtml { Point => vector {0,ymax*1.06,0,1}, HtmlContent => if instance(g.Axes,List) and #g.Axes>1 then g.Axes#1 else local y, FontSize => 0.08*min(rr_0,rr_1)},
	    if is3d g then GraphicsHtml { Point => vector {0,0,zmax*1.06,1}, HtmlContent => if instance(g.Axes,List) and #g.Axes>2 then g.Axes#2 else local z, FontSize => 0.08*min(rr_0,rr_1)}
	    );
	defsList = scanDefs axes | scanDefs axeslabels;
	axes=svg(axes,p,p);
	axeslabels=svg(axeslabels,p,p);
	);
    if g.?SizeX then g.cache.SizeX = numeric g.SizeX;
    if g.?SizeY then g.cache.SizeY = numeric g.SizeY;
    if not (g.?SizeX or g.?SizeY) then -- by default, make it fit inside 16 x 10
	if rr_0 > 1.6*rr_1 then g.cache.SizeX = 16. else g.cache.SizeY = 10.;
    -- at this stage one of the two is set
    if not g.cache.?SizeY then g.cache.SizeY = g.cache.SizeX * rr_1/rr_0;
    if not g.cache.?SizeX then g.cache.SizeX = g.cache.SizeY * rr_0/rr_1;
    -- put some extra blank space around picture
    margin := if g.?Margin then g.Margin else 0.1;
    r = { r#0-margin*rr, r#1+margin*rr }; rr = (1+2*margin)*rr;
    --
--    tag := graphicsId();
    ss := SVG {
	"preserveAspectRatio" => "none",
	"class" => "M2Svg",
--	"id" => tag,
	"style" => concatenate("width:",toString g.cache.SizeX,"em;",
	    "height:",toString g.cache.SizeY,"em;",
	    "stroke-linejoin:round;",
	    if not g#?"stroke-width" then "stroke-width:1%", -- define a default stroke-width
	),
	"viewBox" => concatenate between(" ",toString \ {r#0_0,-r#1_1,r#1_0-r#0_0,r#1_1-r#0_1}),
	"data-pmatrix" => jsString p
	};
    if is3d g then ss = append(ss, "onmousedown" => "gfxMouseDown.call(this,event)");
    if axes =!= null then ss = append(ss, axes);
    if axeslabels =!= null then ss = append(ss, axeslabels);
    ss = append(ss,main);
    defsList = unique ( defsList | scanDefs g );
    if #defsList>0 then ss=append(ss,svgDefs defsList);
    -- then autorotate button
    if anim g then (
	sizex := rr_0*min(0.5,1.5/g.cache.SizeX); sizey := rr_1*min(0.5,1.5/g.cache.SizeY); -- can't be larger than half the pic; default = 1.5em
	ss = append(ss,
	GraphicsList.SVGElement {
	    "transform" => "translate("|toString(r#0_0)|" "|toString(r#0_1)|") scale("|toString sizex|" "|toString sizey|")",
	    "class" => "gfxauto",
	    "onclick" => "gfxToggleRotation.call(this,event)",
	    Circle.SVGElement { "cx" => "0.5", "cy" => "0.5", "r" => "0.45", "style" => "fill:white; stroke:black; stroke-width:0.05" },
	    Polygon.SVGElement { "class" => "gfxautoplay", "points" => "0.3,0.25 0.8,0.5 0.3,0.75", "style" => "stroke:none; fill:black" },
	    Line.SVGElement { "class" => "gfxautostop", "x1" => "0.3", "y1" => "0.25", "x2" => "0.3", "y2" => "0.75", "style" => "stroke:black; stroke-width:0.15" },
	    Line.SVGElement { "class" => "gfxautostop", "x1" => "0.7", "y1" => "0.25", "x2" => "0.7", "y2" => "0.75", "style" => "stroke:black; stroke-width:0.15" }
	    }
	));
    ss
    )

html GraphicsObject := g -> html SVG g;

-- now transformations
-- following 2 functions can be used to produce matrices to be fed to either
-- AnimMatrix (animation) or TransformMatrix (static)

rotation = args -> (
    args = sequence args;
    if #args>3 then error("Too many arguments");
    angle := args#0;
    threeD :=  #args === 3 or (#args === 2 and (( instance(args#1,Vector) and rank class args#1 === 3 ) or ( instance(args#1,Sequence) and #args#1 === 3 )));
    axis := promote(if threeD then if instance(args#1,Vector) then args#1 else vector toList args#1 else vector {0,0,1},RR);
    invr := 1/sqrt(axis_0^2+axis_1^2+axis_2^2);
    axis = invr*axis;
    cross := (axis#0)**transpose(axis#0);
    rot := cross + (sin angle) * matrix {{0,-axis_2,axis_1},{axis_2,0,-axis_0},{-axis_1,axis_0,0}} + (cos angle) * (1-cross);
    rot = rot ++ 1;
    if (#args==2 and threeD) or #args==1 then rot else (
	center := gParse last args;
	(translation(center))*rot*(translation(-center))
	)
    )
translation = vec -> (
    vec = gParse vec;
    matrix {{1,0,0,vec_0},{0,1,0,vec_1},{0,0,1,vec_2},{0,0,0,1}}
)
-- scaling = x -> matrix{{x,0,0,0},{0,x,0,0},{0,0,x,0},{0,0,0,1}}; -- sadly atm strokeWidth *does not* scale with scaling

determineSide = method()
determineSide GraphicsObject := x -> ()
determineSide GraphicsPoly := g -> (
    -- find first 3 coords
    if instance(g,Path) then coords := select(g.PathList, x -> instance(x,Vector)) else coords = g.Points;
    if #coords<3 then ( remove(g.cache,Filter); return; );
    coords=apply(take(coords,3),x->g.cache.CurrentMatrix*x);
    coords = apply(coords, x -> (1/x_3)*x^{0,1});
    coords = {coords#1-coords#0,coords#2-coords#0};
    g.cache#"visibility" = if coords#0_0*coords#1_1-coords#0_1*coords#1_0 < 0 then "hidden" else "visible";
    )

-- lighting
Light = new GraphicsType of Circle from ( "circle",
    { symbol Center => vector {0,0,0,1.}, symbol Radius => 10, symbol Specular => 64, symbol Blur => 0.3, symbol Static => true, "opacity" => "0", "fill" => "#FFFFFF", "stroke" => "none" },
    { "r", "cx", "cy" } -- atm these are not inherited
    )
-- in case it's drawn, it's a circle

-- viewPort1 ignores lights if invisible
viewPort1 Light := g -> if g.Radius === 0 then null else (lookup(viewPort1,Circle)) g

setupLights = (g,m,p) -> (
    remove(g.cache,Filter); -- clean up filters from past
    if instance(g,Light) then (
    updateTransformMatrix(g,m,p);
    g.cache.GraphicsId = graphicsId();
    { g } ) else if g.?Contents then (
    updateTransformMatrix(g,m,p);
    flatten apply(g.Contents, x -> setupLights(x,g.cache.CurrentMatrix,p))
    ) else {}
) -- yeah, could make a method...

HypertextInternalLink = new Type of Hypertext -- could be useful elsewhere
toString HypertextInternalLink := net HypertextInternalLink := x -> (
    -- ideally we'd use "override" to get the tag, but...
    tag := (select(x, y -> instance(y,Option) and y#0==="id"))#0#1;
    "url(#"|tag|")"
)

svgFilter := new MarkUpType of HypertextInternalLink
addAttribute(svgFilter,svgAttr | {"x","y","width","height"})
svgFilter.qname="filter"
feGaussianBlur := new MarkUpType of Hypertext
addAttribute(feGaussianBlur,svgAttr|{"in","result","stdDeviation"})
feGaussianBlur.qname="feGaussianBlur";
feSpecularLighting := new MarkUpType of Hypertext
addAttribute(feSpecularLighting,svgAttr| {"result","specularExponent","lighting-color"})
feSpecularLighting.qname="feSpecularLighting"
fePointLight := new MarkUpType of Hypertext
addAttribute(fePointLight,svgAttr|{"x","y","z"})
fePointLight.qname="fePointLight"
feComposite := new MarkUpType of Hypertext
addAttribute(feComposite,svgAttr|{"in","in2","operator","result","k1","k2","k3","k4"})
feComposite.qname="feComposite"

filter = (g,l) -> if (g.?Blur and g.Blur != 0) or (#l > 0 and instance(g,GraphicsPoly) and g#?"fill") then (
    tag := graphicsId();
    i:=0;
    opts := { "id" => tag };
    if g.?Blur then (
	b := g.Blur;
	opts = opts | { "x" => toString(-100*b)|"%", "y" => toString(-100*b)|"%", "width" => toString(100*(1+2*b))|"%", "height" => toString(100*(1+2*b))|"%" };
	rng := g.cache.ViewPort; if rng =!= null then (
    	    drng:=rng#1-rng#0;
    	    r := b*min(drng_0,drng_1);
	    g.cache.ViewPort={rng#0-vector{r,r},rng#1+vector{r,r}}; -- a bit of a hack
	    opts = append(opts, feGaussianBlur { "in" => "SourceGraphic",
		    "result" => "result"|toString i, "stdDeviation" => toString(0.5*r) } ); -- problem is, this should be updated dynamically as radius changes...
	    i=i+1;
	)
    );
    if is3d g and instance(g,GraphicsPoly) and g#?"fill" then (
    	-- find first 3 coords
	if instance(g,Path) then coords := select(g.PathList, x -> instance(x,Vector)) else coords = g.Points;
    	if #coords>=3 then (
	    coords=apply(take(coords,3),x->(xx:=g.cache.CurrentMatrix*x;(1/xx_3)*xx^{0,1,2}));
	    u:=coords#1-coords#0; v:=coords#2-coords#0; w:=vector{u_1*v_2-v_1*u_2,u_2*v_0-v_2*u_0,u_0*v_1-v_0*u_1}; w2:=w_0*w_0+w_1*w_1+w_2*w_2;
	    if w_2>0 then w=-w;
	    scan(l, gg -> (
	    	    -- compute reflected coords
		    light0 := gg.cache.CurrentMatrix*gg.Center;
		    light := (1/light0_3)*light0^{0,1,2};
		    lightrel := light-coords#0;
	    	    sp := w_0*lightrel_0+w_1*lightrel_1+w_2*lightrel_2;
	    	    c := 2*sp/w2;
		    light = light - c*w;
		    opts = opts | {
			feSpecularLighting { "result" => "spec"|toString i, "specularExponent" => toString gg.Specular, "lighting-color" => if sp<0 then "black" else toString gg#"fill",
			    fePointLight { "data-origin" => gg.cache.GraphicsId, "x" => toString(light_0*light0_3/light_2), "y" => toString(-light_1*light0_3/light_2), "z" => toString(4*gg.Radius/light_2) } },
			feComposite { "in" => "spec"|toString i, "in2" => "SourceGraphic", "operator" => "in", "result" => "clipspec"|toString i },
			feComposite { "in" => (if i==0 then "SourceGraphic" else "result"|toString(i-1)),  "in2" => "clipspec"|toString i, "result" => "result"|toString i,
			    "operator" => "arithmetic", "k1" => "0", "k2" => "1", "k3" => "1", "k4" => "0" }
			};
	    	    i=i+1;
	    	    ));
	    );
	);
    if (g.cache.?Filter) then ( -- can happen if object used several times in a list
	g.cache#(g.cache.Filter)=g.cache.Filter;
	);
    g.cache.Filter=svgFilter opts;
    )

svgLinearGradient := new MarkUpType of HypertextInternalLink
addAttribute(svgLinearGradient,svgAttr|{"in","in2","operator","result","k1","k2","k3","k4"})
svgLinearGradient.qname="linearGradient"
svgRadialGradient := new MarkUpType of HypertextInternalLink
addAttribute(svgRadialGradient,svgAttr|{"in","in2","operator","result","k1","k2","k3","k4"} )
svgRadialGradient.qname="radialGradient"
svgStop := new MarkUpType of HypertextInternalLink
addAttribute(svgStop,svgAttr|{"offset"} )
svgStop.qname="stop"
linearGradient = true >> o -> stop -> (
    tag := graphicsId();
    svgLinearGradient prepend(
	"id" => tag,
	apply(pairs o, (key,val) -> key => val) -- lame but what to do
	|
	apply(stop,(offset,style) -> svgStop { "offset" => offset, "style" => style })
	)
    )
radialGradient = true >> o -> stop -> (
    tag := graphicsId();
    svgRadialGradient prepend(
	"id" => tag,
	apply(pairs o, (key,val) -> key => val) -- lame but what to do
	|
	apply(stop,(offset,style) -> svgStop { "offset" => offset, "style" => style })
	)
    )

GraphicsArrow = new OptionTable from gParse { symbol Points => { vector {0,0}, vector {0,4}, vector {3,2} }, "fill" => "black", "stroke" => "none", Is3d => false }
svgMarker := new MarkUpType of HypertextInternalLink
addAttribute(svgMarker,svgAttr|{ "orient" => "auto", "markerWidth" => "3", "markerHeight" => "4", "refX" => "0", "refY" => "2"})
svgMarker.qname="marker"

m := matrix {{1,0,0,0},{0,-1,0,0},{0,0,1,0},{0,0,0,1}}*perspective 1;

arrow = true >> o -> x -> (
    tag := graphicsId();
    svgMarker {
	"id" => tag,
	svg(new Polygon from (GraphicsArrow ++ gParse o),m,m)  -- eww
	}
    )

-* TODO recreate at some point
gfxLabel = true >> o -> label -> (
    tag := graphicsId();
    f:=1; -- TEMP
--    s:="<marker id='"|tag|"' markerUnits='userSpaceOnUse' markerSizeX='"|toString(f*0.6*length label)|"' markerSizeY='"|toString f|"' refX='0' refY='0'>"; -- very approximate
    s:="<marker id='"|tag|"' markerSizeX='100' markerSizeY='100' refX='0' refY='0'>"; -- very approximate
    saveTransformMatrix := currentTransformMatrix;
    s=s|(svg new GraphicsText from (new GraphicsObject) ++ { "fill" => "black", "stroke" => "none" } ++ gParse o ++ { Point => vector {0,0}, Content => label });
    currentTransformMatrix = saveTransformMatrix;
    s=s|"</marker>";
    new GraphicsIdged from (tag,s)
    )
*-

-- note that the range is only where the curve actually lies, not the original range "r" provided.
-- the reason is that it's not clear how to force that original range (there are possible coordinate transformations etc)
plot = true >> o -> (P,r) -> (
    pkg := needsPackage "NumericalAlgebraicGeometry"; -- probably overkill
    sS := value pkg.Dictionary#"solveSystem";
    pkg2 := needsPackage "NAGtypes";
    Crd := pkg2.Dictionary#"Coordinates";
    R := ring P; -- R should have one or two variables
    if not instance(r,List) then error("incorrect ranges");
    if not instance(r#0,List) then r = { r };
    if #r>2 or (numgens R =!= #r and numgens R =!= #r+1) then error("incorrect number of variables / ranges");
    if numgens R === #r then R2 := coefficientRing R else R2 = (coefficientRing R) ( monoid [last gens R] );
    if (#r === 1) then ( r = r#0;
	if (o.?Mesh) then n := o.Mesh else n = 100;
	val := transpose apply(n+1, i -> (
		x := i*(r#1-r#0)/n+r#0;
		f := map(R2,R, matrix { if numgens R === 1 then { x } else { x, R2_0 } });
		y := if numgens R === 1 then { f P } else sort apply(sS { f P }, p -> first p#Crd); -- there are subtle issues with sorting solutions depending on real/complex...
		apply(y, yy -> if abs imaginaryPart yy < 1e-6 then vector { x, realPart yy })));
	new GraphicsList from (
	    (new OptionTable from { "fill"=>"none", Axes=>gens R, Is3d=>false,
		symbol Contents => apply(val, v -> Path { flag:=true; PathList => flatten apply(v, w -> if w === null then (flag=true; {}) else first({ if flag then "M" else "L", w },flag=false))})
		}) ++ gParse o
	    )
	) else (
	if (o.?Mesh) then n = o.Mesh else n = 10;
	val = table(n+1,n+1,(i,j)->(
		x := i*(r#0#1-r#0#0)/n+r#0#0;
		y := j*(r#1#1-r#1#0)/n+r#1#0;
		f := map(R2,R, matrix { if numgens R === 2 then { x,y } else { x, y, R2_0 } });
		z := if numgens R === 2 then { f P } else sort apply(sS { f P }, p -> first p#Crd); -- there are subtle issues with sorting solutions depending on real/complex...
		apply(z, zz -> if abs imaginaryPart zz < 1e-6 then vector { x, y, realPart zz })));
	new GraphicsList from (
	    (new OptionTable from { Axes=>gens R, Is3d=>true,
		symbol Contents => flatten flatten table(n,n,(i,j) -> for k from 0 to min(#val#i#j,#val#(i+1)#j,#val#i#(j+1),#val#(i+1)#(j+1))-1 list (
			if val#i#j#k === null or val#(i+1)#j#k === null or val#i#(j+1)#k === null or val#(i+1)#(j+1)#k === null then continue;
			Polygon { Points => { val#i#j#k, val#(i+1)#j#k, val#(i+1)#(j+1)#k, val#i#(j+1)#k } } ) ) -- technically this is wrong -- the quad isn't flat, we should make triangles
		 }) ++ gParse o
	)
    )
)

beginDocumentation()
multidoc ///
 Node
  Key
   VectorGraphics
  Headline
   A package to produce SVG graphics
  Description
   Text
    {\bf VectorGraphics} is a package to produce SVG 2d and 3d graphics.
    All usable types are descendents of the type GraphicsObject, and are self-initializing.
    Coordinates can be entered as vectors in $\mathbb{R}^2$, $\mathbb{R}^3$ or $\mathbb{R}^4$
    ($\mathbb{R}^4$ is projective coordinates); alternatively, one can enter them as sequences.
    With the default perspective matrix,
    the x axis points to the right, the y axis points up, and the z axis points towards the viewer.
    All types are option tables, i.e., their arguments are options. There are two types of options:
    VectorGraphics options, that are symbols (e.g., {\tt Radius} for circles);
    and styling options, which are CSS style options,
    and which are {\bf strings} (e.g., {\tt "fill"} for fill color).
    {\bf VectorGraphics} does not use units (coordinates are dimensionless).
    In @ TO {Standard} @ mode, the graphical objects are not directly visible; to export them to SVG
    in order to embed them into a web page, use @ TO {html} @. In @ TO {WebApp} @ mode, the graphical objects
    are shown as output.
 Node
  Key
   GraphicsObject
  Headline
   The ancestor class of all VectorGraphics objects
 Node
  Key
   GraphicsPoly
  Headline
   The ancestor class of complex VectorGraphics objects
 Node
  Key
   GraphicsList
  Headline
   A list of VectorGraphics objects
  Description
   Text
    A class that represents a list of @ TO {VectorGraphics} @ objects, displayed together. See also @ TO{gList} @.
 Node
  Key
   Circle
  Headline
   An SVG circle
  Description
   Text
    An SVG circle. The two compulsory options are Center (coordinates of the center) and Radius (radius).
    In 3d, gives a decent approximation of a sphere.
   Example
    Circle{Center=>vector {10,10},Radius=>50,"fill"=>"green","stroke"=>"none"}
    Circle{(10,10),10} -- equivalent syntax for coordinates
 Node
  Key
   Line
  Headline
   An SVG line
  Description
   Text
    A simple SVG line. The two compulsory options are Point1 and Point2, which are vectors (or sequences) describing the two endpoints.
   Example
    Line{Point1=>vector{0,0},Point2=>vector{2,1},"stroke"=>"green"}
    Line{(0,0),(2,1),"stroke-width"=>0.1} -- simplified syntax
 Node
  Key
   Light
  Headline
   A source of light
  Description
   Text
    A source of light for a 3d SVG picture.
    This corresponds to the SVG "specular" lighting, use the property Specular. The location is given by Center.
    By default a Light is invisible (it has opacity 0) and is unaffected by matrix transformations outside it (Static true).
   Example
    Light{Radius=>10,"opacity"=>"1","fill"=>"yellow"}
    v={(74.5571, 52.0137, -41.6631),(27.2634, -29.9211, 91.4409),(-81.3041, 57.8325, 6.71156),(-20.5165, -79.9251, -56.4894)};
    f={{v#2,v#1,v#0},{v#0,v#1,v#3},{v#0,v#3,v#2},{v#1,v#2,v#3}};
    c={"red","green","blue","yellow"};
    tetra=gList(apply(4,i->Polygon{f#i,"fill"=>c#i,"stroke"=>"none"}),
	Light{(110,0,0),Radius=>10,"opacity"=>"1"},ViewPort=>{(-110,-100),(110,100)},
	SizeY=>30,TransformMatrix=>rotation(-1.5,(4,1,0)))
  Caveat
   Do not use the same Light object multiple times in a given @ TO {GraphicsList} @.
 Node
  Key
   Ellipse
  Headline
   An SVG ellipse
  Description
   Text
    An SVG ellipse. The three compulsory options are Center (coordinates of the center) and RadiusX, RadiusY (radii).
   Example
    Ellipse{Center=>vector{10,10},RadiusX=>50,RadiusY=>20,"stroke"=>"none","fill"=>"red"}
    Ellipse{(10,10),50,20,"stroke"=>"blue"} -- equivalent syntax
  Caveat
   Does not really make sense in a 3d context.
 Node
  Key
   Path
  Headline
   An SVG path
  Description
   Text
    An SVG path. It follows the syntax of SVG paths, except successive commands must be grouped together in a list called PathList.
   Example
    Path{PathList => {"M", (0, 25), "Q", (25, 25), (25, 0), "M", (50, 25), "Q", (25, 25), (25, 50)},
	"stroke"=>"black","fill"=>"transparent","stroke-width"=>5}
 Node
  Key
   Polygon
  Headline
   An SVG polygon
  Description
   Text
    An SVG polygon. The coordinates must form a list called Points. (the difference with Polyline is that the last coordinate is reconnected to the first)
   Example
    Polygon{Points=>{(0,10),(100,10),(90,90),(0,80)},"stroke"=>"red","fill"=>"white"}
 Node
  Key
   Polyline
  Headline
   An SVG sequence of lines
  Description
   Text
    An SVG sequence of lines. The coordinates must form a list called Points. (the difference with Polygon is that the last coordinate is not reconnected to the first)
   Example
    Polyline{Points=>{(0,10),(100,10),(90,90),(0,80)},"stroke"=>"red","fill"=>"white"}
 Node
  Key
   GraphicsText
  Headline
   SVG text
  Description
   Text
    Some SVG text. The location of the start of the text is given by the option Point.
    The text itself is the option TextContent (a string).
    The text can be "stroke"d or "fill"ed.
    Font size should be specified with FontSize.
   Example
    GraphicsText{TextContent=>"Test","stroke"=>"red","fill"=>"none","stroke-width"=>0.5}
    gList(GraphicsText{(0,0),"P",FontSize=>14},GraphicsText{(7,0),"AUL",FontSize=>10})
  Caveat
   Currently, cannot be rotated. (coming soon)
 Node
  Key
   gList
  Headline
    Group together VectorGraphics objects
  Description
   Text
    gList(a,b,...,c, options) results in a new @ TO{GraphicsList} @ object containing a,b,...,c
    and the given options.
   Example
    a=gList(Line{(-100, 15, 78), (-9, 100, 4)},
	Line{(-96, -49, -100), (46, -100, 52)},
	Line{(-100, -42, -51), (59, 100, 76)},
	Line{(-100, 66, 54), (83, -100, -27)})
    b=gList(Line{(-30, 100, 20), (9, -100, 8)},
	Line{(-78, -73, -100), (-64, 84, 100)},
	"stroke"=>"red")
    gList(a,b,SizeX=>20)
 Node
  Key
   viewPort
  Headline
    ViewPort of view port
  Description
   Text
    viewPort gives the range of view port occupied by a @ TO {VectorGraphics} @ object, as computed by the package.
    See also @ TO{ViewPort} @.
  Caveat
    At the moment viewPort does not take into account the width of "stroke"s.
 Node
  Key
   is3d
  Headline
   Whether a VectorGraphics object is 3d
  Description
   Text
    Returns a boolean according to whether the @ TO {VectorGraphics} @ object is 2d (false) or 3d (true).
 Node
  Key
   distance
  Headline
   Distance to the viewer
  Description
   Text
    Returns the distance (perpendicularly to the screen) to the viewer of a @ TO {VectorGraphics} @ 3d object,
    normalized so the screen is at distance $1$.
 Node
  Key
   rotation
  Headline
   Computes a rotation matrix
  Usage
   rotation ( angle, axis, center)
   rotation ( angle, center)
  Description
   Text
    Produces a rotation encoded as a 4x4 matrix that can be used as an argument to @TO{TransformMatrix}@ or @TO{AnimMatrix}@.
    For a 3d rotation, use 3d vectors for axis and center.
    For a 2d rotation, use a 2d vector for the center.
    In both cases, the center is optional.
 Node
  Key
   translation
  Headline
   Computes a translation matrix
  Description
   Text
    Produces a translation encoded as a 4x4 matrix that can be used as an argument to @TO{TransformMatrix}@ or @TO{AnimMatrix}@.
    The vector can be 2d or 3d.
   Example
    v={vector{7.456, 5.201, -4.166}, vector{2.7263, -2.992, 9.144},
       vector{-8.130, 5.783, 0.671}, vector {-2.052, -7.993, -5.649}};
    f={{v#2,v#1,v#0},{v#0,v#1,v#3},{v#0,v#3,v#2},{v#1,v#2,v#3}};
    tetra=gList(apply(4,i->Polygon{f#i,"fill"=>"white"}))
    g = memoize(n -> if n==0 then tetra else gList apply(4,i->g(n-1)++{TransformMatrix=>translation(2^(n-1)*v#i)}))
    apply(4,g)
  Usage
   translation ( vector )
 Node
  Key
   OneSided
  Description
   Text
    A property of @ TO{GraphicsPoly} @ 3d objects, means that polygons must be drawn only if they are facing the correct way.
 Node
  Key
   ViewPort
  Headline
   Fix the view port
  Description
   Text
    An option to fix manually the view port range of a @ TO {VectorGraphics} @ object.
    Only has an effect if in the outermost @ TO {VectorGraphics} @ object.
    See also @ TO{viewPort} @ and @ TO{Margin} @.
 Node
  Key
   SizeX
  Headline
   Set the width
  Description
   Text
    An option to fix the width of the @ TO {VectorGraphics} @ object in line width units.
    Only has an effect if in the outermost @ TO {VectorGraphics} @ object.
 Node
  Key
   Perspective
  Headline
   Set the amount of perspective
  Description
   Text
    A 4x4 matrix that is applied to 3d coordinates for perspective.
    After this transformation, the coordinates must be up to normalization $(x,y,z,p)$
    where $(x,y,z>0)$ are coordinates in the reference frame where the observer is at the origin looking in the $z$ direction,
    and $p$ is the distance from the observer to the screen.
    One can instead provide a real number $p$, which is equivalent to placing the screen
    centered at $z=0$ and the viewer at $(0,0,p)$.
    Only has an effect if in the outermost @ TO {VectorGraphics} @ object.
 Node
  Key
   SizeY
  Headline
   Set the height
  Description
   Text
    An option to fix the height of the @ TO {VectorGraphics} @ object in line width units.
    Only has an effect if in the outermost @ TO {VectorGraphics} @ object.
 Node
  Key
   AnimMatrix
  Headline
   Create a rotation animation matrix
  Description
   Text
    An option to create a rotation animation for the @ TO {VectorGraphics} @ 3d object.
    The value can be a single 4x4 matrix, or a list which is cycled.
    The syntax {\tt n => ...} can be used to repeat a sequence n times (where {\tt 0} means infinity).
    The animation automatically loops (use {\tt 0 => \{ \}} to stop!)
    In order for the animation to work, {\tt VectorGraphics.css} and {\tt VectorGraphics.js} must be included in the web page.
   Example
    (anim1=rotation(0.1,(0,0,1),(0,0,0)); anim2=rotation(-0.1,(0,0,1),(0,0,0)); anim3 = { 5 => {5 => anim1, 5 => anim2}, 10 => anim1 });
    gList(Polygon{{(-1,0),(1,0.1),(1,-0.1)},"fill"=>"red",AnimMatrix=>anim1},Circle{(1,0),0.1},Circle{(0,0),1})
    gList(Polygon{{(-1,0),(1,0.1),(1,-0.1)},"fill"=>"red",AnimMatrix=>anim3},Circle{(1,0),0.1},Circle{(0,0),1})
 Node
  Key
   TransformMatrix
  Headline
   Create a rotation matrix
  Description
   Text
    An option to rotate the coordinates of the @ TO {VectorGraphics} @ 3d object.
    Must be a 4x4 matrix (projective coordinates).
   Example
    a=Polygon{{(-1,0),(1,0.1),(1,-0.1)},"fill"=>"red"}
    gList(a,a++{TransformMatrix=>rotation(2*pi/3)})
 Node
  Key
   Blur
  Headline
   An option to blur a VectorGraphics object
  Description
   Text
    This corresponds to the feGaussianBlur SVG filter.
    The value is the amount of blurriness relative to the size of the object.
  Caveat
    In animated 3d, the amount of blurriness does not vary as the size varies.
 Node
  Key
   Static
  Headline
   An option to make a VectorGraphics object unmoving
  Description
   Text
    The @ TO {VectorGraphics} @ 3d object is unaffected by matrix transformations of its ancestors.
 Node
  Key
   linearGradient
  Headline
   An SVG gradient
  Description
   Text
    This corresponds to the linearGradient SVG gradient.
    The argument is a list of pairs of offsets and styles.
    Optional arguments (e.g., "x1", "y1", "x2", "y2") are used to determine the orientation of the gradient.
   Example
    Ellipse{(60,60),40,30, "fill"=>linearGradient{("0%","stop-color:red"),("100%","stop-color:yellow")}}
 Node
  Key
   radialGradient
  Headline
   An SVG gradient
  Description
   Text
    This corresponds to the radialGradient SVG gradient.
    The argument is a list of pairs of offsets and styles.
    Optional arguments (e.g., "cx", "cy", "r", "fx", "fy") are used to position the gradient.
   Example
    Ellipse{(60,60),40,30, "fill"=>radialGradient{("0%","stop-color:red"),("100%","stop-color:yellow")}}
 Node
  Key
   plot
  Headline
   Draws a curve or surface
  Description
   Text
    Draws a curve or surface defined implicitly or explicitly by a polynomial.
    The first argument is a polynomial, the second is a (list of) range(s) of variable(s).
    If the number of ranges is equal to the number of variables of the polynomial, the graph of the polynomial
    is drawn. If it is one fewer, then the zero set of the polynomial is drawn.
    The option Mesh specifies the number of sampled values of the variables.
   Example
    R=RR[x,y];
    P=y^2-(x+1)*(x-1)*(x-2);
    plot(P,{-2,3},"stroke-width"=>0.05,SizeY=>25,"stroke"=>"red")
 Node
  Key
   Axes
  Headline
   An option to draw axes
 Node
  Key
   Margin
  Headline
   An option to specify the margin
  Description
   Text
    The margin is proportional to the size of the image.
    It increases the view port beyond the value returned by @ TO{viewPort} @ or set by @ TO{ViewPort} @.
   Example
    Circle{"fill"=>"red","stroke"=>"none",Margin=>0}
    Circle{"fill"=>"red","stroke"=>"none",Margin=>0.5}
 Node
  Key
   arrow
  Headline
   A marker to add to paths
  Description
   Text
    Must be used as styling options "marker-start", "marker-mid" or "marker-end", to add an arrow to a path.
   Example
    Polyline{Points=>{(0,0),(50,50),(0,100),(50,150)},"stroke"=>"yellow","stroke-width"=>5,"marker-end"=>arrow("fill"=>"orange"),Margin=>0.3}
 Node
  Key
   GraphicsHtml
  Headline
   Html content inside a VectorGraphics object
  Description
   Text
    Some arbitrary HTML content, specified by the option HtmlContent (a @ TO{Hypertext} @ object or other content to render in HTML).
  Caveat
   Due to a limitation of <foreignObject>, coordinates are rounded to the nearest integer. So use large enough coordinate systems.
 Node
  Key
   SVG
  Headline
   hypertext SVG item
 Node
  Key
   GraphicsType
  Headline
   A particular type of type used by VectorGraphics, similar to @ TO{SelfInitializingType}.
///
undocumented { -- there's an annoying conflict with NAG for Point, Points
    Contents, TextContent, HtmlContent, SVGElement, Point, Points, Specular, Radius, Point1, Point2, PathList, Mesh, FontSize, RadiusX, RadiusY,
    (symbol ++, GraphicsObject, List), (symbol ?,GraphicsObject,GraphicsObject), (symbol SPACE,GraphicsType,List),
    (expression, GraphicsObject), (html,GraphicsObject), (net,GraphicsObject), (toString,GraphicsObject),
    (NewFromMethod,GraphicsObject,List), (NewFromMethod,GraphicsObject,OptionTable), (NewOfFromMethod,GraphicsType,GraphicsObject,VisibleList), (NewFromMethod,SVG,GraphicsObject),
}

end--

-- ex of use
gr=linearGradient{("0%","stop-color:red"),("100%","stop-color:yellow")};
gList(Ellipse{(0,0),90,30,"stroke"=>"none","fill"=>gr,Blur=>0.3},GraphicsText{(-65,-7),"Macaulay2",FontSize=>25,"stroke"=>"black","fill"=>"white"},SizeY=>12)

a=Circle{"fill"=>"yellow","stroke"=>"green",SizeX=>1,SizeY=>1}
b=Line{(10,10),(20,50),"stroke"=>"black"}
c=Circle{(50,50),50,"fill"=>"blue","fill-opacity"=>0.25}
d=Ellipse{(60,60),40,30, "fill"=>"blue", "stroke"=>"grey"}
e=Polyline{{(0,0),(100,100),(100,50)},"fill"=>"pink","stroke"=>"green"}
f=Polygon{{(0,10),(100,10),(90,90),(0,80)},"stroke"=>"red","fill"=>"white"}
gList (f,a,b,c,d,e)
-- or
rgb={"red","green","blue"};
scan(rgb, x -> (value x <- Circle{"fill"=>x,"stroke"=>"black",SizeX=>0.8,SizeY=>0.8,Margin=>0}))
value\rgb
R=QQ[x_red,x_green,x_blue]
describe R
x_red^2-x_green^2
factor oo

-- or
z=Polygon{{(0,0),(0,50),(50,50),(50,0)},"fill"=>"white"}
b1=Path{{"M", (0, 25), "Q", (25, 25), (25, 0), "M", (50, 25), "Q", (25, 25), (25, 50)},"stroke"=>"black","fill"=>"transparent","stroke-width"=>5}
b2=Path{{"M", (0, 25), "Q", (25, 25), (25, 0), "M", (50, 25), "Q", (25, 25), (25, 50)},"stroke"=>"red","fill"=>"transparent","stroke-width"=>4}
b=gList(z,b1,b2,SizeX=>2,SizeY=>2,Margin=>0)
a1=Path{{"M", (50, 25), "Q", (25, 25), (25, 0), "M", (0, 25), "Q", (25, 25), (25, 50)},"stroke"=>"black","fill"=>"transparent","stroke-width"=>5}
a2=Path{{"M", (50, 25), "Q", (25, 25), (25, 0), "M", (0, 25), "Q", (25, 25), (25, 50)},"stroke"=>"red","fill"=>"transparent","stroke-width"=>4}
a=gList(z,a1,a2,SizeX=>2,SizeY=>2,Margin=>0)
--ab=a|b
--ba=b|a
--ab||ba||ba
tile = (I,i,j)->(if m_(i+1,j+1)%I == 0 then if c_(i+1,j+1)%I==0 then () else a else b);
tiledRow = (I,i)->new RowExpression from apply(n,j->tile(I,i,j));
loopConfig = I->new ColumnExpression from apply(k,i->tiledRow(I,i)); -- no such a thing as ColumnExpression. there should


-- or
barside1=Path{{"M",(80,60,100),"L",(80,55,100),"L",(220,55,100),"L",(220,60,100),"Z"},"fill"=>"#222","stroke-width"=>0}; -- stroke-width shouldn't be necessary
triangle1=Path{{"M",(-50,160,2),"L",(0,80,2),"L",(50,160,2),"Z"},"fill"=>"#2040d0","stroke"=>"#80c0ff","stroke-width"=>1,"stroke-miterlimit"=>0};
triangle2=Path{{"M",(30,160,98),"L",(80,80,98),"L",(130,160,98),"Z"},"fill"=>"#2040d0","stroke"=>"#80c0ff","stroke-width"=>1,"stroke-miterlimit"=>0};
edge1=Path{{"M",(30,160,98),"L",(30,160,102),"L",(80,80,102),"L",(80,80,98),"Z"},"fill"=>"#4080e0","stroke-width"=>1};
edge2=Path{{"M",(130,160,98),"L",(130,160,102),"L",(80,80,102),"L",(80,80,98),"Z"},"fill"=>"#4080e0","stroke-width"=>1};
bartop=Path{{"M",(80,55,98),"L",(80,55,102),"L",(220,55,102),"L",(220,55,98),"Z"},"fill"=>"#aaa","stroke-width"=>0}; -- stroke-width shouldn't be necessary
thread=Path{{"M",(80,55,100),"L",(80,80,100),"Z"},"stroke"=>"#111","stroke-width"=>0.5,"stroke-opacity"=>0.8};
gList{barside1,triangle1,triangle2,edge1,edge2,bartop,thread}

-- tetrahedron
v={(74.5571, 52.0137, -41.6631),(27.2634, -29.9211, 91.4409),(-81.3041, 57.8325, 6.71156),(-20.5165, -79.9251, -56.4894)};
c={"red","green","blue","yellow"};
vv={{v#2,v#1,v#0},{v#0,v#1,v#3},{v#0,v#3,v#2},{v#1,v#2,v#3}};
triangles=apply(4,i->Path{{"M",vv#i#0,"L",vv#i#1,"L",vv#i#2,"Z"},"fill"=>c#i,OneSided=>true});
gList(triangles,Light{(100,0,0),Radius=>10},ViewPort=>{(-100,-150),(150,150)},SizeY=>30,TransformMatrix=>rotation(-1.5,(0,1,0)))

-- dodecahedron
vertices={vector{-137.638,0.,26.2866},vector{137.638,0.,-26.2866},vector{-42.5325,-130.902,26.2866},vector{-42.5325,130.902,26.2866},vector{111.352,-80.9017,26.2866},vector{111.352,80.9017,26.2866},vector{-26.2866,-80.9017,111.352},vector{-26.2866,80.9017,111.352},vector{-68.8191,-50.,-111.352},vector{-68.8191,50.,-111.352},vector{68.8191,-50.,111.352},vector{68.8191,50.,111.352},vector{85.0651,0.,-111.352},vector{-111.352,-80.9017,-26.2866},vector{-111.352,80.9017,-26.2866},vector{-85.0651,0.,111.352},vector{26.2866,-80.9017,-111.352},vector{26.2866,80.9017,-111.352},vector{42.5325,-130.902,-26.2866},vector{42.5325,130.902,-26.2866}};
faces={{14,9,8,13,0},{1,5,11,10,4},{4,10,6,2,18},{10,11,7,15,6},{11,5,19,3,7},{5,1,12,17,19},{1,4,18,16,12},{3,19,17,9,14},{17,12,16,8,9},{16,18,2,13,8},{2,6,15,0,13},{15,7,3,14,0}};
centers=apply(faces,f->1/5*sum(f,i->vertices#i));
steps=30;
dodeca=apply(faces,centers,(f,c)->Polygon{apply(f,j->vertices#j),"fill"=>concatenate("rgb(",toString(134+round(1.2*c_0)),",",toString(134+round(1.2*c_1)),",",toString(134+round(1.2*c_2)),")")});
label=apply(#vertices,i->GraphicsText{vertices#i,toString i});
dodecasplit=apply(faces,centers,(f,c)->Polygon{apply(f,j->vertices#j),
	AnimMatrix=>apply(steps,j->rotation(2*pi/5/steps*4*min(j/steps,1-j/steps),c,c)*translation(0.075*sin(2*pi*j/steps)*c)),
	"fill"=>concatenate("rgb(",toString(134+round(1.2*c_0)),",",toString(134+round(1.2*c_1)),",",toString(134+round(1.2*c_2)),")")});
d=gList(dodecasplit,"fill-opacity"=>0.65,AnimMatrix=>rotation(0.02,(1,2,3)));
d1=gList(d,TransformMatrix=>translation(200,0,0)); -- using alternate syntax of Sequence instead of Vector
d2=gList(d,TransformMatrix=>translation(-200,0,0));
gList(d1,d2,ViewPort=>{vector{-400,-400},vector{400,400}},SizeY=>25,"stroke-width"=>2)

p=random splice{0..11};


-- icosahedron
vertices={vector{0.,0.,-95.1057},vector{0.,0.,95.1057},vector{-85.0651,0.,-42.5325},vector{85.0651,0.,42.5325},vector{68.8191,-50.,-42.5325},vector{68.8191,50.,-42.5325},vector{-68.8191,-50.,42.5325},vector{-68.8191,50.,42.5325},vector{-26.2866,-80.9017,-42.5325},vector{-26.2866,80.9017,-42.5325},vector{26.2866,-80.9017,42.5325},vector{26.2866,80.9017,42.5325}};
faces={{1,11,7},{1,7,6},{1,6,10},{1,10,3},{1,3,11},{4,8,0},{5,4,0},{9,5,0},{2,9,0},{8,2,0},{11,9,7},{7,2,6},{6,8,10},{10,4,3},{3,5,11},{4,10,8},{5,3,4},{9,11,5},{2,7,9},{8,6,2}};
icosa=apply(faces,f->Polygon{apply(f,j->vertices#j),"fill"=>"gray","stroke"=>"none"});
i=gList(icosa,TransformMatrix=>matrix{{0.7,0,0,0},{0,0.7,0,0},{0,0,0.7,0},{0,0,0,1}})

rnd = () -> random(-1.,1.); cols={"red","green","blue","yellow","magenta","cyan"};
gList(i, apply(cols, c -> Light{100*vector{1.5+rnd(),rnd(),rnd()},Radius=>10,"opacity"=>1,"fill"=>c,Specular=>20,AnimMatrix=>rotation(0.02,(rnd(),rnd(),rnd()))}),ViewPort=>{(-200,-200),(200,200)},SizeY=>30)

subdivide = (v,f) -> (
    u := v#0;
    c := u_0*u_0+u_1*u_1+u_2*u_2;
    e := unique flatten apply(f,x->{sort{x#0,x#1},sort{x#0,x#2},sort{x#1,x#2}});
    mid := apply(e, x -> (u=0.5*(v#(x#0)+v#(x#1)); r:=sqrt(c/(u_0*u_0+u_1*u_1+u_2*u_2)); r*u));
    ff := flatten apply(f, x -> (
	    i:=#v+position(e,y->y==sort{x#0,x#1});
	    j:=#v+position(e,y->y==sort{x#0,x#2});
	    k:=#v+position(e,y->y==sort{x#1,x#2});
	    {{x#0,i,j},{x#1,i,k},{x#2,j,k},{i,j,k}}
	    ));
    (v|mid,ff)
    )
(v2,f2)=subdivide(vertices,faces);
(v3,f3)=subdivide(v2,f2);
sph=apply(f3,f->Polygon{apply(f,j->v3#j),"stroke"=>"white","stroke-width"=>0.01,"fill"=>"gray"});
gList(sph, apply(cols, c -> Light{100*vector{1.5+rnd(),rnd(),rnd()},Radius=>10,"fill"=>c,Specular=>10,AnimMatrix=>rotation(0.02,(rnd(),rnd(),rnd()))}),ViewPort=>{(-200,-200),(200,200)},SizeY=>30)

-- simple plot
R=RR[x,y]; P=0.1*(x^2-y^2);
gList(plot(P,{{-10,10},{-10,10}},Mesh=>15,"stroke-width"=>0.05,"fill"=>"gray"),Light{(200,0,-500),Specular=>10,"fill"=>"rgb(180,0,100)"},Light{(-200,100,-500),Specular=>10,"fill"=>"rgb(0,180,100)"},SizeY=>40,Axes=>false)

-- implicit plot
R=RR[x,y];
P=y^2-(x+1)*(x-1)*(x-2);
plot(P,{-2,3},"stroke-width"=>0.05,SizeY=>25,"stroke"=>"red")

-- Schubert calculus
a=gList(Line{(-100, 15, 78), (-9, 100, 4)},Line{(-96, -49, -100), (46, -100, 52)},Line{(-100, -42, -51), (59, 100, 76)},Line{(-100, 66, 54), (83, -100, -27)})
b=gList(Line{(-30, 100, 20), (9, -100, 8)},Line{(-78, -73, -100), (-64, 84, 100)},"stroke"=>"red")
c=gList(Polygon{{(-100,100,100),(-100,-100,100),(-100,-100,-100),(-100,100,-100)}},
		  Polygon{{(100,100,100),(100,-100,100),(100,-100,-100),(100,100,-100)}},
		  Polygon{{(100,-100,100),(-100,-100,100),(-100,-100,-100),(100,-100,-100)}},
		  Polygon{{(100,100,100),(-100,100,100),(-100,100,-100),(100,100,-100)}},
		  Polygon{{(100,100,-100),(100,-100,-100),(-100,-100,-100),(-100,100,-100)}},
		  Polygon{{(100,100,100),(100,-100,100),(-100,-100,100),(-100,100,100)}},
		  "stroke"=>"black","fill"=>"grey", "opacity"=>"0.25")
gList(a,b,c,SizeX=>20)

--
n=10;
v1=apply(n,i->vector {cos(2*pi*i/n),sin(2*pi*i/n),0.1});
v2=apply(n,i->vector {cos(2*pi*i/n),sin(2*pi*i/n),-0.1});
l=apply(n,i->Polygon{{v1#i,v2#i,v2#((i+1)%n),v1#((i+1)%n)},"fill"=>"hsl("|toString(360.*i/n)|",100%,50%)"});
a=gList(l,AnimMatrix=>rotation(0.2,vector{0,0,1}));
m=50;r=apply(m,i->rotation(0.05,vector{cos(2*pi*i/m),sin(2*pi*i/m),0}));
b=gList(a,AnimMatrix=>r)
--c=gList(a,TransformMatrix=>(map(RR^3,RR^3,0.5)++1)*rotation(2,vector{1,2,3}),AnimMatrix=>r)
--gList(b,c)

-- stars
n=100;
speed=100;
far=-10000;
screen=1000;
stars=apply(n,i->(
z=speed*(random(far,screen)//speed);
Circle{(random(-200,200),random(-200,200),z),10,"fill"=>"yellow","stroke"=>"none",Blur=>0.3, -- TODO: make blurriness dynamically depend on size
AnimMatrix=>{((screen-z)//speed)=>translation (0,0,speed),translation (0,0,far-screen),((-far+z)//speed)=>translation (0,0,speed)}}
));
gList(stars,ViewPort=>{(-100,-100),(100,100)})
style(SVG oo,"background"=>"black")

-- removed (might be added back if correctly implemented in 3d)
 Node
  Key
   Rectangle
  Headline
   An SVG rectangle
  Description
   Text
    An SVG rectangle. The SW coordinate is given as Point, the difference between NE and SW corners is given as GraphicsSize.
   Example
    Rectangle{(10,10),(20,50),"fill"=>"pink","stroke"=>"black"} -- first argument is Point, second GraphicsSize
  Caveat
   Rectangle can only be used in 2d. Use Polygon for 3d.

