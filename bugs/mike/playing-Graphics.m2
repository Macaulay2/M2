-- Looking at the Graphics package.

restart
loadPackage "Graphics"

-- viewPicture
polygon
svgPicture(mypict, 300, 300, filename)

p = point(43, 56)
q = point(70, 90)
P = segment(p,q)

G = formatGraphicPrimitives({P}, new HashTable)
svgPicture(picture {G}, 300, 300, "foo1.svg")
get ///!open "foo1.svg"///

hasseGraphToPicture = method(Options=>true)
hasseGraphToPicture(HasseGraph) := {
    "top margin"=>100,
    "left margin"=>100,
    "horizontal space"=>100,
    "row space"=>100,
    "tag distance"=>20,
    "point radius"=>2,
    "point options"=>new HashTable from {"fill"=>"black"}, 
    "edge options" => new HashTable from {"stroke-width"=>"2"}, 
    "tag options"=>new HashTable from {}
    } >> opts -> (G) ->	(
	topmargin:=opts#"top margin";
	leftmargin:=opts#"left margin";
	interpoint:= opts#"horizontal space";
	interrow:= opts#"row space";
	pointtagdist:=opts#"tag distance";
	--edgetagdist:=20;
	pointrad:= opts#"point radius";
	--linewidth:= opts#"edge width";
	rowsize:=toList(apply(G,x->#x));
	maxpoints:=max(rowsize);
	optionspoints:= opts#"point options"; 
	optionstags:= opts#"tag options"; 
	optionslinks:= opts#"edge options";
	mypoints := flatten for i from 0 to #G-1 list
	  (
	  flatten for j from 0 to #(G#i)-1 list
	    {formatGraphicPrimitives({circle(point(round(0.0+leftmargin+((maxpoints-rowsize#i)/2 +j)*interpoint),topmargin+i*interrow),pointrad)}, optionspoints),
	    formatGraphicPrimitives({textTag(point(round(0.0+leftmargin+((maxpoints-rowsize#i)/2 +j)*interpoint),topmargin+i*interrow),G#i#j#0)}, optionstags)}
	  ); 
	mylinks := flatten for i from 0 to #G-1 list
	  flatten for j from 0 to #(G#i)-1 list
	    flatten for k from 0 to #(G#i#j#1)-1 list
	      formatGraphicPrimitives(
              {segment(
                      point(round(0.0+leftmargin+((maxpoints-rowsize#i)/2 +j)*interpoint),topmargin+i*interrow),
                      point(round(0.0+leftmargin+((maxpoints-rowsize#(i+1))/2 +(G#i#j#1#k#1))*interpoint),topmargin+(i+1)*interrow))
                  },
              optionslinks);
	picture(mylinks|mypoints)
	)
