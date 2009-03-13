newPackage "Sage"
needsPackage "Python"
export { "sage", "plot", "plot3d", "implicitPlot" }
python0 = s -> (
     if debugLevel > 1 then stderr << "python command: " << s << endl;
     runSimpleString s)
python0 "dict = {}"
python1 = s -> python0 concatenate(
     "eval(compile(", s, ",'dummyfile','single'),dict)")
python = s -> python1 format s
python "from sage.all import *"
python "x,y=var('x,y')"
x := local x
y := local y
sageRing := QQ[x,y]
sage = s -> (
     python concatenate("s = preparse(",format s,")");
     python1 "dict['s']";
     )
toSageRing := (f,n) -> (
     R := ring f;
     if not instance(R,PolynomialRing) then error "expected an element of a polynomial ring";
     if not numgens R == n 
     then error("expected an element of a polynomial ring in ",toString n," variable(s)");
     p := map(sageRing,R,take({x,y},n));
     p f)
plot = method()
plot String := s -> sage concatenate ("plot(",s,")")
plot RingElement := f -> plot toString toSageRing(f,1)
plot3d = method()
plot3d String := s -> sage concatenate ("plot3d(",s,", (x,-4,4),(y,-4,4))")
plot3d RingElement := f -> plot3d toString toSageRing(f,2)
implicitPlot = method()
implicitPlot String := s -> sage concatenate( "implicit_plot(",s,",plot_points=100)" )
implicitPlot RingElement := f -> implicitPlot concatenate( toString toSageRing(f,2), ",(x,-1.5,3),(y,-5,5)" )
