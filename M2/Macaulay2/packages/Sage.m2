newPackage "Sage"
loadPackage "Python"
export { "sage", "plot", "plot3d", "implicitPlot" }
python := s -> (
     if debugLevel > 1 then stderr << "python command: " << s << endl;
     runSimpleString s)
python "from sage.all import *"
x := local x
y := local y
sageRing := QQ[x,y]
python "x,y=var('x,y')"
sage = s -> (
     if debugLevel > 0 then stderr << "sage command: " << s << endl;
     python concatenate("print(eval(preparse(", format s, ")))")
     )
toSageRing := (f,n) -> (
     R := ring f;
     if not instance(R,PolynomialRing) then error "expected an element of a polynomial ring";
     if not numgens R == n 
     then error("expected an element of a polynomial ring in ",toString n," variable(s)");
     p := map(sageRing,R,take({x,y},n));
     p f)
plot = method()
plot String := s -> sage concatenate ("show(plot(",s,"))")
plot RingElement := f -> plot toString toSageRing(f,1)
plot3d = method()
plot3d String := s -> sage concatenate ("show(plot3d(",s,", (x,-4,4),(y,-4,4)))")
plot3d RingElement := f -> (
     R := ring f;
     if not instance(R,PolynomialRing) then error "expected an element of a polynomial ring";
     if not numgens R == 2 then error "expected an element of a polynomial ring in one variable";
     p := map(sageRing,R,{x,y});
     plot3d replace("\\^","**",toString p f))
implicitPlot = method()
implicitPlot String := s -> sage concatenate(
     "show(implicit_plot(",s,",plot_points=100))"
     )
implicitPlot RingElement := f -> implicitPlot concatenate(
     toString toSageRing(f,2),
     ",(x,-1.5,3),(y,-5,5)"
     )
