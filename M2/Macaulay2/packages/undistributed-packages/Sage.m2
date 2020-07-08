needsPackage "Python"
newPackage "Sage"
needsPackage "Python"
export { "sage", "plot", "plot3d", "implicitPlot" }
sage = context("from sage.all import *", Preprocessor => "preparse");
sage.stmt "x,y=var('x,y')"
x := local x
y := local y
sageRing := QQ[x,y]
toSageRing := (f,n) -> (
     R := ring f;
     if not instance(R,PolynomialRing) then error "expected an element of a polynomial ring";
     if not numgens R == n 
     then error("expected an element of a polynomial ring in ",toString n," variable(s)");
     p := map(sageRing,R,take({x,y},n));
     p f)
plot = method()
plot String := s -> sage.stmt concatenate ("plot(",s,")")
plot RingElement := f -> plot toString toSageRing(f,1)
plot3d = method()
plot3d String := s -> sage.stmt concatenate ("plot3d(",s,", (x,-4,4),(y,-4,4))")
plot3d RingElement := f -> plot3d toString toSageRing(f,2)
implicitPlot = method()
implicitPlot String := s -> sage.stmt concatenate( "implicit_plot(",s,",plot_points=100)" )
implicitPlot RingElement := f -> implicitPlot concatenate( toString toSageRing(f,2), ",(x,-1.5,3),(y,-5,5)" )
