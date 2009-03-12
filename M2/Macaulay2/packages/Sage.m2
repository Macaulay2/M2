newPackage "Sage"
loadPackage "Python"
export { "sage", "plot", "plot3d" }
runSimpleString "from sage.all import *"
sage = s -> (
     if debugLevel > 0 then stderr << "sage command: " << s << endl;
     runSimpleString s
     )
x := local x
y := local y
sageRing := QQ[x,y]
sage "x,y=var('x,y')"
fix := s -> (
     s = replace("\\^","**",s);
     s = replace("/[0-9]+","\\0.",s);
     s)
plot = method()
plot String := s -> sage concatenate ("show(plot(",fix s,"))")
plot RingElement := f -> (
     R := ring f;
     if not instance(R,PolynomialRing) then error "expected an element of a polynomial ring";
     if not numgens R == 1 then error "expected an element of a polynomial ring in one variable";
     p := map(sageRing,R,{x});
     plot toString p f)
plot3d = method()
plot3d String := s -> sage concatenate ("show(plot3d(",fix s,", (x,-4,4),(y,-4,4)))")
plot3d RingElement := f -> (
     R := ring f;
     if not instance(R,PolynomialRing) then error "expected an element of a polynomial ring";
     if not numgens R == 2 then error "expected an element of a polynomial ring in one variable";
     p := map(sageRing,R,{x,y});
     plot3d replace("\\^","**",toString p f))
