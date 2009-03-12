newPackage "Sage"
loadPackage "Python"
export { "sage", "plot", "plot3d" }
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
plot = method()
plot String := s -> sage concatenate ("show(plot(",s,"))")
plot RingElement := f -> (
     R := ring f;
     if not instance(R,PolynomialRing) then error "expected an element of a polynomial ring";
     if not numgens R == 1 then error "expected an element of a polynomial ring in one variable";
     p := map(sageRing,R,{x});
     plot toString p f)
plot3d = method()
plot3d String := s -> sage concatenate ("show(plot3d(",s,", (x,-4,4),(y,-4,4)))")
plot3d RingElement := f -> (
     R := ring f;
     if not instance(R,PolynomialRing) then error "expected an element of a polynomial ring";
     if not numgens R == 2 then error "expected an element of a polynomial ring in one variable";
     p := map(sageRing,R,{x,y});
     plot3d replace("\\^","**",toString p f))
