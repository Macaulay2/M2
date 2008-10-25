k = RR -- change RR to CC later after the bug is fixed
common := (
     symbol s,
     symbol A,
     symbol cd,
     symbol mol,
     symbol K,
     symbol bit,
     symbol wholenote,
     symbol US$
     )
CGS = k[symbol cm,symbol g,common, Inverses=>true, MonomialOrder => Lex]
MKS = k[symbol m,symbol kg,common, Inverses=>true, MonomialOrder => Lex]
MKS Number := Number MKS := times
CGS Number := Number CGS := times

MKS/MKS := CGS/CGS := (x,y) -> (
     if x%y != 0 then error "division not possible";
     x//y)

units := identity

CGS.use = CGS -> (
     m = 100 cm;
     kg = 1000 g;
     units();
     )

MKS.use = MKS -> (
     cm = .01 m;
     g = .001 kg;
     units();
     )

use CGS ; toCGS = map(CGS,MKS,{m,kg}|drop(gens CGS,2))
use MKS ; toMKS = map(MKS,CGS,{cm,g}|drop(gens MKS,2))

units = x -> (
     second = s;
     minute = 60 s;
     hour = 60 minute;
     day = 24 hour;
     week = 7 day;
     year = tropicalyear = 365.242198781 day;
     meter = m;
     gram  = .001 kg;
     exa   = 1e18;
     peta  = 1e15;
     tera  = 1e12;
     giga  = 1e9;
     mega  = 1000000;
     kilo  = 1000;
     hecto = 100;
     deka  = 10;
     deci  = .1;
     centi = .01;
     milli = .001;
     micro = .000001;
     nano  = 1e-9;
     pico  = 1e-12;
     femto = 1e-15;
     atto  = 1e-18;
     inch = 2.54 centi meter;
     foot = 12 inch;
     yard = 3 foot;
     mile = 5280 foot;
     marathon = 26 mile + 385 yard;
     )

use MKS

{*
marathon//feet						    -- oops
*}
