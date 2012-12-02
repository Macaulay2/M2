newPackage("Units",
    	Headline => "units conversion and physical constants",
    	Version => "0.9", 				    -- needs to be documented
    	Date => "May 5, 2011",
    	Authors => {
	     {Name => "Dan Grayson", Email => "dan@math.uiuc.edu", HomePage => "http://dangrayson.com/"}
	     }
    	)
fundamentalUnits = { symbol s, symbol g, symbol m, symbol A, symbol cd, symbol mol, symbol K }
UnitMonomial = new Type of HashTable
scan(fundamentalUnits, u -> (
	  u <- new UnitMonomial from { u => 1 };
	  protect u;
	  ))
UnitMonomial * UnitMonomial := UnitMonomial UnitMonomial := (m,n) -> (
     p := select(merge(m,n,plus), i -> i =!= 0);
     if #p === 0 then 1 else p)
UnitMonomial ^ ZZ := (m,i) -> if i === 0 then 1 else applyValues(m, j -> i*j)
UnitMonomial / UnitMonomial := (m,n) -> m * n^-1
expression UnitMonomial := m -> if #m === 0 then expression 1 else product(sort pairs m, (u,i) -> (hold u)^i)
net UnitMonomial := net @@ expression

Measurement = new Type of BasicList			    -- { number, unit monomial }
expression Measurement := m -> expression m#0 * expression m#1
net Measurement := net @@ expression
Number * UnitMonomial := Constant * UnitMonomial := Number UnitMonomial := Constant UnitMonomial := (x,m) -> new Measurement from { x, m }
Number / UnitMonomial := Constant / UnitMonomial := (x,m) -> new Measurement from { x, m^-1 }
UnitMonomial * Number := UnitMonomial * Constant := UnitMonomial Number := UnitMonomial Constant := (m,x) -> x*m
UnitMonomial / Number := UnitMonomial / Constant := (m,x) -> new Measurement from { x^-1, m }
Number * Measurement  := Constant * Measurement  := Number Measurement  := Constant Measurement  := (x,m) -> new Measurement from { x * m#0, m#1 }
Number / Measurement  := Constant / Measurement  := (x,m) -> new Measurement from { x / m#0, m#1^-1 }
Measurement * Number  := Measurement * Constant  := Measurement Number  := Measurement Constant  := (m,x) -> x*m
Measurement / Number  := Measurement / Constant  := (m,x) -> new Measurement from { m#0/x, m#1 }

Measurement * UnitMonomial := Measurement UnitMonomial := (m,n) -> (
     p := m#1*n;
     if p === 1 then m#0 else new Measurement from {m#0,p})
UnitMonomial * Measurement := UnitMonomial Measurement := (n,m) -> (
     p := n*m#1;
     if p === 1 then m#0 else new Measurement from {m#0,p})
UnitMonomial / Measurement := Measurement / UnitMonomial := (m,n) -> m * n^-1

Measurement * Measurement := Measurement Measurement := (m,n) -> (
     x := m#0 * n#0;
     p := m#1 * n#1;
     if p === 1 then x else new Measurement from {x,p})
Measurement / Measurement := (m,n) -> m * n^-1

Measurement ^ ZZ := (m,i) -> if i === 0 then 1 else new Measurement from {m#0^i,m#1^i}

- Measurement := (n) -> new Measurement from { - n#0 , n#1 }
Measurement + Measurement := (m,n) -> ( if m#1 =!= n#1 then error "sum: incompatible measurements"; new Measurement from { m#0 + n#0 , m#1 } )
Measurement - Measurement := (m,n) -> m + -n

Constant Number := Number Constant := Number Number := times

kg = 1000 g
cm = m / 100

exa = 10^18
peta = 10^15
tera = 10^12
giga = 10^9
mega = 1000000
kilo = 1000
hecto = 100
deka = 10
deci = 10^-1
centi = 10^-2
milli = 10^-3
micro = 10^-6
nano = 10^-9
pico = 10^-12
femto = 10^-15
atto = 10^-18
hundred = 100
thousand = 1000
million = 10^6
billion = 10^9
trillion = 10^12
quadrillion = 10^15
quintillion = 10^18
sextillion = 10^21
septillion = 10^24
octillion = 10^27
noventillion = nonillion = 10^30
decillion = 10^33
undecillion = 10^36
duodecillion = 10^39
tredecillion = 10^42
quattuordecillion = 10^45
quindecillion = 10^48
sexdecillion = 10^51
septendecillion = 10^54
octodecillion = 10^57
novemdecillion = 10^60
vigintillion = 10^63
centillion = 10^303
googol = 10^100

kelvin = K
mole = mol
amp = ampere = A
second = s
minute = 60 s
hour = 60 minute
day = 24 hour
wk = week = 7 day
fortnight = 14 day
yr = year = tropicalyear = 365.242198781 day
mo = month = yr/12
decade = 10 yr
century = 100 yr
millennia = millennium = 1000 year
meter = m
gm = gram = g
t = tonne = 1000 kg
inch = 254/100 * cm
ft = foot = 12 inch
chain = 66 ft
yd = yard = 3 foot
rod = (5+1/2) yard
furlong = 40 rod
mile = 5280 foot
acre = mile^2/640
league = 3 mile
marathon = 26 mile + 385 yard
N = newton = kg m / s^2
mm = milli m
Pa = pascal = N/m^2
bar = 10^5 Pa
micron = micro meter
cc = cm^3
are = 100 m^2
hectare = hecto are
l = liter = 1000 cc
J = joule = m N
btu = 1055.06 J
W = watt = J/s
C = coulomb = A s
V = volt = W/A
ohm = V/A
mho = 1/ohm
angstrom = 10^-10 m
fermi = 10^-15 m
F = farad = C/V
Wb = weber = V s
H = henry = Wb/A
T = tesla = Wb/m^2
Hz = hertz = 1/s
barn = 10^-28 m^2
diopter = 1/m
radian = m/m
circle = pi radian 2
sr = steradian = m^2/m^2
arcdeg = deg = {* degree = *} 1/360 * circle
arcmin = arcdeg/60
arcsec = arcmin/60
degC = degcelsius = K
degF = degfahrenheit = 5/9 * degC
light = c = 299792458 m/s
au = astronomicalunit = 499.004783806 light second
solarmass = 1.9891e30 kg
mu0 = 4 pi 10^-7 H/m
epsilon0 = 1/mu0 c^2
electron = e = 1.602176462e-19 C
ev = eV = electron volt
h = 6.62606896p24e-34 J s
hbar = h / 2 pi
G = 6.6743e-11 N m^2 / kg^2
amu = u = atomicmassunit = 1.66053873e-27 kg
N$A = avogadro = gram/amu mol
R = gasconstant = 8.314472 J / mol K
k = boltzmann = R / N$A
stdtemp = standardtemp = 273.15 K
atmosphere = atm = 101325 Pa
force = gravity = 9.80665 m/s^2
water = gram force/cm^3
waterdensity = gram / cm^3
molarvolume = mol R stdtemp / atm
alpha = 7.297352533e-3
Hg = 13.5951 gram force / cm^3
mmHg = mm Hg
inHg = inch Hg
dyne = cm gram / s^2
erg = cm dyne
P = poise = gram / cm s
candela = cd
lm = lumen = cd sr
lx = lux = lm/m^2
footcandle = lumen/ft^2
lb = pound = 0.45359237 kg
hp = horsepower = 550 foot pound force / s
grain = 1/7000 * pound
oz = ounce = 1/16 * pound
troyounce = 480 grain
brgallon = 4.54609 l
gal = gallon = usgallon = 231 inch^3
qt = quart = 1/4 * gallon
pt = pint = 1/2 * quart
cup = 1/2 * pint
tbsp = tablespoon = 1/16 * cup
teaspoon = 1/3 * tablespoon
floz = fluidounce = 1/16 * pint
bbl = barrel = 42 gallon
bu = bushel = 2150.42 inch^3
peck = 1/4 * bushel


Offset = new Type of BasicList
Offset + Measurement := (o,m) -> new Offset from { o#0 + m, o#1 }
Measurement + Offset := (m,o) -> new Offset from { o#0 + m, o#1 }
Offset - Measurement := (o,m) -> new Offset from { o#0 - m, o#1 }
Offset - Offset := (o,p) -> ( if o#1 =!= p#1 then error "expected offsets from the same base symbol"; o#0 - p#0 )
expression Offset := o -> expression o#0 + expression o#1
net Offset := net @@ expression

ZeroKelvin = new Offset from { 0 * K , symbol ZeroKelvin }
ZeroCelsius = ZeroKelvin + 273.15 K
ZeroFahrenheit = ZeroCelsius - 32 degF


type := new MutableHashTable;
scan({
	  s/s => "dimensionless quantity",
	  s => "time",
	  m => "length",
	  m^2 => "area",
	  m^3 => "volume",
	  volt => "voltage",
	  newton => "force",
	  watt => "power",
	  gm => "mass",
	  ohm => "resistance",
	  mho => "conductance",
	  pascal => "pressure",
	  joule => "energy, work",
	  newton/m^2 => "stress",
	  ampere => "current",
	  mole => "amount",
	  radian => "angle",
	  steradian => "solid angle",
	  coulomb => "charge",
	  farad => "capacitance",
	  henry => "inductance",
	  hertz => "frequency",
	  m/s => "velocity",
	  m/s^2 => "acceleration",
	  kg/m^3 => "density",
	  poise => "viscosity",
	  kelvin => "temperature",
	  candela => "luminous intensity",
	  lumen => "luminous flux",
	  lux => "illuminance, exitance",
	  cd/m^2 => "luminance",
	  J/K => "entropy",
	  W/K => "entropy flow"
	  },
     x -> (
	  m := x#0;
	  if instance(m,Measurement) then m = m#1;
	  type#m = if type#?m then append(type#m,x#1) else {x#1}
	  ))

Measurement#{Standard,AfterPrint} = x -> (
     << endl << concatenate(interpreterDepth:"o") << lineNumber << " : " << class x;
     m := x#1;
     if type#?m then << concatenate(" (",between(", ",type#m),")");
     << endl;
     )

UnitMonomial#{Standard,AfterPrint} = m -> (
     << endl << concatenate(interpreterDepth:"o") << lineNumber << " : " << class m;
     if type#?m then << concatenate(" (",between(", ",type#m),")");
     << endl;
     )

twodigits := s -> if match("^[[:digit:]]{2}",s) then s else "0" | s
onecolon = method()
onecolon RR := x -> toString floor x | ":" | twodigits toString (60 * (x - floor x))
onecolon QQ := onecolon ZZ := x -> onecolon (0. + x)
onecolon Measurement := m -> toString ( expression onecolon m#0 * expression m#1 )
twocolon = method()
twocolon RR := x -> toString floor x | ":" | twodigits onecolon (60 * (x - floor x))
twocolon QQ := twocolon ZZ := x -> twocolon (0. + x)
twocolon Measurement := m -> toString ( expression onecolon m#0 * expression m#1 )

export values Units#"private dictionary"
