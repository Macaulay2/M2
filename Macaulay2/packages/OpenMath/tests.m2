------------------------
-----SOME OLD TESTS-----
------------------------
-- This file is NOT intended to be loaded automatically by OpenMath.m2 !!
--

R = QQ[x];
p = x^2 - 1;
s = openMath p

s = OMA("polynomial4", "factorise", {s} );
fs = value s;
print fs;
tfs = openMath fs

-- (lambda.x.(x^2)) ((lambda.x.x+3)(1)) = 16 (I think)
x = symbol x;
b1 = OMBIND("fns1", "lambda", { x }, { OMA("arith1", "power", { OMV("x"), OMI(2) }) });
b2 = OMBIND("fns1", "lambda", { x }, { OMA("arith1", "plus", { OMV("x"), OMI(3) }) });
s = OMA(b1, {OMA(b2, {OMI(1)})});
s2 = value s;
print "s2 = \n"; print s2;


s = OMA("scscp1", "procedure_call", { OMA("arith1", "plus", {OMI(1), OMI(5) }) });
pc = OMATTR(s, hashTable{ OMS("scscp1", "call_id") => OMSTR("baz"), OMS("scscp1", "option_return_cookie") => OMSTR("") } );
--pc = OMATTR(s, hashTable{ OMS("scscp1", "call_id") => OMSTR("baz") } );

<< "pc becomes " ;
fpc = value pc;
print fpc;

<< "trying to resolve OMR: " << endl;
s = OMA("scscp1", "procedure_call", { OMA("scscp2", "retrieve", {OMR("#r0")})});
pc = OMATTR(s, hashTable{ "scscp.call_id" => (OMS("scscp1", "call_id"), OMSTR("baz")), "scscp1.option_return_object" => (OMS("scscp1", "option_return_object"), OMSTR("")) } );
print value pc;


l = openMath(set{1,3,7});
b = OMBIND("fns1", "lambda", { x }, { OMA("arith1", "power", { OMV("x"), OMI(2) }) });
m = OMA("set1", "map", {b, l});
print value m;

R = GF(2,5);
t = random(R);
<< "t = " << t << endl;
<< "from to t = " << value openMath t << endl;

R = GF(2);
R[x]; p = x^2+x+1;
s = OMA("field3", "field_by_poly", {
	openMath R,
	openMath p
} )
<< "from(s) = " << value s << endl;


-- Long (GAP) input test: --
toLibxmlNode openMath value parse///<OMOBJ>
	<OMA>
		<OMS cd="polyd1" name="DMP"/>
		<OMA id="polyringSJk3jJX4oVxJWsOb" >
			<OMS cd="polyd1" name="poly_ring_d"/>
			<OMS cd="setname1" name="Q"/>
			<OMI>2</OMI>
		</OMA>
		<OMA>
			<OMS cd="polyd1" name="SDMP"/>
			<OMA>
				<OMS cd="polyd1" name="term"/>
				<OMA>
					<OMS cd="nums1" name="rational"/>
					<OMI>17</OMI>
					<OMI>3</OMI>
				</OMA>
				<OMI>0</OMI>
				<OMI>0</OMI>
			</OMA>
			<OMA>
				<OMS cd="polyd1" name="term"/>
				<OMI>1</OMI>
				<OMI>1</OMI>
				<OMI>1</OMI>
			</OMA>
			<OMA>
				<OMS cd="polyd1" name="term"/>
				<OMI>1</OMI>
				<OMI>2</OMI>
				<OMI>0</OMI>
			</OMA>
		</OMA>
	</OMA>
</OMOBJ>///

