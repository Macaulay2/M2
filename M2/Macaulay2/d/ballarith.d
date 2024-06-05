------------------------------------------
-- FLINT ball arithmetic (formerly Arb) --
------------------------------------------

use gmp;

declarations "
	#ifdef HAVE_ARB_H
	# include <arb.h>
	# include <acb.h>
	#endif
	#ifdef HAVE_FLINT_ARB_H
	# include <flint/arb.h>
	# include <flint/acb.h>
	#endif
	";

header "
	#ifdef HAVE_ARB_H
	# include <arb_hypgeom.h>
	# include <acb_hypgeom.h>
	#endif
	#ifdef HAVE_FLINT_ARB_H
	# include <flint/arb_hypgeom.h>
	# include <flint/acb_hypgeom.h>
	#endif
	";

-- TODO: RRball and CCball at top level?

------------
-- RRball --
------------

RRball := Pointer "arb_ptr";
init(x:RRball) ::= (
    Ccode(void, "arb_init(", x, ")");
    x);
newRRball():RRball := init(GCmalloc(RRball));
clear(x:RRball) ::= Ccode(void, "arb_clear(", x, ")");

-- clear after using
toRRball(x:RR, y:RR, prec:ulong):RRball := (
    z := newRRball();
    Ccode(void, "arb_set_interval_mpfr(", z, ", ", x, ", ", y, ", ", prec, ")");
    z);
toRRball(x:RR):RRball := toRRball(x, x, precision(x));
toRRball(x:RRi):RRball := toRRball(leftRR(x), rightRR(x), precision(x));

toRR(x:RRball, prec:ulong):RR := (
    y := newRRmutable(prec);
    Ccode(int, "arf_get_mpfr(", y, ", arb_midref(", x, "), MPFR_RNDN)");
    moveToRRandclear(y));

toRRi(x:RRball, prec:ulong):RRi := (
    y := newRRimutable(prec);
    Ccode(void, "arb_get_interval_mpfr((mpfr_ptr)&", y,
	"->left, (mpfr_ptr)&", y, "->right, ", x, ")");
    moveToRRiandclear(y));

moveToRRiandclear(x:RRball, prec:ulong):RRi := (
    r := toRRi(x, prec);
    clear(x);
    r);

------------
-- CCBall --
------------

CCball := Pointer "acb_ptr";
init(z:CCball) ::= (
    Ccode(void, "acb_init(", z, ")");
    z);
newCCball():CCball := init(GCmalloc(CCball));
clear(z:CCball) ::= Ccode(void, "acb_clear(", z, ")");

-- clear after using
toCCball(z:CC):CCball := (
    x := toRRball(realPart(z));
    y := toRRball(imaginaryPart(z));
    w := newCCball();
    Ccode(void, "acb_set_arb_arb(", w, ", ", x, ", ", y, ")");
    clear(x);
    clear(y);
    w);

toCC(z:CCball, prec:ulong):CC := (
    x := Ccode(RRball, "acb_realref(", z, ")");
    y := Ccode(RRball, "acb_imagref(", z, ")");
    toCC(toRR(x, prec), toRR(y, prec)));

moveToCCandclear(z:CCball, prec:ulong):CC := (
    r := toCC(z, prec);
    clear(z);
    r);
