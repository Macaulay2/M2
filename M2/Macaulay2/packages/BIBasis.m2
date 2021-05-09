-- -*- coding: utf-8 -*-
-- licensed under GPL v2 or any later version

----------------------------------------------------------------------
-- Header
----------------------------------------------------------------------

newPackage(
    "BIBasis",
    Version => "0.6.3",
    Date => "March 29, 2011",
    Authors => {
        {Name => "Mikhail Zinin", Email => "mzinin@gmail.com"}
    },
    Headline => "involutive Pommaret basis in a Boolean ring",
    Keywords => {"Groebner Basis Algorithms"},
    PackageImports => {"BooleanGB"},
    DebuggingMode => false
    )

export {
       -- functions
       "biBasis",
       -- options
       "toGroebner" -- whether reduce to Groebner basis or not
       }

debug Core

----------------------------------------------------------------------
-- Methods
----------------------------------------------------------------------

biBasis = method(TypicalValue => Ideal, Options => {toGroebner => true})
biBasis Ideal := Ideal => o -> I -> ideal map(ring I, rawBIBasis(raw compress generators I, o.toGroebner))

----------------------------------------------------------------------
-- Documentation
----------------------------------------------------------------------

beginDocumentation()

document {
        Key => BIBasis,
        Headline => "Involutive Pommaret basis in a Boolean ring",
        EM "BIBasis", TEX " is the package which implements the methods for constructing the reduced Pommaret 
        and Gr\\\"obner bases in a Boolean ring for a given polynomial Ideal.",
        PARA {
            "Some references:"
             },
        UL {
            TEX "V. P. Gerdt and M. V. Zinin, Involutive Method for Computing Gr\\\"obner Bases over F_2. Programming and Computer Software, Vol. 34, No. 4, 2008, 191-203.",
            TEX "V.P.Gerdt and M.V.Zinin. A Pommaret Division Algorithm for Computing Gr\\\"obner Bases in Boolean Rings. Proceedings of ISSAC 2008, ACM Press, 2008, pp.95--102.",
            "Vladimir Gerdt, Mikhail Zinin and Yuri Blinkov. On computation of Boolean involutive bases. Programming and Computer Software, Vol. 36, No. 2, 2010, 117-123."
           }        
         }

document {
        Key => { biBasis, 
                 (biBasis, Ideal), 
                 [biBasis, toGroebner]
               },
        Headline => "constructs a reduced Boolean Gröbner basis for the given Ideal, if the second argument is true (default), and reduced Boolean Pommaret basis otherwise.",
        Usage => "biBasis(I, toGroebner => true)",
        Inputs => {
                "I" => Ideal
                  },
        Outputs => {
                "P" => Ideal
                   },
        EXAMPLE {
                "R = ZZ/2[x, y, MonomialOrder => GRevLex]",
                "I = ideal(x*y + x + 1)",
                "P = biBasis(I, toGroebner => true)"
                },
        Caveat => UL {
                    "biBasis assumes coefficient Ring of the given Ideal is ZZ/2",
                    "biBasis always treats the given Ideal as a Boolean one",
                    "biBasis assumes the following monomials orders: Lex, GLex, GRevLex"
                     },
        SeeAlso => {gb, gbBoolean}
         }

----------------------------------------------------------------------
-- Tests
----------------------------------------------------------------------

-- Test 0
TEST ///
    -- loadPackage "BIBasis"
    R = ZZ/2[x, y, MonomialOrder => GRevLex]
    I = ideal(x*y + x + 1)
    P = biBasis(I)
    assert(P == ideal(x + 1, y))
///

-- Test 1
TEST ///
    -- loadPackage "BIBasis"
    R = ZZ/2[x, y, z, MonomialOrder => GRevLex]
    I = ideal(x, z)
    G = biBasis(I, toGroebner => true)
    assert(G == ideal(x, z))
    P = biBasis(I, toGroebner => false)
    assert(G == ideal(x, z, y*z))
///

-- Test 2
TEST ///
    -- loadPackage "BIBasis"
    R = ZZ/2[x0,x1,x2,x3,x4,x5,x6,x7,x8,x9, MonomialOrder => GRevLex]
    J = apply(gens R, x -> x^2+x)
    QR = R/J
    I = ideal(x9+x0*x1*x2*x3*x4*x5*x6+x0*x1*x2*x3*x4*x5*x7+x0*x1*x2*x3*x4*x6*x7+x0*x1*x2*x3*x5*x6*x7+x0*x1*x2*x4*x5*x6*x7+x0*x1*x3*x4*x5*x6*x7+x0*x2*x3*x4*x5*x6*x7+x1*x2*x3*x4*x5*x6*x7+x0*x1*x2+x0*x1*x3+x0*x1*x4+x0*x1*x5+x0*x1*x6+x0*x1*x7+x0*x2*x3+x0*x2*x4+x0*x2*x5+x0*x2*x6+x0*x2*x7+x0*x3*x4+x0*x3*x5+x0*x3*x6+x0*x3*x7+x0*x4*x5+x0*x4*x6+x0*x4*x7+x0*x5*x6+x0*x5*x7+x0*x6*x7+x1*x2*x3+x1*x2*x4+x1*x2*x5+x1*x2*x6+x1*x2*x7+x1*x3*x4+x1*x3*x5+x1*x3*x6+x1*x3*x7+x1*x4*x5+x1*x4*x6+x1*x4*x7+x1*x5*x6+x1*x5*x7+x1*x6*x7+x2*x3*x4+x2*x3*x5+x2*x3*x6+x2*x3*x7+x2*x4*x5+x2*x4*x6+x2*x4*x7+x2*x5*x6+x2*x5*x7+x2*x6*x7+x3*x4*x5+x3*x4*x6+x3*x4*x7+x3*x5*x6+x3*x5*x7+x3*x6*x7+x4*x5*x6+x4*x5*x7+x4*x6*x7+x5*x6*x7+x0*x1*x2*x3*x4*x5*x6*x8+x0*x1*x2*x3*x4*x5*x7*x8+x0*x1*x2*x3*x4*x6*x7*x8+x0*x1*x2*x3*x5*x6*x7*x8+x0*x1*x2*x4*x5*x6*x7*x8+x0*x1*x3*x4*x5*x6*x7*x8+x0*x2*x3*x4*x5*x6*x7*x8+x1*x2*x3*x4*x5*x6*x7*x8+x0*x1*x2*x3*x4*x5*x8+x0*x1*x2*x3*x4*x6*x8+x0*x1*x2*x3*x4*x7*x8+x0*x1*x2*x3*x5*x6*x8+x0*x1*x2*x3*x5*x7*x8+x0*x1*x2*x3*x6*x7*x8+x0*x1*x2*x4*x5*x6*x8+x0*x1*x2*x4*x5*x7*x8+x0*x1*x2*x4*x6*x7*x8+x0*x1*x2*x5*x6*x7*x8+x0*x1*x3*x4*x5*x6*x8+x0*x1*x3*x4*x5*x7*x8+x0*x1*x3*x4*x6*x7*x8+x0*x1*x3*x5*x6*x7*x8+x0*x1*x4*x5*x6*x7*x8+x0*x2*x3*x4*x5*x6*x8+x0*x2*x3*x4*x5*x7*x8+x0*x2*x3*x4*x6*x7*x8+x0*x2*x3*x5*x6*x7*x8+x0*x2*x4*x5*x6*x7*x8+x0*x3*x4*x5*x6*x7*x8+x1*x2*x3*x4*x5*x6*x8+x1*x2*x3*x4*x5*x7*x8+x1*x2*x3*x4*x6*x7*x8+x1*x2*x3*x5*x6*x7*x8+x1*x2*x4*x5*x6*x7*x8+x1*x3*x4*x5*x6*x7*x8+x2*x3*x4*x5*x6*x7*x8+x0*x1*x2*x8+x0*x1*x3*x8+x0*x1*x4*x8+x0*x1*x5*x8+x0*x1*x6*x8+x0*x1*x7*x8+x0*x2*x3*x8+x0*x2*x4*x8+x0*x2*x5*x8+x0*x2*x6*x8+x0*x2*x7*x8+x0*x3*x4*x8+x0*x3*x5*x8+x0*x3*x6*x8+x0*x3*x7*x8+x0*x4*x5*x8+x0*x4*x6*x8+x0*x4*x7*x8+x0*x5*x6*x8+x0*x5*x7*x8+x0*x6*x7*x8+x1*x2*x3*x8+x1*x2*x4*x8+x1*x2*x5*x8+x1*x2*x6*x8+x1*x2*x7*x8+x1*x3*x4*x8+x1*x3*x5*x8+x1*x3*x6*x8+x1*x3*x7*x8+x1*x4*x5*x8+x1*x4*x6*x8+x1*x4*x7*x8+x1*x5*x6*x8+x1*x5*x7*x8+x1*x6*x7*x8+x2*x3*x4*x8+x2*x3*x5*x8+x2*x3*x6*x8+x2*x3*x7*x8+x2*x4*x5*x8+x2*x4*x6*x8+x2*x4*x7*x8+x2*x5*x6*x8+x2*x5*x7*x8+x2*x6*x7*x8+x3*x4*x5*x8+x3*x4*x6*x8+x3*x4*x7*x8+x3*x5*x6*x8+x3*x5*x7*x8+x3*x6*x7*x8+x4*x5*x6*x8+x4*x5*x7*x8+x4*x6*x7*x8+x5*x6*x7*x8+x0*x1*x8+x0*x2*x8+x0*x3*x8+x0*x4*x8+x0*x5*x8+x0*x6*x8+x0*x7*x8+x1*x2*x8+x1*x3*x8+x1*x4*x8+x1*x5*x8+x1*x6*x8+x1*x7*x8+x2*x3*x8+x2*x4*x8+x2*x5*x8+x2*x6*x8+x2*x7*x8+x3*x4*x8+x3*x5*x8+x3*x6*x8+x3*x7*x8+x4*x5*x8+x4*x6*x8+x4*x7*x8+x5*x6*x8+x5*x7*x8+x6*x7*x8)
    G1 = biBasis(I)
    G2 = gb(I)
    assert(sort gens G1 - sort gens G2 == 0) 
///

-- Test 3
TEST ///
    -- loadPackage "BIBasis"
    R = ZZ/2[x0,x1,x2,x3,x4,x5,x6,x7,x8,x9, MonomialOrder => GLex]
    J = apply(gens R, x -> x^2+x)
    QR = R/J
    I = ideal(x9+x0*x1*x2*x3*x4*x5*x6+x0*x1*x2*x3*x4*x5*x7+x0*x1*x2*x3*x4*x6*x7+x0*x1*x2*x3*x5*x6*x7+x0*x1*x2*x4*x5*x6*x7+x0*x1*x3*x4*x5*x6*x7+x0*x2*x3*x4*x5*x6*x7+x1*x2*x3*x4*x5*x6*x7+x0*x1*x2+x0*x1*x3+x0*x1*x4+x0*x1*x5+x0*x1*x6+x0*x1*x7+x0*x2*x3+x0*x2*x4+x0*x2*x5+x0*x2*x6+x0*x2*x7+x0*x3*x4+x0*x3*x5+x0*x3*x6+x0*x3*x7+x0*x4*x5+x0*x4*x6+x0*x4*x7+x0*x5*x6+x0*x5*x7+x0*x6*x7+x1*x2*x3+x1*x2*x4+x1*x2*x5+x1*x2*x6+x1*x2*x7+x1*x3*x4+x1*x3*x5+x1*x3*x6+x1*x3*x7+x1*x4*x5+x1*x4*x6+x1*x4*x7+x1*x5*x6+x1*x5*x7+x1*x6*x7+x2*x3*x4+x2*x3*x5+x2*x3*x6+x2*x3*x7+x2*x4*x5+x2*x4*x6+x2*x4*x7+x2*x5*x6+x2*x5*x7+x2*x6*x7+x3*x4*x5+x3*x4*x6+x3*x4*x7+x3*x5*x6+x3*x5*x7+x3*x6*x7+x4*x5*x6+x4*x5*x7+x4*x6*x7+x5*x6*x7+x0*x1*x2*x3*x4*x5*x6*x8+x0*x1*x2*x3*x4*x5*x7*x8+x0*x1*x2*x3*x4*x6*x7*x8+x0*x1*x2*x3*x5*x6*x7*x8+x0*x1*x2*x4*x5*x6*x7*x8+x0*x1*x3*x4*x5*x6*x7*x8+x0*x2*x3*x4*x5*x6*x7*x8+x1*x2*x3*x4*x5*x6*x7*x8+x0*x1*x2*x3*x4*x5*x8+x0*x1*x2*x3*x4*x6*x8+x0*x1*x2*x3*x4*x7*x8+x0*x1*x2*x3*x5*x6*x8+x0*x1*x2*x3*x5*x7*x8+x0*x1*x2*x3*x6*x7*x8+x0*x1*x2*x4*x5*x6*x8+x0*x1*x2*x4*x5*x7*x8+x0*x1*x2*x4*x6*x7*x8+x0*x1*x2*x5*x6*x7*x8+x0*x1*x3*x4*x5*x6*x8+x0*x1*x3*x4*x5*x7*x8+x0*x1*x3*x4*x6*x7*x8+x0*x1*x3*x5*x6*x7*x8+x0*x1*x4*x5*x6*x7*x8+x0*x2*x3*x4*x5*x6*x8+x0*x2*x3*x4*x5*x7*x8+x0*x2*x3*x4*x6*x7*x8+x0*x2*x3*x5*x6*x7*x8+x0*x2*x4*x5*x6*x7*x8+x0*x3*x4*x5*x6*x7*x8+x1*x2*x3*x4*x5*x6*x8+x1*x2*x3*x4*x5*x7*x8+x1*x2*x3*x4*x6*x7*x8+x1*x2*x3*x5*x6*x7*x8+x1*x2*x4*x5*x6*x7*x8+x1*x3*x4*x5*x6*x7*x8+x2*x3*x4*x5*x6*x7*x8+x0*x1*x2*x8+x0*x1*x3*x8+x0*x1*x4*x8+x0*x1*x5*x8+x0*x1*x6*x8+x0*x1*x7*x8+x0*x2*x3*x8+x0*x2*x4*x8+x0*x2*x5*x8+x0*x2*x6*x8+x0*x2*x7*x8+x0*x3*x4*x8+x0*x3*x5*x8+x0*x3*x6*x8+x0*x3*x7*x8+x0*x4*x5*x8+x0*x4*x6*x8+x0*x4*x7*x8+x0*x5*x6*x8+x0*x5*x7*x8+x0*x6*x7*x8+x1*x2*x3*x8+x1*x2*x4*x8+x1*x2*x5*x8+x1*x2*x6*x8+x1*x2*x7*x8+x1*x3*x4*x8+x1*x3*x5*x8+x1*x3*x6*x8+x1*x3*x7*x8+x1*x4*x5*x8+x1*x4*x6*x8+x1*x4*x7*x8+x1*x5*x6*x8+x1*x5*x7*x8+x1*x6*x7*x8+x2*x3*x4*x8+x2*x3*x5*x8+x2*x3*x6*x8+x2*x3*x7*x8+x2*x4*x5*x8+x2*x4*x6*x8+x2*x4*x7*x8+x2*x5*x6*x8+x2*x5*x7*x8+x2*x6*x7*x8+x3*x4*x5*x8+x3*x4*x6*x8+x3*x4*x7*x8+x3*x5*x6*x8+x3*x5*x7*x8+x3*x6*x7*x8+x4*x5*x6*x8+x4*x5*x7*x8+x4*x6*x7*x8+x5*x6*x7*x8+x0*x1*x8+x0*x2*x8+x0*x3*x8+x0*x4*x8+x0*x5*x8+x0*x6*x8+x0*x7*x8+x1*x2*x8+x1*x3*x8+x1*x4*x8+x1*x5*x8+x1*x6*x8+x1*x7*x8+x2*x3*x8+x2*x4*x8+x2*x5*x8+x2*x6*x8+x2*x7*x8+x3*x4*x8+x3*x5*x8+x3*x6*x8+x3*x7*x8+x4*x5*x8+x4*x6*x8+x4*x7*x8+x5*x6*x8+x5*x7*x8+x6*x7*x8)
    G1 = biBasis(I)
    G2 = gb(I)
    assert(sort gens G1 - sort gens G2 == 0) 
///

-- Test 4
TEST ///
    -- loadPackage "BIBasis"
    R = ZZ/2[x0,x1,x2,x3,x4,x5,x6,x7,x8,x9, MonomialOrder => Lex]
    J = apply(gens R, x -> x^2+x)
    QR = R/J
    I = ideal(x9+x0*x1*x2*x3*x4*x5*x6+x0*x1*x2*x3*x4*x5*x7+x0*x1*x2*x3*x4*x6*x7+x0*x1*x2*x3*x5*x6*x7+x0*x1*x2*x4*x5*x6*x7+x0*x1*x3*x4*x5*x6*x7+x0*x2*x3*x4*x5*x6*x7+x1*x2*x3*x4*x5*x6*x7+x0*x1*x2+x0*x1*x3+x0*x1*x4+x0*x1*x5+x0*x1*x6+x0*x1*x7+x0*x2*x3+x0*x2*x4+x0*x2*x5+x0*x2*x6+x0*x2*x7+x0*x3*x4+x0*x3*x5+x0*x3*x6+x0*x3*x7+x0*x4*x5+x0*x4*x6+x0*x4*x7+x0*x5*x6+x0*x5*x7+x0*x6*x7+x1*x2*x3+x1*x2*x4+x1*x2*x5+x1*x2*x6+x1*x2*x7+x1*x3*x4+x1*x3*x5+x1*x3*x6+x1*x3*x7+x1*x4*x5+x1*x4*x6+x1*x4*x7+x1*x5*x6+x1*x5*x7+x1*x6*x7+x2*x3*x4+x2*x3*x5+x2*x3*x6+x2*x3*x7+x2*x4*x5+x2*x4*x6+x2*x4*x7+x2*x5*x6+x2*x5*x7+x2*x6*x7+x3*x4*x5+x3*x4*x6+x3*x4*x7+x3*x5*x6+x3*x5*x7+x3*x6*x7+x4*x5*x6+x4*x5*x7+x4*x6*x7+x5*x6*x7+x0*x1*x2*x3*x4*x5*x6*x8+x0*x1*x2*x3*x4*x5*x7*x8+x0*x1*x2*x3*x4*x6*x7*x8+x0*x1*x2*x3*x5*x6*x7*x8+x0*x1*x2*x4*x5*x6*x7*x8+x0*x1*x3*x4*x5*x6*x7*x8+x0*x2*x3*x4*x5*x6*x7*x8+x1*x2*x3*x4*x5*x6*x7*x8+x0*x1*x2*x3*x4*x5*x8+x0*x1*x2*x3*x4*x6*x8+x0*x1*x2*x3*x4*x7*x8+x0*x1*x2*x3*x5*x6*x8+x0*x1*x2*x3*x5*x7*x8+x0*x1*x2*x3*x6*x7*x8+x0*x1*x2*x4*x5*x6*x8+x0*x1*x2*x4*x5*x7*x8+x0*x1*x2*x4*x6*x7*x8+x0*x1*x2*x5*x6*x7*x8+x0*x1*x3*x4*x5*x6*x8+x0*x1*x3*x4*x5*x7*x8+x0*x1*x3*x4*x6*x7*x8+x0*x1*x3*x5*x6*x7*x8+x0*x1*x4*x5*x6*x7*x8+x0*x2*x3*x4*x5*x6*x8+x0*x2*x3*x4*x5*x7*x8+x0*x2*x3*x4*x6*x7*x8+x0*x2*x3*x5*x6*x7*x8+x0*x2*x4*x5*x6*x7*x8+x0*x3*x4*x5*x6*x7*x8+x1*x2*x3*x4*x5*x6*x8+x1*x2*x3*x4*x5*x7*x8+x1*x2*x3*x4*x6*x7*x8+x1*x2*x3*x5*x6*x7*x8+x1*x2*x4*x5*x6*x7*x8+x1*x3*x4*x5*x6*x7*x8+x2*x3*x4*x5*x6*x7*x8+x0*x1*x2*x8+x0*x1*x3*x8+x0*x1*x4*x8+x0*x1*x5*x8+x0*x1*x6*x8+x0*x1*x7*x8+x0*x2*x3*x8+x0*x2*x4*x8+x0*x2*x5*x8+x0*x2*x6*x8+x0*x2*x7*x8+x0*x3*x4*x8+x0*x3*x5*x8+x0*x3*x6*x8+x0*x3*x7*x8+x0*x4*x5*x8+x0*x4*x6*x8+x0*x4*x7*x8+x0*x5*x6*x8+x0*x5*x7*x8+x0*x6*x7*x8+x1*x2*x3*x8+x1*x2*x4*x8+x1*x2*x5*x8+x1*x2*x6*x8+x1*x2*x7*x8+x1*x3*x4*x8+x1*x3*x5*x8+x1*x3*x6*x8+x1*x3*x7*x8+x1*x4*x5*x8+x1*x4*x6*x8+x1*x4*x7*x8+x1*x5*x6*x8+x1*x5*x7*x8+x1*x6*x7*x8+x2*x3*x4*x8+x2*x3*x5*x8+x2*x3*x6*x8+x2*x3*x7*x8+x2*x4*x5*x8+x2*x4*x6*x8+x2*x4*x7*x8+x2*x5*x6*x8+x2*x5*x7*x8+x2*x6*x7*x8+x3*x4*x5*x8+x3*x4*x6*x8+x3*x4*x7*x8+x3*x5*x6*x8+x3*x5*x7*x8+x3*x6*x7*x8+x4*x5*x6*x8+x4*x5*x7*x8+x4*x6*x7*x8+x5*x6*x7*x8+x0*x1*x8+x0*x2*x8+x0*x3*x8+x0*x4*x8+x0*x5*x8+x0*x6*x8+x0*x7*x8+x1*x2*x8+x1*x3*x8+x1*x4*x8+x1*x5*x8+x1*x6*x8+x1*x7*x8+x2*x3*x8+x2*x4*x8+x2*x5*x8+x2*x6*x8+x2*x7*x8+x3*x4*x8+x3*x5*x8+x3*x6*x8+x3*x7*x8+x4*x5*x8+x4*x6*x8+x4*x7*x8+x5*x6*x8+x5*x7*x8+x6*x7*x8)
    G1 = biBasis(I)
    G2 = gb(I)
    assert(sort gens G1 - sort gens G2 == 0) 
///

-- Test 5
TEST ///
    -- loadPackage "BIBasis"
    R = ZZ/2[u0,u1,u2,u3,u4,u5,u6,u7,u8,u9, MonomialOrder => GRevLex]
    J = apply(gens R, x -> x^2+x)
    QR = R/J
    I = ideal(u0*u1+u1*u2+u1+u2*u3+u3*u4+u4*u5+u5*u6+u6*u7+u7*u8+u8*u9,u0*u2+u1+u1*u3+u2*u4+u2+u3*u5+u4*u6+u5*u7+u6*u8+u7*u9,u0*u3+u1*u2+u1*u4+u2*u5+u3*u6+u3+u4*u7+u5*u8+u6*u9,u0*u4+u1*u3+u1*u5+u2+u2*u6+u3*u7+u4*u8+u4+u5*u9,u0*u5+u1*u4+u1*u6+u2*u3+u2*u7+u3*u8+u4*u9+u5,u0*u6+u1*u5+u1*u7+u2*u4+u2*u8+u3+u3*u9+u6,u0*u7+u1*u6+u1*u8+u2*u5+u2*u9+u3*u4+u7,u0*u8+u1*u7+u1*u9+u2*u6+u3*u5+u4+u8,u0+u1+u2+u3+u4+u5+u6+u7+u8+u9+1)
    G1 = biBasis(I)
    G2 = gb(I)
    assert(sort gens G1 - sort gens G2 == 0) 
///

-- Test 6
TEST ///
    -- loadPackage "BIBasis"
    R = ZZ/2[u0,u1,u2,u3,u4,u5,u6,u7,u8,u9, MonomialOrder => GLex]
    J = apply(gens R, x -> x^2+x)
    QR = R/J
    I = ideal(u0*u1+u1*u2+u1+u2*u3+u3*u4+u4*u5+u5*u6+u6*u7+u7*u8+u8*u9,u0*u2+u1+u1*u3+u2*u4+u2+u3*u5+u4*u6+u5*u7+u6*u8+u7*u9,u0*u3+u1*u2+u1*u4+u2*u5+u3*u6+u3+u4*u7+u5*u8+u6*u9,u0*u4+u1*u3+u1*u5+u2+u2*u6+u3*u7+u4*u8+u4+u5*u9,u0*u5+u1*u4+u1*u6+u2*u3+u2*u7+u3*u8+u4*u9+u5,u0*u6+u1*u5+u1*u7+u2*u4+u2*u8+u3+u3*u9+u6,u0*u7+u1*u6+u1*u8+u2*u5+u2*u9+u3*u4+u7,u0*u8+u1*u7+u1*u9+u2*u6+u3*u5+u4+u8,u0+u1+u2+u3+u4+u5+u6+u7+u8+u9+1)
    G1 = biBasis(I)
    G2 = gb(I)
    assert(sort gens G1 - sort gens G2 == 0) 
///

-- Test 7
TEST ///
    -- loadPackage "BIBasis"
    R = ZZ/2[u0,u1,u2,u3,u4,u5,u6,u7,u8,u9, MonomialOrder => Lex]
    J = apply(gens R, x -> x^2+x)
    QR = R/J
    I = ideal(u0*u1+u1*u2+u1+u2*u3+u3*u4+u4*u5+u5*u6+u6*u7+u7*u8+u8*u9,u0*u2+u1+u1*u3+u2*u4+u2+u3*u5+u4*u6+u5*u7+u6*u8+u7*u9,u0*u3+u1*u2+u1*u4+u2*u5+u3*u6+u3+u4*u7+u5*u8+u6*u9,u0*u4+u1*u3+u1*u5+u2+u2*u6+u3*u7+u4*u8+u4+u5*u9,u0*u5+u1*u4+u1*u6+u2*u3+u2*u7+u3*u8+u4*u9+u5,u0*u6+u1*u5+u1*u7+u2*u4+u2*u8+u3+u3*u9+u6,u0*u7+u1*u6+u1*u8+u2*u5+u2*u9+u3*u4+u7,u0*u8+u1*u7+u1*u9+u2*u6+u3*u5+u4+u8,u0+u1+u2+u3+u4+u5+u6+u7+u8+u9+1)
    G1 = biBasis(I)
    G2 = gb(I)
    assert(sort gens G1 - sort gens G2 == 0) 
///

end
