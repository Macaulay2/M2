-*
Copyright (C) 2019 Dylan Peifer and Mahrud Sayrafi

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.
*-

-- borrowed from initial version of FGLM
-- written by Dylan Peifer and Mahrud Sayrafi
-- see https://github.com/Macaulay2/M2/pull/947

incrLU = method()
incrLU(MutableList, MutableMatrix, MutableMatrix, ZZ) := (P, LU, v, n) -> (
    m := numrows LU;
    -- permute rows of v according to P
    rowPermute(v, 0, new List from P);
    -- copy v to LU_{n}
    for j to m - 1 do LU_(j, n) = v_(j, 0);
    -- reduce LU_{n} + forward substitute
    -- FIXME: about 25% of total time is spent in this loop
    forwardSub(LU, v, n, Strategy => "incremental");
    -- place solution of forward substitution in U
    for i to n - 1 do LU_(i, n) = v_(i, 0);
    -- update P
    n' := position(n..m-1, j -> LU_(j, n) != 0);
    if n' =!= null then n' = n' + n else return 0;
    if n != n' then (
       rowSwap(LU, n, n');
       P#n = P#n + P#n';
       P#n' = P#n - P#n';
       P#n = P#n - P#n';
    );
    -- set 1 to diagonal of L
    for j from n + 1 to m - 1 do LU_(j, n) = LU_(j, n) / LU_(n, n);
    -- return new v
    transpose matrix { toList(n:0) | for j from n to m - 1 list LU_(j, n) })

forwardSub = method(Options => {Strategy => null})
forwardSub(MutableMatrix, MutableMatrix, ZZ) := Nothing => opts -> (L, x, n) -> (
    for i to n - 1 do (
	x_(i, 0) = if opts.Strategy === "incremental" then L_(i, n) else L_(i, n) / L_(i,i);
	columnAdd(L, n, -x_(i, 0), i);
	);
    )

backSub = method(Options => {Strategy => null})
backSub(MutableMatrix, MutableMatrix, ZZ) := Nothing => opts -> (U, x, n) -> (
    for i from 1 to n do (
	x_(n-i, 0) = U_(n-i, n) / U_(n-i,n-i);
	columnAdd(U, n, -x_(n-i, 0), n-i);
	);
    )

-- does this preserve LU=M?
-- TODO: can incrLU do this automatically, perhaps when given an option?
colReduce = method(Options => {Strategy => null})
colReduce(MutableMatrix, ZZ) := Nothing => opts -> (L, n) -> (
    for i to n - 1 do (
	columnAdd(L, i, -L_(n, i), n);
	);
    )

extractLU = (LU, r0, c0) -> (
    RP := ring LU;
    if LU == 0 then return if c0 < r0
    then (mutableMatrix(id_(RP^c0) || map(RP^(r0 - c0), RP^c0, 0)), mutableMatrix(0 * id_(RP^c0)))
    else (mutableMatrix(id_(RP^r0)),  mutableMatrix(map(RP^r0, RP^c0, 0)));
    transpose mutableMatrix apply(c0, c -> apply(r0, r -> if c <  r then LU_(r, c) else if c == r then 1_RP else 0_RP)),
    transpose mutableMatrix apply(c0, c -> apply(c0, r -> if c >= r then LU_(r, c) else 0_RP)))

end--
restart
needs "LU.m2"

n = 2
kk = ZZ/101 -- frac(ZZ/101[x])

P = (0..n-1)
P = new MutableList from P
M = mutableMatrix random(kk^3,kk^2)
LU = mutableMatrix(map(kk^3,kk^3,0))
elapsedTime for i to n - 1 do incrLU(P, LU, M_{i}, i);
(L, U) = extractLU(LU, numRows LU, n)
assert(L * U == M)

V = 5*M_{0} + 2 * M_{1}
lambda = transpose mutableMatrix{{0_kk, 0_kk}}

apply(3, i -> LU_(i,2) = V_(i,0))
forwardSub(LU, lambda, 2, Strategy => "incremental");
LU, lambda

apply(2, i -> LU_(i,2) = lambda_(i,0))
backSub(LU, lambda, 2);
LU, lambda

assert(M * lambda == V)

colReduce(LU, 1)
LU

elapsedTime (p, L, U) = LUdecomposition M
colReduce(L, 1)
L
assert(L * U == M)
--rawLUincremental(P, raw LU, raw M_{0}, 0)

