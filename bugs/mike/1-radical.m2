-- In-Reply-To: <B087E517-19AC-4A57-8F86-89F18D7972F7@msri.org>
-- Cc: Dan Grayson <dan@math.uiuc.edu>, Craig Huneke <huneke@math.ku.edu>
-- From: Michael Stillman <mike@math.cornell.edu>
-- Subject: Re: A Macaulay2 problem
-- Date: Wed, 20 Dec 2006 13:08:53 -0500
-- To: David Eisenbud <de@msri.org>
-- 
-- Hi David, Craig, Dan,
-- 
-- I found the bug in 'radical': "presentation Ring" used to return a  
-- Groebner basis, but now it returns the matrix of generators used to  
-- make the ring.  I checked all uses of 'presentation' in M2, and it  
-- seems that this was the only occurrence that was assuming this.  The  
-- source code now is fixed.
-- 
-- However, there is another example later (number 236 on your list):
-- for which the default algorithm (EisenbudHunekeVasconcelos) for radical doesn't finish  
-- reasonably:
-- 

   kk = ZZ/101
   S=kk[vars(0..11)]

   I = ideal"-be+af,-de+cf,-dg+ch,-bi+aj"
   time radical I -- fast now

   J = ideal"-de+cf,-bg+ah,-fg+eh,-bi+aj,-di+cj"
   time intersect decompose J -- fast
   time radical J --  a problem

-- 
-- Also in M1:
-- 
-- <ring 12 a-z r
-- <ideal j -be+af -de+cf -dg+ch -bi+aj
-- <radical j k ; works great
-- <ideal j -de+cf -bg+ah -fg+eh -bi+aj -di+cj
-- <radical j k
-- type k
-- 
-- The problem is that a couple of steps into the algorithm, one must  
-- compute the 5 x 5 minors of a rather large matrix.  Perhaps we will  
-- need to change the default behavior...
-- 
-- Dan is visiting here in early January.  We'll probably make a new  
-- version at the end of that visit.
-- 
-- -- Mike
-- 
-- On Dec 20, 2006, at 1:32 AM, David Eisenbud wrote:
-- 
-- > Dear Mike and Dan,
-- >
-- > My copy of M2 runs through the first 85 subsets of 10 elements
-- >  in the following program, then
-- > chokes completely on the 86-th --- even when I run that separately,
-- > as in the following (it just
-- > eats through all the memory in the machine. M1 has no problems (see  
-- > Craig's
-- > note, below. Any idea what's up?
-- >
-- > By the way, the right index set in the following would be the set  
-- > of all (isom types of)
-- > graphs on 5 vertices, a lot smaller than the subsets of 10  
-- > elements. Is there an easy
-- > way to produce this index set?
-- >
-- > --The original program:
-- > S=kk[vars(0..2*5)]
-- > m=genericMatrix(S,2,5)
-- > i=minors(2,m)
-- > count=-1
-- > for L in  subsets(9) do(
-- > count=count+1;
-- > j=ideal ((gens i)*map(source gens i, S^{#L:-2},i_L));
-- > print (count, L, j==radical j)
-- > )
-- >
-- >
-- > -- and the version with just the troublesome example:
-- > restart
-- > S=kk[vars(0..2*5)]
-- > m=genericMatrix(S,2,5)
-- > i=minors(2,m)
-- > L=(subsets(9))_86
-- > j=ideal ((gens i)*map(source gens i, S^{#L:-2},i_L));
-- > radical j
-- >
-- >
-- > Thanks!
-- >
-- > David
-- >
-- >
-- > ---
-- > David Eisenbud
-- > Director, MSRI           www.msri.org
-- > tel: 510-642-0143     fax: 510-642-8609
-- >
-- >
-- >
-- > On Dec 19, 2006, at 2:16 PM, Craig Huneke wrote:
-- >
-- >> I ran the example on my machine at home and it computed the
-- >> radical in less than 1 second. Something must have been strange.
-- >> I'm using Macaulay classic, of course.
-- >>
-- >> Craig
-- >
-- 
