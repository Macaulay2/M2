-- -*- coding: utf-8 -*-
------------------------------------------------------------------------------
-- Copyright 2009--2020 Gregory G. Smith
--
-- This program is free software: you can redistribute it and/or modify it
-- under the terms of the GNU General Public License as published by the Free
-- Software Foundation, either version 3 of the License, or (at your option)
-- any later version.
--
-- This program is distributed in the hope that it will be useful, but WITHOUT
-- ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
-- FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
-- more details.
--
-- You should have received a copy of the GNU General Public License along
-- with this program.  If not, see <http://www.gnu.org/licenses/>.
------------------------------------------------------------------------------
newPackage(
    "NormalToricVarieties",
    AuxiliaryFiles => true,
    Version => "1.9",
    Date => "31 May 2020",
    Authors => {{
        Name => "Gregory G. Smith", 
        Email => "ggsmith@mast.queensu.ca", 
        HomePage => "http://www.mast.queensu.ca/~ggsmith"}},
    Headline => "routines for working with normal toric varieties and related objects",
    Keywords => {"Toric Geometry"},
    PackageExports => {"Polyhedra", "Schubert2"},
    PackageImports => {"FourierMotzkin","Normaliz","LLLBases"},
    DebuggingMode => false
    )

export {   
    -- types
    "NormalToricVariety",
    "ToricDivisor",  
    "ToricMap",  
    --functions / methods
    "affineSpace",
    "toricBlowup",
    "cartesianProduct",
    "cartierDivisorGroup", 
    "classGroup",
    "diagonalToricMap",
    "fromCDivToPic",  
    "fromCDivToWDiv",
    "fromPicToCl",     
    "fromWDivToCl",
    "hirzebruchSurface", 
    "isAmple",    
    "isCartier",  
    "isDegenerate",
    "isDominant",
    "isEffective",    
    "isFano", 
    "isFibration",
    "isNef",  
    "isProjective",
    "isProper", 
    "isQQCartier", 
    "kleinschmidt",
    "makeSimplicial",
    "makeSmooth",
    "nefGenerators",  
    "normalToricVariety",  
    "orbits",   
    "picardGroup",  
    "pullback",  
    "toricProjectiveSpace",
    "smallAmpleToricDivisor",  
    "smoothFanoToricVariety",
    "toricDivisor",  
    "weightedProjectiveSpace",  
    "weilDivisorGroup",
    -- symbols  
    "WeilToClass"
    }

protect emsBound
protect rawHHOO
protect maxRayList

------------------------------------------------------------------------------
-- CODE
------------------------------------------------------------------------------
load "NormalToricVarieties/ToricVarieties.m2"
load "NormalToricVarieties/Divisors.m2"
load "NormalToricVarieties/Sheaves.m2"
load "NormalToricVarieties/Chow.m2"
load "NormalToricVarieties/ToricMaps.m2"

------------------------------------------------------------------------------
-- THINGS TO IMPLEMENT?
--   homology, NormalToricVariety
--   operational Chow rings
--   linear series
--   isSemiprojective
--   toric maps
--     pullback divisors
--     pushforward divisors
--     birational map with makeSimplicial & makeSmooth

------------------------------------------------------------------------------
-- DOCUMENTATION
------------------------------------------------------------------------------
beginDocumentation ()    
load "NormalToricVarieties/ToricVarietiesDocumentation.m2"
load "NormalToricVarieties/DivisorsDocumentation.m2"
load "NormalToricVarieties/SheavesDocumentation.m2"
load "NormalToricVarieties/ToricMapsDocumentation.m2"
load "NormalToricVarieties/ChowDocumentation.m2"

------------------------------------------------------------------------------
-- TESTS
------------------------------------------------------------------------------
load "NormalToricVarieties/Tests.m2"

end---------------------------------------------------------------------------     

------------------------------------------------------------------------------
-- SCRATCH SPACE
------------------------------------------------------------------------------

-- XXX
uninstallPackage "NormalToricVarieties";
restart
installPackage "NormalToricVarieties"
check NormalToricVarieties

needsPackage "NormalToricVarieties";




------------------------------------------------------------------------------
-- interesting test but it is currently to slow to be on the basic list.
TEST ///
rayList = {{1,0,0,0,0,0},{0,1,0,0,0,0},{0,0,1,0,0,0},{0,0,0,1,0,0},{0,0,0,0,1,0},
  {0,0,0,0,0,1},{-1,-1,-1,-1,-1,-1},{1,1,1,0,0,0},{1,0,0,1,1,0},
  {0,-1,-1,-1,-1,0},{0,1,0,1,0,1},{0,0,1,0,1,1},{-1,-1,0,0,-1,-1},
  {-1,0,-1,-1,0,-1}};
coneList = {{0,1,3,6,8,9,10,13},{0,1,3,6,8,12,13},{0,1,3,6,9,10,12},{0,1,3,7,8,10},
  {0,1,3,7,8,12},{0,1,3,7,10,12},{0,1,4,5,8,11},{0,1,4,5,8,13},{0,1,4,5,11,13},
  {0,1,4,7,8,11},{0,1,4,7,8,13},{0,1,4,7,11,13},{0,1,5,7,8,10,11},
  {0,1,5,7,9,11,13},{0,1,5,7,9,12},{0,1,5,7,10,12},{0,1,5,8,9,10,13},
  {0,1,5,9,10,12},{0,1,6,7,9,12},{0,1,6,7,9,13},{0,1,6,7,12,13},{0,1,7,8,12,13},
  {0,2,3,5,7,8,10,11},{0,2,3,5,7,10,12},{0,2,3,5,8,11,12},{0,2,3,7,8,12},
  {0,2,4,7,8,11},{0,2,4,7,8,12,13},{0,2,4,7,11,13},{0,2,4,8,11,12},
  {0,2,4,11,12,13},{0,2,5,7,9,11},{0,2,5,7,9,12},{0,2,5,9,11,12},{0,2,6,7,9,11,13},
  {0,2,6,7,9,12},{0,2,6,7,12,13},{0,2,6,9,11,12},{0,2,6,11,12,13},
  {0,3,5,8,9,10},{0,3,5,8,9,12},{0,3,5,9,10,12},{0,3,6,8,9,12},{0,4,5,8,9,11},
  {0,4,5,8,9,13},{0,4,5,9,11,13},{0,4,6,8,9,11,12},{0,4,6,8,9,13},
  {0,4,6,8,12,13},{0,4,6,9,11,13},{0,4,6,11,12,13},{0,5,8,9,11,12},
  {1,2,3,7,8,10,11},{1,2,3,7,8,12},{1,2,3,7,10,12},{1,2,3,8,11,12},
  {1,2,3,10,11,12},{1,2,4,7,8,11},{1,2,4,7,8,12,13},{1,2,4,7,11,13},
  {1,2,4,8,11,12},{1,2,4,11,12,13},{1,2,5,6,7,9,11,13},{1,2,5,6,7,9,12},
  {1,2,5,6,11,12,13},{1,2,5,7,10,11},{1,2,5,7,10,12},{1,2,5,10,11,12},
  {1,2,6,7,12,13},{1,3,4,8,10,11},{1,3,4,8,10,13},{1,3,4,8,11,12},
  {1,3,4,8,12,13},{1,3,4,10,11,12,13},{1,3,6,10,12,13},{1,4,5,8,10,11},
  {1,4,5,8,10,13},{1,4,5,10,11,13},{1,5,6,9,10,12},{1,5,6,9,10,13},
  {1,5,6,10,12,13},{1,5,10,11,12,13},{2,3,5,10,11,12},{2,5,6,9,11,12},
  {3,4,5,8,10,11},{3,4,5,8,10,13},{3,4,5,8,11,12},{3,4,5,8,12,13},
  {3,4,5,10,11,12,13},{3,5,6,8,9,10,13},{3,5,6,8,9,12},{3,5,6,8,12,13},
  {3,5,6,9,10,12},{3,5,6,10,12,13},{4,5,6,8,9,11,12},{4,5,6,8,9,13},
  {4,5,6,8,12,13},{4,5,6,9,11,13},{4,5,6,11,12,13}};
X = normalToricVariety (rayList, coneList);
Y = makeSimplicial X;
debugLevel = 2;
assert (isWellDefined Y === true)
assert (isSimplicial Y === true)
assert (isProjective Y === true)
Y = makeSimplicial (X, Strategy => 0);
assert (isWellDefined Y === true)
assert (isSimplicial Y === true)
assert (isProjective Y === true)
///

