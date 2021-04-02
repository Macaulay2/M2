
#include "mixed-cells.h"

#include <iostream>
#include <fstream>
#include <string.h>
#include "cstdio"

// this should be changed next!!! 
#define TrueGen DoubleGen

using namespace std;
static bool debug;
static bool useNewAntiCyclingRule=false;
void outOfRange(int i, int n)
{
  cerr<<"Index out of range: index="<<i<<" vector length="<<n<<endl;
  assert(0);
}

/*
  List of classes:

  Classes for ring elements:
  DoubleGen
    Wrapper for 64bit double.
  ShortInt  
    Wrapper for machine integers.
  DoubleInt
    Fake integers using 64bit doubles. To be removed.

  LType typedef
  RType typedef

  Statistics
  Reducer
    ReducerExact
    LP template
    LPExact typedef
    Polytope
      Polytope as a list of vertices whose coordinates are machine integers
    Cone
      Polyhedral cone, where the right handside of the inequlalities/equations uses RType, and the left handside uses LTYPE

    Fan
    BitSet
    Table
    RelationTable
    RecursionData

    
Future classes:
  TrueGen
    
  TrueInt
    Arbitrary precision integer.



 */
#include "types.h"
#include "classes.h"

using namespace mixedCells;
using namespace std;
int main(int argc, char **argv)
{

  /*  GmpRational s(2,3);
  GmpRational t;
  t=s;
  std::cerr<<s;
  vector<GmpRational> v(2);
  return 0;
  */
  std::cerr<<"The code does not work at the moment because we do not call the constructor for gmp numbers when constructing a matrix.";
    rand();
    useNewAntiCyclingRule=false;

    char *fileName=0;
    if(argc!=2)
      {
	cerr<<"Specify DEMiCs file name as first argument."<<endl;
	return 1;
      }
    fileName=argv[1];
    ifstream in(fileName);
    if(!in)
      {
	cerr<<"Could not open file "<<fileName<<endl;
	return 1;
      }
  int ambientDimension;
  vector<Polytope> polytopes=readPolytopes(in,ambientDimension);
  vector<Fan> fans;//(polytopes.size());
  for(int i=0;i<polytopes.size();i++)
    {
      fans.push_back(Fan::fromPolytope(polytopes[i]));
    }

  //for(int i=0;i<fans.size();i++)cerr<<fans[i];
  int numberOfRemovedDimensions=0;
  fans=reduceDimension(ambientDimension+1,fans,numberOfRemovedDimensions);
  //cerr<<"-----------------------------------------"<<endl;
  //for(int i=0;i<fans.size();i++)cout<<fans[i];
  
  //  Cone fullSpace(ambientDimension+1-numberOfRemovedDimensions,MatrixDouble(0,ambientDimension+1-numberOfRemovedDimensions),MatrixDouble(0,ambientDimension+1-numberOfRemovedDimensions));
  Cone fullSpace(ambientDimension+1-numberOfRemovedDimensions,Matrix<LType>(0,ambientDimension-numberOfRemovedDimensions),Vector<RType>(0),Matrix<LType>(0,ambientDimension-numberOfRemovedDimensions),Vector<RType>(0));


  RecursionData recursionData(ambientDimension+1-numberOfRemovedDimensions,fans);
  //recursionData.table.intersectTriviallyInIntersection(0,0,0,0);
  //return 0;
  recursionData.completeTable();

   mixedCells::allocator.init(1000000);
   mixedCells::allocator.setStackMode(1);
  cout<<"MixedVolume:"<<recursionData.rek(0,fullSpace)<<endl;
  //  cerr<<recursionData.table;
   mixedCells::allocator.deinit();

  cerr<<statistics;
  return 0;
}

