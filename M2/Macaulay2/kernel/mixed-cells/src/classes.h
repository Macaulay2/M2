#include "shortrationals.h"
#include "gmprationals.h"
namespace mixedCells
{
  typedef GmpRational LType;
  //typedef ShortRat LType;
  //typedef DoubleInt LType;
  //typedef ShortInt LType;
  //typedef double LType;
  //  typedef DoubleGen RType;
  typedef RatGen<LType> RType;
  //typedef double RType;

  
  void normalizeRowPair(Matrix<LType> &AL, int j, Vector<RType> &R)
  {
    Matrix<LType>::RowRef row = AL[j];
    LType g(0);
    R[j].assignGCD(g);
    for (int i=0; isOne(g) && i<AL.getWidth(); i++)
      g = gcd(g,row[i]);
    if (isZero(g)) return;
    for (int i=0; i<AL.getWidth(); i++)
      row[i] /= g;
    R[j] /= g;
  }

  void removeZeroRowsPair(Matrix<LType> &AL, Vector<RType> &AR)
{
  int n=0;
  for(int i=0;i<AL.getHeight();i++)
    {
      bool isZer=isZero(AR[i]);
      for(int j=0;j<AL.getWidth();j++)if(!isZero(AL[i][j]))isZer=false;
      if(!isZer)n++;
    }
  Matrix<LType> retL(n,AL.getWidth());
  Vector<RType> retR(n);
  n=0;
  for(int i=0;i<AL.getHeight();i++)
    {
      bool isZer=isZero(AR[i]);
      for(int j=0;j<AL.getWidth();j++)if(!isZero(AL[i][j]))isZer=false;
      if(!isZer){retR[n]=AR[i];retL[n++].set(AL[i].toVector());}
    }
  AL=retL;
  AR=retR;
}

int reducePair(Matrix<LType> &L, Vector<RType> &R, bool returnIfZeroDeterminant)
{
  //  if(width<=1)cerr<<height<<"x"<<width<<endl;
  int retSwaps=0;
  int currentRow=0;
  
  for(int i=0;i<L.getWidth();i++)
    {
      int s=L.findRowIndex(i,currentRow);
      
      if(s!=-1)
	{
	  if(s!=currentRow)
	    {
	      L.swapRows(currentRow,s);
	      {
		RType temp=R[s];
		R[s]=R[currentRow];
		R[currentRow]=temp;
	      }
	      retSwaps++;
	    }
	  if (LType::isField()) {
	    for(int j=currentRow+1;j<L.getHeight();j++)
	      {
		LType s = -L[j][i]/L[currentRow][i];
		L.multiplyAndAddRow(currentRow,s,j);
		R[j]+=s*R[currentRow];
	      }
	  } else {
	    for(int j=currentRow+1;j<L.getHeight();j++)
	      if (!isZero(L[j][i])) {
		//cerr << L[j].toVector() << endl;
		LType g = gcd(L[j][i],L[currentRow][i]);
		LType b = L[j][i]/g;
		LType a = L[currentRow][i]/g;
		L.replaceWithLinearCombination(j,a,currentRow,-b,j);
		R[j] = a*R[j]-b*R[currentRow];
		//cerr << "\n  a = " << a << ", b = " << b << " gcd = " << g << "\n " << (L[j][i]) << "\n";
	      }
	  };
	  currentRow++;
	}
      else
	if(returnIfZeroDeterminant)return -1;
    }

  return retSwaps;
}
  void normalFormPair(Vector<LType> vL, RType vR, Vector<LType> &vDestL, RType &vDestR, Matrix<LType> const &AL, Vector<RType> const &AR)//assume reduced
{
  int pivotI=-1;
  int pivotJ=-1;
  int nonpivots=vL.size();
  while(AL.nextPivot(pivotI,pivotJ))
    {
      nonpivots--;
      //      v-=(v[pivotJ]/(*this)[pivotI][pivotJ])*(*this)[pivotI].toVector();
      LType s=vL[pivotJ]/AL[pivotI][pivotJ];
      vL-=s*AL[pivotI].toVector();
      vR-=s*AR[pivotI];
    }
  vDestL=Vector<LType>(nonpivots);
  vDestR=0;
  pivotI=-1;
  pivotJ=-1;
  int i=0;
  int last=-1;
  while(AL.nextPivot(pivotI,pivotJ))
    {
      while(pivotJ-1>last)
	{
	  vDestL[i++]=vL[++last];
	  //	    cerr<<"("<<(i-1)<<","<<last<<")";
	}
      last=pivotJ;
    }
  last++;
  while(last<AL.getWidth())
    vDestL[i++]=vL[last++];
  vDestR=vR;
  //if(debug)cerr<<v<<":"<<ret<<endl;
  assert(i==nonpivots);
  //  return ret;
}


void normalFormPairs(Matrix<LType> const &mL, Vector<RType> const &mR, Matrix<LType> &retL, Vector<RType> &retR, Matrix<LType> const &AL, Vector<RType> const &AR )//assume reduced
{
  Matrix<LType> tempretL=Matrix<LType>(mL.getHeight(),AL.getWidth()-AL.numberOfPivots());
  Vector<RType> tempretR=Vector<RType>(mL.getHeight());
  for(int i=0;i<mL.getHeight();i++)
    {
      Vector<LType> temp;
      normalFormPair(mL[i].toVector(),mR[i],temp , tempretR[i], AL, AR);
      tempretL[i].set(temp);
    }
  //    ret[i].set(normalForm(m[i].toVector()));
  //return ret;
  retL=tempretL;
  retR=tempretR;
}

}

std::ostream& operator<<(std::ostream& s, const IntegerVector &v)
{
  int minimalFieldWidth=2;
  s<<"(";
  for(int i=0;i<v.size();i++)
    {
      if(i!=0)s<<",";
      s<<v[i];
    }
  s<<")";
  return s;
}

namespace mixedCells
{
  class Statistics
  {
  public:
    int nLPs;
    int nRekCalls;
    int nCells;
    int nFeasible;
    int forFree;
    int nMTKNodes;
    int nLPRunNodes;
    Statistics():
      nLPs(0),
      nRekCalls(0),
      nCells(0),
      nFeasible(0),
      forFree(0),
      nMTKNodes(1),
      nLPRunNodes(0)
    {
    }
 
    friend std::ostream& operator<<(std::ostream& s, const Statistics &S)
    {
      s<<"Number of LPs solved:"<<S.nLPs<<endl;
      s<<"Number of recursive calls:"<<S.nRekCalls<<endl;
      s<<"Number of cells found:"<<S.nCells<<endl;
      s<<"Number of feasible LPs:"<<S.nFeasible<<endl;
      s<<"Number of combinatorially deduced infeasibilities:"<<S.forFree<<endl;
      s<<"Number of MizutaniTekedaKojima nodes:"<<S.nMTKNodes<<endl;
      s<<"Number of LP run nodes:"<<S.nLPRunNodes<<endl;
    }
  };
  Statistics statistics;



  /**
     The Reducer class represents a d-dimensional linear subspace L of
     (typL)^n\times typR together with a description of how to compute
     the normal form of a vector modulo L with (n-d)+1 entries.

     One important feature of Reducer is that it allows pushing and
     popping generators for L efficiently - that is it allows to
     increase or decrease the dimension of L.

     The linear subspace is represented by a reduced matrix [mL|mR].
     In order not to change the complete matrix when pushing or
     popping a generator the ordering of the coordinates with respect
     to which this matrix is reduced is changed dynamically. For this
     to work the pivotIndices and isPivot vectors are needed.

     It is not possible to push a vector which forces the last column
     to have a pivot (the push call will "fail"), see push(). The last
     column will never have a pivot.
   */  
  template<class typL, class typR> class Reducer
{
  /**
     This is the left hand side of the matrix representing the
     subspace L. Notice that only the first d rows are used.
   */
  Matrix<typL> mL;
  /**
     This column vector is the right-most column of the matrix
     representing the subspace L. Notice that only the first d entries
     are used.
   */
  Vector<typR> mR;
  Vector<typL> tempL;
  typR tempR;
  Vector<typL> temp2L;
  typR temp2R;
  /**
     The ith entry of this vector contains the index of the pivot in
     the ith row. This vector has length n at initialization but only
     the first d entries are meaningful.
   */
  IntegerVector pivotIndices;
  /**
     The ith entry of this matrix tells whether the ith column of
     [mL] (restricted to the first d rows) contains a pivot.
   */
  IntegerVector isPivot;
  /**
     The dimension of the ambient space of L is n+1.
   */
  int n;
  /**
     The dimension of the subspace L.
   */
  int d;
 public:
  Reducer(int n_):
    n(n_),
    d(0),
    mL(n_,n_),
    mR(n_,1),
    pivotIndices(n_),
    isPivot(n_),
    tempL(n_),
    temp2L(n_)
  {
  }
  /**
     Add the line generated by (vL,vR) to the subspace. Notice that if
     this makes the standard basis vector e_{n+1} go into the L, then
     the push is not performed and false is returned. Also if the line
     was already contained in L no push is performed and the return
     value is false. In the first case we get an inconsistency for our
     LPs since the last coordinate of an element in the kernel is
     forced to be zero. The latter case never happens in our
     application since the dimension is supposed to drop by one every
     time because of the generic lifting.?????? Is this really true?
   */
  bool push(Vector<typL> const &vL, typR const &vR)
  {
    mL[d].set(vL);
    mR[d]=vR;
    for(int i=0;i<d;i++)
      {
	mR[d]+=-mL[d][pivotIndices[i]]*mR[i];
	mL.multiplyAndAddRow(i,-mL[d][pivotIndices[i]],d);
      }
    int pivot=-1;
    for(int j=0;j<n;j++)if(!isZero(mL[d][j])){pivot=j;break;}
    if(pivot==-1) 
      {
	if(isZero(mR[d])) { // mR[d] == 0 means non-generic
	  assert(false);
	  /** implement measures treating the non-generic case
	   One strategy: 
           -- (relation=equation)
           -- (generate heights with full precision; do not store)
	   -- build relations and inequalities (contained in cones) 
	      so that they have full-precision RHS
	   -- relations should store the list (more like a pair) of _involved heights_
	   -- reducers (mL,mR) should remember a reference to the original relation 
	   -- Reducer stores precision for _every_ height
	   -- pop() should restore the precision of every height 
	      to the original precision of the parent node. (How?)
	   */
	}
	return false;
      }
    if (typL::isField()) {
      typL mult = 1/mL[d][pivot];
      mR[d] = mult*mR[d];
      mL.scaleRow(d,mult);
    } else {
      normalizeRowPair(mL,d,mR);
    }
    pivotIndices[d]=pivot;
    isPivot[pivot]=true;
    d++;
    return true;
  }
  /**
     Pop the latest added generator to L.
   */
  void pop()
  {
    d--;
    isPivot[pivotIndices[d]]=false;
  }
  friend std::ostream& operator<<(std::ostream& s, Reducer const &r)
  {
    s<<"Reducer:"<<endl;
    s<<r.mL.submatrix(0,0,r.d,r.mL.getWidth());
    s<<r.mR.subvector(0,r.d);
    return s;
  }
  /**
     This method transforms the inequalities (rows of the matrix
     [originalL|originalR]) to their normal forms modulo L by reducing
     with the matrix representation of L. Only coordinates with no
     pivots have to be stored. That is, n-d for the left handside, and
     1 for the right hand side. In total d coordinates are removed.

     The output is stored in [LeftHandSide|RightHandSide], and these
     two matrices (/vector) must have been initialized to the right
     width, and with a sufficient number of rows before calling.

     If the normal form of an inequality turns out to be zero, then it
     is not stored. The total number of stored inequalities is the
     return value of the method.
   */
  int storeAndSplitNormalForms(Matrix<typL> const &originalL, Vector<typR> const &originalR, Matrix<typL> &LeftHandSide, Vector<typR> &RightHandSide)
  {
    int ret=0;
    assert(originalL.getWidth()==n);
    for(int i=0;i<originalL.getHeight();i++)
      {
	typR tempR=originalR[i];
	for(int j=0;j<n;j++)
	  {
	    tempL[j]=originalL[i][j];
	  }
	for(int k=0;k<d;k++)
	  {
	    tempR+=-tempL[pivotIndices[k]]*mR[k];
	    mL.maddRowToVector(k,-tempL[pivotIndices[k]],tempL);
	  }
	if(!(tempL.isZero()&&isZero(tempR)))
	  {
	    RightHandSide[ret]=-tempR;
	    int J=0;
	    for(int j=0;j<n;j++)if(!isPivot[j])LeftHandSide[ret][J++]=tempL[j];
	    ret++;
	  }
      }
    return ret;
  }
  /**
     This method transforms the inequalities (rows of the matrix
     [sourceL|sourceR]) to their normal forms modulo L by reducing
     with the matrix representation of L. Only coordinates with no pivots have
     to be stored. Therefore, the input has n+1 coordinates for the
     left handside, and 1 for the right hand side. The output has only
     n-d coordinates for the left handside.

     The output is stored in [destinationL|destinationR], but
     starting at row destinationOffset. The two matrices (/vector)
     must have been initialized to the right width, and with a
     sufficient number of rows before calling.

     If the normal form of an inequality turns out to be zero, then it
     is not stored. The total number of stored inequalities is the
     return value of the method unless if one of the inequalities is
     inconsistent. In this case a -1 is returned. FIX DOCUMENTATION
   */
  int reduction(Matrix<typL> const &sourceL, Vector<typR> const sourceR, Matrix<typL> &destinationL, Vector<typR> &destinationR, int destinationOffset)
  {
    /*    cerr<<"-----------------------"<<endl;
    cerr<<*this;
    cerr<<"Source"<<sourceL<<sourceR;
    cerr<<"Destination"<<destinationL<<destinationR;
    cerr<<"Destination offset"<<destinationOffset<<endl;
    */
    int ret=0;
    assert(sourceL.getWidth()==n);
    //    assert(tempL.size()==n);//??
    for(int i=0;i<sourceL.getHeight();i++)
      {
	typR tempR=sourceR[i];
	for(int j=0;j<n;j++)tempL[j]=sourceL[i][j];
	//cerr<<"temp"<<temp;
	for(int k=0;k<d;k++)
	  {
	    tempR+=(-(tempL[pivotIndices[k]]))*mR[k];
	    mL.maddRowToVector(k,-(tempL[pivotIndices[k]]),tempL);
	  }
	//cerr<<"temp"<<temp;
	
	bool leftHandSideZero=true;
	for(int j=0;j<n;j++)if(!isZero(tempL[j])){leftHandSideZero=false;break;}
	if(leftHandSideZero)
	  {
	    if(isNegative(tempR))return -1;//infeasible
	  }
	else
	  {
	    int J=0;
	    Matrix<LType>::RowRef dest=destinationL[destinationOffset+ret];
	    //	    Matrix<double>::RowRef dest=destinationL[destinationOffset+ret];
	    //	    Matrix<typL>::RowRef dest=destinationL[destinationOffset+ret];
	    for(int j=0;j<n;j++)if(!isPivot[j])dest[J++]=tempL[j];
	    destinationR[destinationOffset+ret]=tempR;
	    ret++;
	  }
      }
    //cerr<<"Destination"<<destinationL<<destinationR;
    return ret;
  }
#define HASH 1
#if HASH
  //  static unsigned char hashTable[256];
 mutable unsigned char hashTable[256];
#endif
  /**
     This method transforms the inequalities (rows of the matrix
     [sourceL|sourceR]) to their normal forms modulo L by reducing
     with the matrix representation of L, under the assumption that
     the equations were already reduced by the first d-1 rows of the
     matrix representation of L. Only coordinates with no pivots have
     to be stored. Therefore, the input has n-d+1 coordinates for the
     left handside, and 1 for the right hand side. The output has
     only n-d coordinates for the left handside.

     The output is stored in [destinationL|destinationR], but
     starting at row destinationOffset. The two matrices (/vector)
     must have been initialized to the right width, and with a
     sufficient number of rows before calling.

     If the normal form of an inequality turns out to be zero, then it
     is not stored. The total number of stored inequalities is the
     return value of the method unless if one of the inequalities is
     inconsistent. In this case a -1 is returned. FIX DOCUMENTATION
   */
  int singleReduction(Matrix<typL> const &sourceL, Vector<typR> const &sourceR, int numberOfUsedRowsInSource, Matrix<typL> &destinationL, Vector<typR> &destinationR)
  {
    /*    cerr<<"------------++++++++++-----------"<<endl;
    cerr<<*this;
    cerr<<"Source"<<source;
    cerr<<"Destination"<<destination;
    cerr<<"NumberOfUsedRowsInSource"<<numberOfUsedRowsInSource<<endl;
    */
    //    cerr<<"n"<<n<<"d"<<d<<"dw"<<destination.getWidth()<<"sw"<<source.getWidth();
    assert(d>0);
    assert(destinationL.getWidth()==n-d);
    assert(sourceL.getWidth()==n-d+1);

#if HASH
    memset(hashTable,255,256);
#endif

    if(pivotIndices[d-1]==n)//if we have a pivot on the right hand side then the system is already infeassible and there is no reason to add????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????
      {
	return 0;
      }

    int numberOfPivotsBeforeCurrent=0;
    for(int j=0;j<pivotIndices[d-1];j++)if(isPivot[j])numberOfPivotsBeforeCurrent++;
    int J=0;
    for(int j=0;j<n;j++)if((!isPivot[j])&&(j!=pivotIndices[d-1])){temp2L[J++]=mL[d-1][j];}
    temp2R=mR[d-1];
    int newPivotIndex=pivotIndices[d-1]-numberOfPivotsBeforeCurrent;

    int ret=0;
    for(int i=0;i<numberOfUsedRowsInSource;i++)
      {
    typL scalar=sourceL[i][newPivotIndex];



    if(1) // change to optimize code below
      {
	for(int j=0;j<newPivotIndex;j++)
	  destinationL[ret][j]=sourceL[i][j]-scalar*temp2L[j];
	for(int j=newPivotIndex;j<n-d;j++)
	  destinationL[ret][j]=sourceL[i][j+1]-scalar*temp2L[j];
	destinationR[ret]=sourceR[i]-scalar*temp2R;
      }
#if 0
    else /* Here are various alternatives */
      {
	MatrixDouble::RowRef dest=destination[ret];
	MatrixDouble::const_RowRef src=source[i];

	typ * __restrict destB=&(dest[0]);
	const typ *srcB=&(src[0]);
	typ *temp2B=&(temp2[0]);

	/*for(signed long j=0;j<newPivotIndex;j++)
	  destB[j]=srcB[j]-scalar* temp2B[j];
	srcB++;

	destB+=(n-d+1);
	srcB+=(n-d+1);
	temp2B+=(n-d+1);

	for(signed long j=-(n-d+1)+newPivotIndex;j<0;j++)
	  destB[j]=srcB[j]-scalar* temp2B[j];
*/	
	/*		destB+=newPivotIndex;
	srcB+=newPivotIndex;
	temp2B+=newPivotIndex;
	for(signed long j=-newPivotIndex;j<0;j++)
	  destB[j]=srcB[j]-scalar* temp2B[j];
	srcB++;

	destB+=(n-d+1)-newPivotIndex;
	srcB+=(n-d+1)-newPivotIndex;
	temp2B+=(n-d+1)-newPivotIndex;

	for(signed long j=-(n-d+1)+newPivotIndex;j<0;j++)
	  destB[j]=srcB[j]-scalar* temp2B[j];
	*/
	/*for(int j=0;j<newPivotIndex;j++)
	  destB[j]=srcB[j]-scalar* temp2B[j];
	srcB++;

	for(int j=newPivotIndex;j<n-d+1;j++)
	  destB[j]=srcB[j]-scalar* temp2B[j];
	*/
	for(int j=newPivotIndex;j>0;j--)
	  *(destB++)=*(srcB++)-scalar* *(temp2B++);
	srcB++;
	for(int j=n-d+1-newPivotIndex;j>0;j--)
	*(destB++)=*(srcB++)-scalar* *(temp2B++);
	/*	for(int j=0;j<newPivotIndex;j++)
	  destB[j]=srcB[j]-scalar*temp2B[j];
	for(int j=newPivotIndex;j<n-d+1;j++)
	  destB[j]=srcB[j+1]-scalar*temp2B[j];	
	*/
      }
#endif
    //	bool leftHandSideZero=true;
    //	for(int j=0;j<n-d;j++)if(!m.isZero(destination[ret][j])){leftHandSideZero=false;break;}
    assert(destinationL.getWidth()==n-d);
    bool leftHandSideZero=destinationL[ret].isZero();
	if(leftHandSideZero)
	  {
	    if(isNegative(destinationR[ret]/*[n-d]*/))return -1;//infeasible
	  }
	else
	  {
#if HASH
	    assert(destinationL.getWidth()==n-d);
	    unsigned char h=destinationL.hashValue(ret,n-d);
	    //cerr<<(int)h<<endl;
	    if((hashTable[h]!=255)&&destinationL.rowsAreEqual(ret,hashTable[h],n-d))
	      {
		if(destinationR[ret]<destinationR[hashTable[h]])destinationR[hashTable[h]]=destinationR[ret];
		ret--;
	      }
	    else
	      hashTable[h]=ret;
#endif
	    ret++;
	  }
      }
    //cerr<<"Destination"<<destination;
    return ret;
  }
  int newAffineDimension()const
  {
    return n-d;
  }
  /**
     This method is given inequalities of the form [mL]*x<=[mR]
     and checks if there are some obvious inconsistencies. Only the
     first usedRows are taken into consideration. Two rows are an
     "obvious" inconsistency if they contradict each other. Since a
     complete check of obvious inconsistence takes time
     usedRows*log(usedRows), we do not make a complete check, but
     rather use a hashtable to match up rows of mL which are the same
     except sign.

     Return value true means that an inconsistency was found.

     The function is a method of the Reducer class even though n, d
     and hashTable are the only class members used.
   */
  bool hashedInconsistencyLookup(Matrix<typL> const &mL, Vector<typR> const &mR, int usedRows)const
  {
    int nMinusD=n-d;
    assert(mL.getWidth()==nMinusD);
    memset(hashTable,255,256);
    for(int i=0;i<usedRows;i++)
      {
	unsigned char h=mL.hashValue(i,nMinusD);
	hashTable[h]=i;
      }
    for(int i=0;i<usedRows;i++)
      {
	unsigned char h=mL.hashValueOfNegative(i,nMinusD);
	if(hashTable[h]!=255)
	  {
	    Vector<typL> sumL=mL[i].toVector();
	    sumL+=mL[hashTable[h]].toVector();
	    if(sumL.isZero())if(isNegative(mR[i]+mR[hashTable[h]]))return true;
	  }
      }
    return false;
  }
};



  typedef Reducer<LType,RType> ReducerExact;



#if HASH
  //    unsigned char ReducerExact::hashTable[256];
#endif
}

/*
  Simplex algorithm for lp of the form
  max <w,y> subject to
  yA=c
  y>=0
 */
namespace mixedCells
{
  template <class typL,class typR> class LP
  {
  public:    int d,n;
    Matrix<typL> const &A;
    Matrix<typL> Ainv;
    Vector<typR> Ainvw;
    Vector<typL> c;
    Vector<typR> w;
    Vector<typL> yValues;
    Vector<typL> edgeCandidateValues;
    int edgeCandidateOneEntry;
    vector<int> basis;
    vector<bool> inBasis;

    void updateCandidateEdge(int i)
    {
      edgeCandidateOneEntry=i;
      // cerr << "Ainv " << Ainv;
      // cerr << "A " << A;
      for(int j=0;j<basis.size();j++)
	edgeCandidateValues[j]=-A.rowDotColumnOfOther(i,Ainv,j);
    }
    bool isImprovingDirection(int i)//const//i is non-basis
    {
      //      updateCandidateEdge(i);
      typR d=w[i];
      //      for(int j=0;j<basis.size();j++)d+=edgeCandidateValues[j]*w[basis[j]];
      for(int j=0;j<Ainvw.size();j++)d-=A[i][j]*Ainvw[j];
      if(debug)cerr<<"EDGE"<<edgeCandidateValues<<"Oneidex:"<<edgeCandidateOneEntry<<"d"<<d<<endl;
      //      return dot(v,w)>0;
      if(isPositive(d))return true;
      if(isNegative(d))return false;
      /*      for(int i=0;i<v.size();i++)
	{
	  if(v[i]<-0.000001)return true;
	  if(v[i]>0.000001)return false;
	  }*/
	    return false;
	    //      return dot(v,w)>0;//-0.0001; //FUDGE FACTOR
    }
    typR improvement(/*int i,*/ int &newNonBasisMemberIndex)
    {
      //updateCandidateEdge(i);
      typR ew=w[edgeCandidateOneEntry];
      for(int j=0;j<basis.size();j++)ew+=edgeCandidateValues[j]*w[basis[j]];
      typR ret;
      bool first=true;
      newNonBasisMemberIndex=-1;

      // This is the new anti-cycling rule
      for(int s=0;s<basis.size();s++)
	if(isNegative(edgeCandidateValues[s]))
	  {
	    if(first)
	      {
		newNonBasisMemberIndex=s;
		first=false;
	      }
	    else
	      {
		bool isBetter=true;
		//		  if(-yValues[s]/edgeCandidateValues[s]+EPSILON<-yValues[newNonBasisMemberIndex]/edgeCandidateValues[newNonBasisMemberIndex])goto isBetter;
		if(isEpsilonLessThan(-yValues[s]/edgeCandidateValues[s],-yValues[newNonBasisMemberIndex]/edgeCandidateValues[newNonBasisMemberIndex]))goto isBetter;
		if(isEpsilonLessThan(-yValues[newNonBasisMemberIndex]/edgeCandidateValues[newNonBasisMemberIndex],-yValues[s]/edgeCandidateValues[s]))goto isNotBetter;
		for(int a=0;a<basis.size();a++)
		  {
		    
		    if(isEpsilonLessThan(-Ainv[a][s]/edgeCandidateValues[s],-Ainv[a][newNonBasisMemberIndex]/edgeCandidateValues[newNonBasisMemberIndex]))goto isBetter;
		    if(isEpsilonLessThan(-Ainv[a][newNonBasisMemberIndex]/edgeCandidateValues[newNonBasisMemberIndex],-Ainv[a][s]/edgeCandidateValues[s]))goto isNotBetter;
		  }
		assert(0);
		isNotBetter:		    
		isBetter=false;
	      isBetter:
		if(isBetter)newNonBasisMemberIndex=s;
	      }
	  }
      if(newNonBasisMemberIndex!=-1)
	return -(yValues[newNonBasisMemberIndex])/edgeCandidateValues[newNonBasisMemberIndex]*ew;
      return 0;
    }
  public:
    /**
       The matrix A_ must be kept alive through out the life of the LP.
     */
    LP(Matrix<typL> const &A_, Vector<typL> const &c_):
      A(A_),
      c(c_),
      w(A_.getHeight(),false),
      Ainv(A_.getWidth(),A_.getWidth()),
      yValues(A_.getWidth(),false),
      edgeCandidateValues(A_.getWidth(),false),
      Ainvw(A_.getWidth(),false)
    {
      d=A.getHeight();
      n=A.getWidth();
      assert(n==c.size());
    }
    void setObjectiveFunction(Vector<typR> const &w_)
    {
      assert(w.size()==w_.size());
      w=w_;
    }
    void setCurrentBasis(vector<int> const &basis_)
    {
      basis=basis_;
      inBasis=vector<bool>(d);
      for(int i=0;i<inBasis.size();i++)inBasis[i]=false;
      for(int i=0;i<basis_.size();i++)
	inBasis[basis[i]]=true;
      //????
    }
    /*    LP buildLPForFeasibilityCheck()
    {
      LP ret=LP(combineOnTop(A,Matrix::identity(n)),c);
      vector<int> basis;
      for(int i=0;i<n;i++)basis.push_back(i+d);
      ret.setCurrentBasis(basis);
      Vector w(n+d);
      //      for(int i=0;i<n;i++)w[d+i]=-1;
      for(int i=0;i<n;i++)w[d+i]=-1;
      ret.setObjectiveFunction(w);
      ret.Ainv=Matrix::identity(n);
      ret.y=concatenation(Vector(d),c);

      for(int i=0;i<n;i++)
	if(c[i]<0)
	  {
	    ret.y[d+i]*=-1;
	    ret.A[A.getHeight()+i][i]=-1;
	    ret.Ainv[i][i]=-1;
	  }

      return ret;
      }*/
    friend std::ostream& operator<<(std::ostream& s, LP &lp)
    {
      s<<"LP problem:"<<endl;
      s<<"A="<<lp.A<<endl;
      
      s<<"c="<<lp.c;
      s<<"w="<<lp.w<<endl;
      s<<"yValues="<<lp.yValues<<endl;
      /*      {
	Matrix ym(1,lp.y.size());ym[0].set(lp.y);
	s<<"yA="<<ym*lp.A<<endl;
	}*/
      s<<"basis={";
      for(vector<int>::const_iterator i=lp.basis.begin();i!=lp.basis.end();i++)
	{
	  if(i!=lp.basis.begin())s<<",";
	  s<<*i;
	}
      s<<"}"<<endl;
      s<<"Ainv="<<lp.Ainv<<endl;
    }
    void updateAinvw()
    {
      Ainvw.clear();
      for(int j=0;j<basis.size();j++)
	for(int k=0;k<Ainv.getHeight();k++)
	  Ainvw[k]+=Ainv[k][j]*w[basis[j]];
    }
    int step()
    {
      //      cerr<<A.getHeight()<<"x"<<A.getWidth()<<endl;
      typR impBest=-EPSILON;//FUDGE FACTOR
      int bestIndex=-1;
      int bestNewNonElementIndex=-1;
      for(int i=0;i<d;i++)
	if(!inBasis[i])
	{
	  //cerr<<"----"<<i<<"is not in Basis"<<endl;
	  if(isImprovingDirection(i))
	    {
	      updateCandidateEdge(i);
	      int newNonBasisElementIndex;
	      typR imp=improvement(/*i,*/newNonBasisElementIndex);
	      if(debug)cerr<<"Improvement"<<imp<<"newnonbasisindex"<<newNonBasisElementIndex<<"\n";
	      if(newNonBasisElementIndex==-1){/*cerr<<"UNBOUNDED"<<endl;*/return -1;} //UNBOUNDED
	      //	      if((imp>=impBest))//needed for perturbation
	      if(isGreaterEqual(imp,impBest))//needed for perturbation
		{
		  impBest=imp;
		  bestIndex=i;
		  bestNewNonElementIndex=newNonBasisElementIndex;break;//<----not that it matters much, but we may return after the first improving direction is found.
		}
	    }
	}
      if(bestIndex==-1)
	return 0; //OPTIMAL
      
      updateCandidateEdge(bestIndex);

      typL scalar=-(yValues[bestNewNonElementIndex]/edgeCandidateValues[bestNewNonElementIndex]);
      yValues.madd(scalar,edgeCandidateValues);
      yValues[bestNewNonElementIndex]=scalar;
      //      assert(yValues[bestNewNonElementIndex]>-EPSILON);
      //cerr<<"Need to update Ainv according to edge direction"<<e;

      //cerr<<"Row "<<bestIndex<<"Can be expressed using"<<e<<endl;

      //cerr<<"|||||||scale"<<bestNewNonElementIndex<<"by"<<-e[basis[bestNewNonElementIndex]]/e[bestIndex];
      Ainv.scaleColumn(bestNewNonElementIndex,-1/edgeCandidateValues[bestNewNonElementIndex]);
      for(int i=0;i<basis.size();i++)
	if(i!=bestNewNonElementIndex)
	  {
	    Ainv.multiplyAndAddColumn(bestNewNonElementIndex,edgeCandidateValues[i]/1,i);
	    //cerr<<"Muladd"<<basis[i]<<"by"<<-e[basis[i]]/e[bestIndex]<<"to"<<bestNewNonElementIndex;
	  }
      //cerr<<endl;
      

      inBasis[basis[bestNewNonElementIndex]]=false;
      inBasis[bestIndex]=true;
      basis[bestNewNonElementIndex]=bestIndex;

      updateAinvw();

      /*      {
		Matrix Asub=Ainv;
	for(int i=0;i<Asub.getHeight();i++)
	  for(int j=0;j<Asub.getWidth();j++)
	    Asub[i][j]=A[basis[i]][j];
	
	cerr<<"Asubmatrix"<<Asub;
	cerr<<"Ainv"<<Ainv;
	cerr<<"Prod"<<Asub*Ainv;
	}*/
      //      y[basis[bestNewNonElementIndex]]=0;

      return 1;
    }
    /*    bool findFeasibleBasis()
    {
      LP A2=buildLPForFeasibilityCheck();

      //      cerr<<A2;

      int status;
      do
	{

	  status=A2.step();
	  if(debug) cerr<<"-----------------\n"<<A2;
	}
      while(status==1);
      //      fprintf(stderr,status?"LP is unbounded.\n":"Optimal solution found.\n");

      vector<int> newBasis;
      for(vector<int>::const_iterator i=A2.basis.begin();i!=A2.basis.end();i++)
	{
	  if(*i<d)
	    {
	      newBasis.push_back(*i);
	      y[*i]=A2.y[*i];
	    }
	  else
	    if(A2.y[*i]>0.0000001)return false;///!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
	}

      //      if(newBasis.size()!=n)return false;!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      setCurrentBasis(newBasis);
      Ainv=A2.Ainv;
      return true;
    }
    */
    void chooseRightHandSideToMakeFeasibleSolution()
    {
      Matrix<typL> A2=A.transposed();
      c=Vector<typL> (A.getWidth());
      A2.reduce(false);
      basis=vector<int>();
      inBasis=vector<bool>(A.getHeight());
      yValues=Vector<typL>(A.getWidth());
      Matrix<typL> ASub(A.getWidth(),A.getWidth());
      int index=0;
      for(int i=0;i<inBasis.size();i++)inBasis[i]=false;
      {
	int pivotI=-1;
	int pivotJ=-1;
	while(A2.nextPivot(pivotI,pivotJ))
	  {
	    c+=A[pivotJ].toVector();
	    yValues[index]=1;
	    inBasis[pivotJ]=true;
	    basis.push_back(pivotJ);
	    ASub[index++].set(A[pivotJ].toVector());
	  }
	assert(Ainv.getHeight()==ASub.getHeight());
	assert(Ainv.getWidth()==ASub.getWidth());
	Ainv=ASub.inverse();
	assert(Ainv.getHeight()==ASub.getHeight());
	assert(Ainv.getWidth()==ASub.getWidth());
	updateAinvw();
      }
      if(index!=A.getWidth())
	{
	  cerr<<*this;
	  cerr<<"A2"<<A2<<endl;
	  assert(0);
	}
    }
  };

  typedef LP<LType,RType> LPExact;

  class Polytope
  {
  public:
    int n;
    vector<vector<int> > vertices;
    Polytope(int n_, vector<vector<int> > const &vertices_):
      n(n_),
      vertices(vertices_)
    {
    }
    int ambientDimension()const{return n;}
  };
  vector<int> readVector(istream &s, int length)
  {
    vector<int> ret(length);
    for(int i=0;i<length;i++)
      s>>ret[i];
    return ret;
  }
  vector <Polytope> readPolytopes(istream &s, int &dim)
  {
    string temp;
    int numberOfPolytopes;
    s>> temp>>temp>>dim;
    s>> temp>>temp>>numberOfPolytopes;

    assert(numberOfPolytopes>0);
    assert(numberOfPolytopes<10000);

    s>> temp>>temp;
    vector<int> elem=readVector(s,numberOfPolytopes);

    s>> temp>>temp;
    vector<int> type=readVector(s,numberOfPolytopes);

    //cerr<<"\""<<temp<<"\"\n";

    vector<Polytope> ret;
    for(int i=0;i<numberOfPolytopes;i++)
      {
	vector<vector<int> > vertices(elem[i]);
	
	for(int j=0;j<elem[i];j++)
	  vertices[j]=readVector(s,dim);

	ret.push_back(Polytope(dim,vertices));
      }
    return ret;
  }

  class Cone
  {
  public:
    int n;
    Matrix<LType> inequalitiesL;
    Vector<RType> inequalitiesR;
    Matrix<LType> equationsL;
    Vector<RType> equationsR;
    /*    Cone(int n_, MatrixDouble const &inequalities_, MatrixDouble const &equations_):
      n(n_),
      inequalitiesL(inequalities_.submatrix(0,0,inequalities_.getHeight(),n_-1)),
      inequalitiesR(inequalities_.getHeight()),
      equationsL(equations_.submatrix(0,0,equations_.getHeight(),n_-1)),
      equationsR(equations_.getHeight())
    {
      for(int i=0;i<inequalitiesR.size();i++)inequalitiesR[i]=inequalities_[i][n-1];
      for(int i=0;i<equationsR.size();i++)equationsR[i]=equations_[i][n-1];
      assert((n-1)==inequalitiesL.getWidth());
      assert((n-1)==equationsL.getWidth());
      }*/
    Cone(int n_, Matrix<LType> const &ineqL, Vector<RType> const &ineqR, Matrix<LType> const &eqL, Vector<RType> const &eqR):
      n(n_),
      inequalitiesL(ineqL),
      inequalitiesR(ineqR),
      equationsL(eqL),
      equationsR(eqR)
    {
    } 
    friend std::ostream &operator<<(std::ostream &out, Cone const &cone)
    {
      out<<"Printing Cone"<<endl;
      out<<"Ambient dimension:"<<cone.n<<endl;
      out<<"InequalitiesL:"<<endl<<cone.inequalitiesL;
      out<<"InequalitiesR:"<<endl<<cone.inequalitiesR;
      out<<"EquationsL:"<<endl<<cone.equationsL;
      out<<"EquationsR:"<<endl<<cone.equationsR;
      out<<"Done printing Cone"<<endl;
      return out;
    }
    /*    bool doesContain(VectorDouble const &v)const
    {
      for(int i=0;i<equations.getHeight();i++)
	if(dot(v,equations[i].toVector())>0.0001)return false;
      for(int i=0;i<equations.getHeight();i++)
	if(dot(v,equations[i].toVector())<-0.0001)return false;
      for(int i=0;i<inequalities.getHeight();i++)
	if(dot(v,inequalities[i].toVector())<-0.0001)return false;
      return true;
      }*/
    bool hasPointWithLastCoordinatePositiveInCone(Matrix<LType> &coneInequalitiesL, Vector<RType> &coneInequalitiesR, int oldNumberOfInequalities, int &newNumberOfInequalities, ReducerExact &reducer)
    {
      //cerr<<"----INCONE"<<endl;
      statistics.nLPs++;
      int numberOfAddedInequalities=reducer.reduction(inequalitiesL,inequalitiesR,coneInequalitiesL,coneInequalitiesR,oldNumberOfInequalities);
      //cerr<<"ADDED:"<<numberOfAddedInequalities<<endl;
      if(numberOfAddedInequalities<0)return false;

      newNumberOfInequalities=oldNumberOfInequalities+numberOfAddedInequalities;

      if(reducer.hashedInconsistencyLookup(coneInequalitiesL,coneInequalitiesR,newNumberOfInequalities)){/*cerr<<"A";*/return false;}/*else cerr<<"B";*/

      int newAffineDimension=reducer.newAffineDimension();
      Matrix<LType> Inequalities=coneInequalitiesL.submatrix(0,0,newNumberOfInequalities,newAffineDimension);
      Vector<RType>  RightHandSide=-coneInequalitiesR.subvector(0,newNumberOfInequalities);

      LPExact lp(Inequalities,Vector<LType>(Inequalities.getWidth()));
      lp.setObjectiveFunction(RightHandSide);
      lp.chooseRightHandSideToMakeFeasibleSolution();
      
      // cerr<<reducer;
      // cerr<<*this<<coneInequalitiesL<<coneInequalitiesR<<oldNumberOfInequalities<<newNumberOfInequalities<<endl;
      // cerr<<"INCONE"<<lp;
      int status;
      int loops=0;

      //      {static int i;i=(i+1)&1023;if(!i)cerr<<lp;}
      do
	{
	  if(loops++>10000)
	    {
	      cerr<<lp;
	      //debug=true;
	    }
	  status=lp.step();
	}
      while(status==1);
      //cerr<<"STATUS"<<status<<endl;//{static int p;assert(p++<9);}
      bool isFeasible=(status!=-1);
      if(isFeasible)statistics.nFeasible++;
      //squareMatrixAllocator.free();
      return isFeasible;
    }
    bool hasPointWithLastCoordinatePositive(ReducerExact *reducer=0)const
    {
      statistics.nLPs++;
      bool isFeasible;
      {
	Matrix<LType> Inequalities(0,0);
	Vector<RType> RightHandSide(0);

	if(reducer)
	  {
	    int numberOfInequalities=inequalitiesL.getHeight();
	    int newAffineDimension=reducer->newAffineDimension();
	    Inequalities=Matrix<LType>(numberOfInequalities,newAffineDimension);
	    RightHandSide=Vector<RType>(numberOfInequalities);
	    //int newNumberOfInequalities=reducer->storeAndSplitNormalForms(this->inequalitiesL,this->inequalitiesR,Inequalities,RightHandSide);
	    int newNumberOfInequalities=reducer->reduction(this->inequalitiesL,this->inequalitiesR,Inequalities,RightHandSide,0);
	    assert(newNumberOfInequalities>=0);
	    if(newNumberOfInequalities!=this->inequalitiesL.getHeight())
	      {
		Inequalities=Inequalities.submatrix(0,0,newNumberOfInequalities,Inequalities.getWidth());
		RightHandSide=RightHandSide.subvector(0,newNumberOfInequalities);
	      }
	  }
	else
	  {
	    Matrix<LType> equationsL=this->equationsL;
	    Vector<RType> equationsR=this->equationsR;
	    //	    equations.reduce(false);
	    reducePair(equationsL,equationsR,false);
	    //	    cerr << equationsL << endl << equationsR << endl; 
	    {
	      if(equationsL.numberOfPivots()!=equationsL.getHeight())return false;//THIS ONLY WORKS IF THERE ARE NO REPEATED EQUATIONS
	      /*	      int pivotI=-1;
	      int pivotJ=-1;
	      while(equations.nextPivot(pivotI,pivotJ))if(pivotJ==equations.getWidth()-1)return false;
	      */
	      int maxpivot=-1;
	      int pivotI=-1;
	      int pivotJ=-1;
	      while(equationsL.nextPivot(pivotI,pivotJ))if(maxpivot<pivotJ)maxpivot=pivotJ;
	      if((maxpivot+1)<equationsL.getHeight())if(!isZero(equationsR[maxpivot+1]))return false;	      
	    }
	    
	    /*	    MatrixDouble inequalities=equations.normalForms(this->inequalities);
	    inequalities.removeZeroRows();
	    
	    Inequalities=inequalities.submatrix(0,0,inequalities.getHeight(),inequalities.getWidth()-1);
	    RightHandSide=(-inequalities.submatrix(0,inequalities.getWidth()-1,inequalities.getHeight(),inequalities.getWidth())).transposed()[0].toVector();
	    */
	    // Here we compute normal forms of inequalities L+R, and store them in Inequalities and Righthandside
	    
	    normalFormPairs(inequalitiesL,inequalitiesR,Inequalities,RightHandSide,equationsL,equationsR);
	    removeZeroRowsPair(Inequalities,RightHandSide);
	    RightHandSide=-RightHandSide;
	  }

	
	//If inequalities do not span space, then we may restrict
	Inequalities=Inequalities.reduceDimension();
	for(int i=0;i<Inequalities.getHeight();i++)if(Inequalities[i].isZero())if(isPositive(RightHandSide[i]))return false;
	

	LPExact lp(Inequalities,Vector<LType>(Inequalities.getWidth()));
	lp.setObjectiveFunction(RightHandSide);
	lp.chooseRightHandSideToMakeFeasibleSolution();
	//	cerr<<lp;assert(0);
	int status;
	do
	  {
	    status=lp.step();
	  }
	while(status==1);
	isFeasible=(status!=-1);
	//squareMatrixAllocator.free();
      }
      if(isFeasible)statistics.nFeasible++;
      return isFeasible;
    }
    /*    void optimize()
    {
      for(int i=0;i<inequalitiesL.getHeight();i++)
	{
	  inequalities[i].set(-1*inequalities[i].toVector());
	  bool doRemove=!hasPointWithLastCoordinatePositive();
	  inequalities[i].set(-1*inequalities[i].toVector());
	  if(doRemove)
	    {
	      inequalities=combineOnTop(inequalities.submatrix(0,0,i,inequalities.getWidth()),inequalities.submatrix(i+1,0,inequalities.getHeight(),inequalities.getWidth()));
	      i--;
	    }
	}
	}*/
  };
  Cone intersection(Cone const &a, Cone const &b)
  {
    assert(a.n==b.n);
    
    //return Cone(a.n, combineOnTop(a.inequalities,b.inequalities),combineOnTop(a.equations,b.equations));
    return Cone(a.n,combineOnTop(a.inequalitiesL,b.inequalitiesL),
		concatenation(a.inequalitiesR,b.inequalitiesR),
		combineOnTop(a.equationsL,b.equationsL),
		concatenation(a.equationsR,b.equationsR));
  }
  bool haveEmptyIntersection(Cone const &a, Cone const &b, ReducerExact *reducer=0)
  {
    return !intersection(a,b).hasPointWithLastCoordinatePositive(reducer);
    //    return false;//FIX THIS
  }

  class Fan
  {
    int n;
  public:
    vector<Cone> cones;
    Matrix<LType> edges;// every cone comes from an edge - used for finding volume of mixed cell
    int getAmbientDimension()const{return n;}
    Fan(int n_):
      n(n_),
      edges(0,n_)
    {
    }
    int maximalNumberOfInequalitiesOfACone()
    {
      int ret=0;
      for(int i=0;i<cones.size();i++)if(ret<cones[i].inequalitiesL.getHeight())ret=cones[i].inequalitiesL.getHeight();
      return ret;
    }
    int size()const{return cones.size();}
    static double random()
    {
      double d=0;
      for(int i=0;i<53;i++){d=(d+(rand()&1))*0.5;}
      return d;
    }
    static Fan fromPolytope(Polytope const &p)
    {
      int numberOfVertices=p.vertices.size();
      Vector<RType> heights(numberOfVertices);
      for(int i=0;i<numberOfVertices;i++)heights[i].random();
      cerr<<"Heights"<<heights<<endl;

      vector<pair<int,int> > edges;

      for(int i=0;i<numberOfVertices;i++)
	for(int j=0;j<i;j++)
	  edges.push_back(pair<int,int>(j,i));

      int n=p.ambientDimension();
      Fan ret(n+1);
      Matrix<LType> edgeVectors(edges.size(),n);

      
      int I=0;
      for(vector<pair<int, int> >::const_iterator i=edges.begin();i!=edges.end();i++,I++)
	{
	  int a=i->first;
	  int b=i->second;
	  Matrix<LType> equationsL(1,n);
	  Vector<RType> equationsR(1);
	  equationsR[0]=heights[a]-heights[b];
	  for(int j=0;j<n;j++)equationsL[0][j]=p.vertices[a][j]-p.vertices[b][j];

	  Matrix<LType> inequalitiesL(numberOfVertices-1,n);
	  Vector<RType> inequalitiesR(numberOfVertices-1);
	  /*	  for(int j=0;j<n+1;j++)inequalities[0][j]=0;
	  inequalities[0][0]=1;//SIGN OF T
	  */	  int K=0/*1*/;
	  for(int k=0;k<numberOfVertices;k++)
	    if(k!=b)
	      {
		inequalitiesR[K]=heights[b]-heights[k];
		for(int j=0;j<n;j++)inequalitiesL[K][j]=p.vertices[b][j]-p.vertices[k][j];
		K++;
	      }
	  ret.cones.push_back(Cone(n+1,inequalitiesL,inequalitiesR,equationsL,equationsR));
	  for(int j=0;j<n;j++)edgeVectors[I][j]=p.vertices[a][j]-p.vertices[b][j];
	}
      ret.edges=edgeVectors;
      return ret;
    }
    friend std::ostream &operator<<(std::ostream &out, Fan const &fan)
    {
      out<<"Number of cones in fan:"<<fan.size()<<endl;
      for(vector<Cone>::const_iterator i=fan.cones.begin();i!=fan.cones.end();i++)
	{
	  out<<*i;
	}
      cerr<<fan.edges;
      out<<"Done printing fan"<<endl;
      return out;
    }
  };

#define BITSPERINTEGER 32
#define BITSPERINTEGER_LOG2 5
#define INTEGERSPERSET 7
class BitSet
{
  //  vector<int> v;
  int v[INTEGERSPERSET];
  int n;
public:
  BitSet():
    n(0)
  {
  }
  BitSet(int n_):
    n(n_)
  {
    assert(n<INTEGERSPERSET*BITSPERINTEGER);
    for(int i=0;i<INTEGERSPERSET;i++)v[i]=0;
  }
  bool get(int i)const{
    assert(i>=0 && i<n);
    int mask=1<<(i&(BITSPERINTEGER-1));
    int index=i>>BITSPERINTEGER_LOG2;
    return mask&(v[index]);
  }
  void set(int i, bool value){
    assert(i>=0 && i<n);
    int mask=1<<(i&(BITSPERINTEGER-1));
    int index=i>>BITSPERINTEGER_LOG2;
    v[index]=(v[index]&(-1-mask))|((value)?mask:0);
  }
  //  int& operator[](int n){assert(n>=0 && n<v.size());return (v[n]);}
  //const int& operator[](int n)const{assert(n>=0 && n<v.size());return (v[n]);}
  void add(BitSet const &b)
  {
    assert(b.n==n);
    for(int i=0;i<INTEGERSPERSET;i++)
      v[i]|=b.v[i];
  }
  inline int size()const
  {
    return n;
  }
  friend std::ostream& operator<<(std::ostream& s, const BitSet &t)
  {
    s<<"(";
    for(int i=0;i<t.size();i++)
      {
	if(i!=0)s<<", ";
	s<<t.get(i);
      }
    s<<")\n";
    }
  BitSet negated()const
  {
    BitSet ret(size());
    for(int i=0;i<INTEGERSPERSET;i++)ret.v[i]=-1-v[i];
    return ret;
  }
  int sizeOfSubset()const
  {
    int ret=0;
    for(int i=0;i<size();i++)if(get(i))ret++;
    return ret;
  }
  };

class Table
{
  vector<vector<vector<BitSet> > > table;
public:
  Table(vector<Fan > const &l):
    table(l.size())
  {
    int N=l.size();
    for(int i=0;i<N;i++)
      {
	vector<vector<BitSet> > v(N);
	for(int j=0;j<N;j++)
	  {
	    vector<BitSet> w(l[i].size());
	    for(int k=0;k<l[i].size();k++)
	      {  w[k]=BitSet(l[j].size());
	      }
	    v[j]=w;
	  }
	table[i]=v;
      }
  }
  bool lookUp(int fan1, int cone1, int fan2, int cone2)
  {
    assert(fan1<table.size());
    assert(fan2<table[fan1].size());
    assert(cone1<table[fan1][fan2].size());
    assert(cone2<table[fan1][fan2][cone1].size());

    return table[fan1][fan2][cone1].get(cone2);
  }
  void set(int fan1, int cone1, int fan2, int cone2)
  {
    assert(fan1<table.size());
    assert(fan2<table[fan1].size());
    assert(cone1<table[fan1][fan2].size());
    assert(cone2<table[fan1][fan2][cone1].size());

    table[fan1][fan2][cone1].set(cone2,true);
    table[fan2][fan1][cone2].set(cone1,true);
    //table[fan1][fan2][cone1].setValue(cone2,true);
    //table[fan2][fan1][cone2].setValue(cone1,true);
  }
  BitSet const& nonCandidates(int fan1, int cone1, int fan2)const
  {
    assert(fan1<table.size());
    assert(fan2<table[fan1].size());
    assert(cone1<table[fan1][fan2].size());

    return table[fan1][fan2][cone1];
  }
  friend std::ostream& operator<<(std::ostream& s, const Table &t)
  {
    for(int i=0;i<t.table.size();i++)
      for(int j=0;j<t.table[i].size();j++)
	{
	  s<<"Entry ("<<i<<","<<j<<")\n";
	  for(int k=0;k<t.table[i][j].size();k++)
	    s<<t.table[i][j][k];
	}
  }
};

class RelationTable
{
  vector<Fan > fanList;
  Table knownEmptyIntersectionInIntersection;
  Table knownNonEmptyIntersection;
public:
  int numberOfSolvedLPs;
  RelationTable(vector<Fan> const &l):
    fanList(l),
    knownEmptyIntersectionInIntersection(l),
    knownNonEmptyIntersection(l),
    numberOfSolvedLPs(0)
  {

  }
  bool knownToIntersectTriviallyInIntersection(int fan1, int cone1, int fan2, int cone2)
  {
    assert(fan1<fanList.size());
    assert(fan2<fanList.size());
    assert(cone1<fanList[fan1].size());
    assert(cone2<fanList[fan2].size());

    return knownEmptyIntersectionInIntersection.lookUp(fan1,cone1,fan2,cone2);
  }
  bool intersectTriviallyInIntersection(int fan1, int cone1, int fan2, int cone2)
  {
    assert(fan1<fanList.size());
    assert(fan2<fanList.size());
    assert(cone1<fanList[fan1].size());
    assert(cone2<fanList[fan2].size());


    if(knownEmptyIntersectionInIntersection.lookUp(fan1,cone1,fan2,cone2))
      return true;
    if(knownNonEmptyIntersection.lookUp(fan1,cone1,fan2,cone2))
      return false;

    //    fprintf(Stderr,"UPDATING:f1:%i,c1:%i,f2:%i,c2:%i\n",fan1,cone1,fan2,cone2);
    bool ret;
    if((fan1!=fan2) && (cone1!=cone2))
      ret=haveEmptyIntersection(fanList[fan1].cones[cone1],fanList[fan2].cones[cone2]);
    else
      ret=false;
    //    cerr<<"UPDATING:f1:"<<fan1<<",c1:"<<cone1<<",f2:"<<fan2<<",c2:"<<cone2<<"ret"<<ret<<"\n";
    numberOfSolvedLPs++;
    if(ret)
      knownEmptyIntersectionInIntersection.set(fan1,cone1,fan2,cone2);
    else
      knownNonEmptyIntersection.set(fan1,cone1,fan2,cone2);
    return ret;
  }
  const BitSet &getNonCandidates(int fan1, int cone1, int fan2)
  {
    //  for(int c2=0;c2<fanList[fan2].size();c2++)
	  //      intersectTriviallyInIntersection(fan1,cone1,fan2,c2);

    return knownEmptyIntersectionInIntersection.nonCandidates(fan1,cone1,fan2);
  }
  void markNoIntersectionInIntersection(int fan1, int cone1, int fan2, int cone2)
  {
    //    cerr<<"MARKING"<<fan1<<cone1<<fan2<<cone2<<endl;assert(fan1!=fan2 || cone1!=cone2);
    knownEmptyIntersectionInIntersection.set(fan1,cone1,fan2,cone2);
  }
  void markKnownNonEmptyIntersection(int fan1, int cone1, int fan2, int cone2)
  {
	  knownNonEmptyIntersection.set(fan1,cone1,fan2,cone2);
  }
  friend std::ostream& operator<<(std::ostream& s, const RelationTable &t)
  {
    s<<"knownEmptyIntersectionInIntersection:";
    s<<t.knownEmptyIntersectionInIntersection;
    s<<"knownNonEmptyIntersection:";
    s<<t.knownNonEmptyIntersection;
  }
};


struct RecursionData
{
  int ambientDimension; //with the t-coordinate
  vector<Fan> fans;
  vector<Matrix<LType> > inequalityMatricesL;//one matrix for each recursion level
  vector<Vector<RType> > inequalityMatricesR;
  IntegerVector inequalityMatricesNumberOfUsedRows1;
  IntegerVector inequalityMatricesNumberOfUsedRows2;
  IntegerVector chosen;
  IntegerVector chosenFans;
  IntegerVector iterators; //just used for printing
  IntegerVector nCandidates; //just used for printing
  BitSet usedFans;
  int numberOfUsefulCalls;
  int totalNumberOfCalls;
  ReducerExact reducer;
public:
  RelationTable table;
  int cellVolume()
  {
    Matrix<LType> m(fans.size(),ambientDimension-1);
    for(int i=0;i<fans.size();i++)
      m[i].set(fans[chosenFans[i]].edges[chosen[i]].toVector());
    //cerr<<m;
    LType d=m.reduceAndComputeDeterminant();
    //cerr<<"DETERMINANT"<<d<<endl;
    return volumeToInt(d);
  }
  RecursionData(int ambientDimension_, vector<Fan> const &fans_):
    ambientDimension(ambientDimension_),
    table(fans_),
    fans(fans_),
    chosen(fans_.size()),
    chosenFans(fans_.size()),
    usedFans(fans_.size()),
    iterators(fans_.size()),
    nCandidates(fans_.size()),
    numberOfUsefulCalls(0),
    totalNumberOfCalls(0),
    reducer(fans_.size()),
    inequalityMatricesL(0),
    inequalityMatricesR(0),
    inequalityMatricesNumberOfUsedRows1(fans_.size()),
    inequalityMatricesNumberOfUsedRows2(fans_.size())
  {
    int totalNumberOfInequalities=0;
    int maximalNumberOfInequalities=0;
    for(int i=0;i<fans_.size();i++)
      {
	int a=fans[i].maximalNumberOfInequalitiesOfACone();
	//cerr<<a;
	totalNumberOfInequalities+=a;
	if(a>maximalNumberOfInequalities)maximalNumberOfInequalities=a;
      }
    for(int i=0;i<ambientDimension-1;i++)
      {
	inequalityMatricesL.push_back(Matrix<LType>(min((i+1)*maximalNumberOfInequalities,totalNumberOfInequalities),ambientDimension-i-1-1));
	inequalityMatricesR.push_back(Vector<RType>(min((i+1)*maximalNumberOfInequalities,totalNumberOfInequalities)));
      }
  }
  BitSet computeCandidates(int index, int fanNumber)
  {
    BitSet nonCandidates(fans[fanNumber].size());
      for(int i=0;i<index;i++)
      {
	nonCandidates.add(table.getNonCandidates(chosenFans[i],chosen[i],fanNumber));
      }
      /* <------------------------------------------------------------------------------------------- Remember to uncomment this
    for(int j=0;j<nonCandidates.size();j++)
      if((!nonCandidates[j]))// ||randBool()
	for(int i=0;i<index;i++)
	  if(table.intersectTriviallyInIntersection(chosenFans[i], chosen[i], fanNumber, j))
	    {
	      nonCandidates[j]=true;
	      break;
	    }
      */
    return nonCandidates.negated();
  }

  bool closure()
  {
    //cerr<<table;assert(0);
    bool ret=false;
    int a=0;
    for(int f1=0;f1<fans.size();f1++)
      {
	for(int f2=f1+1;f2<fans.size();f2++)
	  for(int c1=0;c1<fans[f1].size();c1++)
	    for(int c2=0;c2<fans[f2].size();c2++)
	      {
		//		if(!table.intersectTriviallyInIntersection(f1,c1,f2,c2))
		if(!table.knownToIntersectTriviallyInIntersection(f1,c1,f2,c2))
		  {
		    bool dontintersect=false;
		    for(int f3=0;f3<fans.size();f3++)
		      {
			BitSet c=table.getNonCandidates(f1,c1,f3);
			c.add(table.getNonCandidates(f2,c2,f3));
			//cerr<<table.getNonCandidates(f1,c1,f3);
			//cerr<<table.getNonCandidates(f2,c2,f3);
			if(c.negated().sizeOfSubset()==0)
			  {
			    dontintersect=true;
			    a++;
			    statistics.forFree++;
			    //  cerr<<" f3:"<<f3;
			    break;
			  }
						if(c.negated().sizeOfSubset()<4 && ((f3&7) ==0))//just an experiment
			  {
			    for(int k=0;k<c.size();k++)
			      if(!c.get(k))
				{
				  table.intersectTriviallyInIntersection(f1,c1,f3,k);
				  table.intersectTriviallyInIntersection(f2,c2,f3,k);
				}
				}
		      }
		    if(dontintersect)
		      {
			table.markNoIntersectionInIntersection(f1,c1,f2,c2);
			ret=true;
			//cerr<<"f1:"<<f1<<" f2:"<<f2<<" c1:"<<c1<<" c2:"<<c2<<endl;
			//cerr<<table;
		      }
		  }
	      }
      }
    cerr<<a<<" FOR FREE\n";
    return ret;
  }

  void transitiveClosure()
  {
    while(closure());
  }

  void completeTable()
  {
	    for(int f1=0;f1<fans.size();f1++)
	    	for(int c1=0;c1<fans[f1].size();c1++)
	    	  for(int c2=0;c2<fans[f1].size();c2++)
	    		  if(c1!=c2)
	    			  table.markNoIntersectionInIntersection(f1,c1,f1,c2);
//	    		  else
//	    			  table.markKnownNonEmptyIntersection(f1,c1,f1,c2);


	    	for(int f1=0;f1<fans.size();f1++)
      {
	for(int f2=f1+1;f2<fans.size();f2++)
	for(int c1=0;c1<fans[f1].size();c1++)
	  for(int c2=0;c2<fans[f2].size();c2++)
	    table.intersectTriviallyInIntersection(f1,c1,f2,c2);
      }
		//		    cerr<<table;
		    transitiveClosure();
		    //   cerr<<table;
  }
  /*
    Returns mixed volume for subtree.
   */
  int rek(int index, Cone const &current)
  {
    statistics.nRekCalls++;
    totalNumberOfCalls++;

    int mixedVolumeAccumulator=0;

    if(index == fans.size())
      {
	//	cerr<<"CELL FOUND\n";
	numberOfUsefulCalls++;
	statistics.nCells++;
	return int(cellVolume()+0.5);
      }
    else
      {
	int bestIndex=-1;
	int bestNumberOfCandidates=1000000;
	for(int i=0;i<fans.size();i++)
	  {
	    if(!usedFans.get(i))
	      {
		int n=computeCandidates(index,i).sizeOfSubset();
		if(n<=bestNumberOfCandidates)  //we could choose a strict inequality
		  {
		    bestNumberOfCandidates=n;
		    bestIndex=i;
		  }
	      }
	  }
	assert(bestIndex!=-1);
	BitSet candidates=computeCandidates(index,bestIndex);


	chosenFans[index]=bestIndex;
	usedFans.set(chosenFans[index],true);


	nCandidates[index]=bestNumberOfCandidates;//just for printing

	static int iterationNumber;
	if(!(iterationNumber++ & (16*256-1)))
	  //	  log2
	  	{
	  fprintf(stderr,"Iteration level:%i, Chosen fan:%i, Number of candidates:%i, Iteration Number:%i, Useful (%i/%i)=%f\n",index,bestIndex,bestNumberOfCandidates,iterationNumber,numberOfUsefulCalls,totalNumberOfCalls,float(numberOfUsefulCalls)/totalNumberOfCalls);
	  cerr<<"Chosen fans vector: "<<chosenFans<<endl;
	  cerr<<"\nChosen cone vector: "<<chosen<<endl;
	  cerr<<"\nNcandidates vector: "<<nCandidates<<endl;
	  cerr<<"\nIterator vector:    "<<iterators<<endl;
	  fprintf(stderr,"\n\n");
	  }


	statistics.nMTKNodes+=fans[chosenFans[index]].size();
	for(int i=0;i<fans[chosenFans[index]].size();i++)
	  if(candidates.get(i))
	    {
	      bool ok=true;
	      for(int j=0;j<index;j++)
		{
		  if(table.intersectTriviallyInIntersection(chosenFans[j],chosen[j],chosenFans[index],i))
		    {
		      ok=false;
		      break;

		    }
		}
	      if(ok)statistics.nLPRunNodes++;
	      bool pushed=reducer.push(fans[chosenFans[index]].cones[i].equationsL[0].toVector(),fans[chosenFans[index]].cones[i].equationsR[0]);
	      if(pushed)
		{
		  int numberOfAddedInequalities=0;
		  if(index!=0)inequalityMatricesNumberOfUsedRows1[index]=numberOfAddedInequalities=reducer.singleReduction(inequalityMatricesL[index-1],inequalityMatricesR[index-1],inequalityMatricesNumberOfUsedRows2[index-1],inequalityMatricesL[index],inequalityMatricesR[index]);
	      //cerr<<"---";
		  // cerr<<"Number of added:"<<numberOfAddedInequalities<<endl;
	 	if(index==5)
		  {
		    // cerr<<inequalityMatrices[index]<<endl;
		    // cerr<<numberOfAddedInequalities<<endl;
		  }
		//if(ok && !haveEmptyIntersection(current,fans[chosenFans[index]].cones[i],&reducer))

		/*		if(index==6)
		  {
		    if(reducer.hashedInconsistencyLookup(inequalityMatrices[index],numberOfAddedInequalities))
		      {
			cerr<<"A";
		      }
		    else
		      {
			cerr<<"B";
			//cerr<<inequalityMatrices[index]<<endl;
			//cerr<<numberOfAddedInequalities<<endl;

		      }
		  }
		if(reducer.hashedInconsistencyLookup(inequalityMatrices[index],numberOfAddedInequalities))
		  {
		    //   cerr<<inequalityMatrices[index]<<endl;
		    //  cerr<<numberOfAddedInequalities<<endl;
		  }else*/
		  if(numberOfAddedInequalities>=0)
		    if(ok && fans[chosenFans[index]].cones[i].hasPointWithLastCoordinatePositiveInCone(inequalityMatricesL[index],inequalityMatricesR[index],
												       inequalityMatricesNumberOfUsedRows1[index],
												       inequalityMatricesNumberOfUsedRows2[index],
												       reducer))
		  		{
				  useNewAntiCyclingRule=false;true;
				  if(!fans[chosenFans[index]].cones[i].hasPointWithLastCoordinatePositiveInCone(inequalityMatricesL[index],inequalityMatricesR[index],
													    inequalityMatricesNumberOfUsedRows1[index],
													    inequalityMatricesNumberOfUsedRows2[index],
														reducer))
				    {
				      //debug=true;
				      fans[chosenFans[index]].cones[i].hasPointWithLastCoordinatePositiveInCone(inequalityMatricesL[index],inequalityMatricesR[index],
													    inequalityMatricesNumberOfUsedRows1[index],
													    inequalityMatricesNumberOfUsedRows2[index],
														reducer);
				      assert(0);
				    }
				  useNewAntiCyclingRule=false;
		  

				  /*if(haveEmptyIntersection(current,fans[chosenFans[index]].cones[i],&reducer))
				    {
				    cerr<<current;
				    cerr<<fans[chosenFans[index]].cones[i];
				    assert(0);
				    }*/
				  chosen[index]=i;
				  
				  Cone next=intersection(current,fans[chosenFans[index]].cones[i]/*,true*/);
				  //if(index==1)next.optimize();//<----------What is the best level for optimizing?
				  {
				    //cerr<<"CALLING"<<index+1<<endl;
				    mixedVolumeAccumulator+=rek(index+1,next);
				  }
				  chosen[index]=-1;//just for printing
				}
		}
	      if(pushed)reducer.pop();
	      iterators[index]++;//just for printing
	    }

	nCandidates[index]=-1;//just for printing
	iterators[index]=0;//just for printing

	usedFans.set(chosenFans[index],false);
	chosenFans[index]=-1;
      }
    if(mixedVolumeAccumulator>0)numberOfUsefulCalls++;
    return mixedVolumeAccumulator;
  }
};




vector<Fan> reduceDimension(int ambientDimension, vector<Fan> const &fans, int &numberOfRemovedDimensions)
{//Assuming subspaces are hyperplanes
  int numberOfSubspaces=0;
  for(int i=0;i<fans.size();i++)
    if(fans[i].cones.size()==1)numberOfSubspaces++;

  int I=0;
  Matrix<LType> equationsL(numberOfSubspaces,ambientDimension-1);
  Vector<RType> equationsR(numberOfSubspaces);
  for(int i=0;i<fans.size();i++)
    if(fans[i].cones.size()==1)
      {
	//	equations[I++].set(fans[i].cones[0].equations[0].toVector());
	equationsL[I].set(fans[i].cones[0].equationsL[0].toVector());
	equationsR[I++]=fans[i].cones[0].equationsR[0];
      }
  //  equations.cycleColumnsLeft(1);
  reducePair(equationsL,equationsR,false);

  //cerr<<"EQUATIONS"<<equationsL<<equationsR;

  //  equations.reduce(false);
  int rank=equationsL.numberOfPivots();
  numberOfRemovedDimensions=rank;


  vector<Fan> ret;
  I=0;
  for(int i=0;i<fans.size();i++)
    if(fans[i].cones.size()!=1)
      {
	//cerr<<fans[i];
	Fan newFan(ambientDimension);
	newFan.edges=Matrix<LType>(fans[i].edges.getHeight(),ambientDimension-1-rank);
	for(int j=0;j<fans[i].cones.size();j++)
	  {
	    Matrix<LType> equations2L=fans[i].cones[j].equationsL;
	    Vector<RType> equations2R=fans[i].cones[j].equationsR;
	    Matrix<LType> inequalities2L=fans[i].cones[j].inequalitiesL;
	    Vector<RType> inequalities2R=fans[i].cones[j].inequalitiesR;
	    //	    equations2=equations.normalForms(equations2);
	    normalFormPairs(equations2L, equations2R, equations2L,equations2R, equationsL, equationsR);

	    //cerr<<inequalities2L.getWidth()<<(int)equationsL.getWidth();
	      //inequalities2=equations.normalForms(inequalities2);
	    normalFormPairs(inequalities2L, inequalities2R, inequalities2L,inequalities2R, equationsL, equationsR);

	    //	    newFan.cones.push_back(Cone(inequalities2.getWidth(),inequalities2,equations2));
	    newFan.cones.push_back(Cone(inequalities2L.getWidth()+1,inequalities2L,inequalities2R,equations2L,equations2R));

	    //	    cerr<<newFan;assert(0);

	    //cerr<<fans[i].edges.getHeight();
	    Vector<LType> v1=fans[i].edges[j].toVector();
	    Vector<LType> v2(v1.size()-equationsL.numberOfPivots());
	    {
	      int pivotI=-1;
	      int pivotJ=-1;
	      int i=0;
	      int last=-1;
	      while(equationsL.nextPivot(pivotI,pivotJ))
		{
		  while(pivotJ-1>last)
		    {
		      v2[i++]=v1[++last];
		    }
		  last=pivotJ;
		}
	      last++;
	      while(last<v1.size())
		{
		  //  cerr<<i<<last<<endl;
		  v2[i++]=v1[last++];
		}
	    }
	    newFan.edges[j].set(v2);
	  }
	ret.push_back(newFan);
      }
  //  for(vector<Fan>::const_iterator i=ret.begin();i!=ret.end();i++)cerr<<*i;
  return ret;
}

};

