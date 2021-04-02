#ifndef MATRIX_H_INCLUDED
#define MATRIX_H_INCLUDED

//#include <cmath>
#include <assert.h>
#include <algorithm>
#include <vector>
#include <iostream>
#include <stdlib.h>

#define MY_INLINE __inline__ __attribute__((always_inline))
//#define MY_INLINE inline

namespace mixedCells
{
#include "allocator.h"
}

#include "vektor.h"

// general threshold
//#define EPSILON 0.0001
//#define EPSILON 0.00001
#define EPSILON 0.0000001


#define TEST if(1)

// allocation of matrices is optimized for cache reasons
#define MATRIXMALLOC(x) allocator.alloc(x)
#define MATRIXFREE(x) allocator.free(x)

namespace mixedCells
{

  // the type of entries in matrices
  //  #define typ double

  template <class typ> class Vector{
    int length;
  public://public needed since we don't know how to make templated classes friends
    typ *data;
  public:
  Vector(int n_=0):
    length(n_)
    {
      assert(length>=0);
      data=(typ*)VECTORMALLOC(length*sizeof(typ));
      for(int i=0;i<length;i++)
	new (data+i) typ();
      //data[i]=0;
    }
  Vector(int n_, bool clear): // clear is ignored
    length(n_)
    {
      assert(length>=0);
      data=(typ*)VECTORMALLOC(length*sizeof(typ));
      for(int i=0;i<length;i++)
	new (data+i) typ();
    }
  Vector(const Vector &a):
    length(a.length)
    {
      data=(typ*)VECTORMALLOC(length*sizeof(typ));
      for(int i=0;i<length;i++)	new (data+i) typ(a.data[i]);
      //data[i]=a.data[i];
    }
  ~Vector(){
    if(data)
      {
	for(int i=0;i<length;i++) data[i].~typ();
	VECTORFREE(data);
	data=0;
      }
  };
  inline friend Vector operator-(const Vector& q){
    Vector ret(q.size());
    for(int i=0;i<q.size();i++) 
      ret[i]=-q[i];
    return ret;
  };
  Vector& operator=(const Vector& v)
    {
      if(this==&v)
	{
	  return *this;
	}
      else
	{
	  if(data)
	    {
	      if(length!=v.length)
		{
		  for(int i=0;i<length;i++) data[i].~typ();
		  VECTORFREE(data);
		  length=v.length;
		  data=(typ*)VECTORMALLOC(length*sizeof(typ));
		  for(int i=0;i<length;i++)	new (data+i) typ();
      		}
	    }
	  else
	    {
	      length=v.length;
	      data=(typ*)VECTORMALLOC(length*sizeof(typ));
	      for(int i=0;i<length;i++)	new (data+i) typ();
	    }
	  for(int i=0;i<length;i++)data[i]=v.data[i];
	}
      return (*this);
    }
  void clear()
  {
    for(int i=0;i<length;i++)data[i]=0;
  }
  MY_INLINE int size()const
  {
    return length;
  }
  MY_INLINE typ& operator[](int n)
    {
      TEST if(!(n>=0 && n<length))assert(!"outOfRange(n,v.size())");
      return (data[n]);
    }
  MY_INLINE typ const& operator[](int n)const
    {
      TEST    if(!(n>=0 && n<length))assert(!"outOfRange(n,v.size())");
      return (data[n]);
    }
  friend Vector concatenation(Vector const &a, Vector const &b)
  {
    Vector ret(a.size()+b.size());
    for(int i=0;i<a.size();i++)ret[i]=a[i];
    for(int i=0;i<b.size();i++)ret[i+a.size()]=b[i];
    return ret;
  }
  friend MY_INLINE typ dot(Vector const &a, Vector const &b)
  {
    TEST assert(a.size()==b.size());
    typ ret=0;
    for(int i=0;i<a.size();i++)ret+=a[i]*b[i];
    return ret;
  }
  friend Vector operator*(typ s, const Vector& q)
  {
    Vector ret(q.size());
    for(int i=0;i<ret.size();i++)ret[i]=s*q[i];
    return ret;
  }
  Vector& operator+=(const Vector& q)
    {
      TEST assert(q.size()==size());
      
      //std::vector<typ>::const_iterator qi=q.data.begin();
      //for(std::vector<typ>::iterator i=data.begin();i!=data.end();i++,qi++)*i+=*qi;
            for(int i=0;i<size();i++)data[i]+=q[i];
      return *this;
    }
  Vector& operator-=(const Vector& q)
    {
      TEST assert(q.size()==size());
      
      for(int i=0;i<size();i++)data[i]-=q[i];
      return *this;
    }
  //  friend std::ostream& operator<<(std::ostream& s, const Vector &v);    
friend std::ostream& operator<<(std::ostream& s, const Vector &v)
{
  s<<"(";
  for(int j=0;j<v.size();j++)
    {
      if(j)s<<",";
      double temp=toDoubleForPrinting(v[j]);
      s<<temp;
    }
  s<<")";
  return s;
}

  Vector subvector(int begin, int end)const
    {
      TEST assert(begin>=0);
      TEST assert(end<=size());
      TEST assert(end>=begin);
      Vector ret(end-begin);
      for(int i=0;i<end-begin;i++)
	ret[i]=data[begin+i];
      return ret;
    }
  /**
     Multiply s and q and add the result to *this.
   */
  Vector& madd(typ s, const Vector& q)
    {
      TEST assert(q.size()==size());
      for(int i=0;i<q.size();i++)
	data[i]+=s*q.data[i];
      return *this;
    }
  bool isZero()const
  {
    for(int i=0;i<size();i++)
      if(!isZero2(data[i]))return false;
    return true;
  }

  //  friend class Matrix<typ>;
  };

  typedef Vector<double> VectorDouble;

template <class typ> class Matrix{
  int height,width;
  typ *data;
 public:
  //  static MY_INLINE bool isZero(typ a){return (a<EPSILON)&&(a>-EPSILON);}
  class RowRef{
    int rowNum;
    Matrix &matrix;
  public:
  MY_INLINE RowRef(Matrix &matrix_, int rowNum_):
    rowNum(rowNum_),
      matrix(matrix_)
      {
      }
    MY_INLINE typ &operator[](int j)
      {
	TEST assert(j>=0);
	TEST assert(j<matrix.width);
	return matrix.data[matrix.width*rowNum+j];
      }
    Vector<typ> toVector()
    {
      Vector<typ> ret(matrix.width);
      for(int j=0;j<matrix.width;j++)
	ret[j]=matrix.data[matrix.width*rowNum+j];
      return ret;
    }
    void set(Vector<typ> const &v)
    {
      TEST assert(v.size()==matrix.width);
      for(int j=0;j<matrix.width;j++)
	matrix.data[matrix.width*rowNum+j]=v[j];      
    }
    bool isZero()const
    {
      for(int j=0;j<matrix.width;j++)if(!isZero2(matrix.data[matrix.width*rowNum+j]))return false;
      return true;
    }
  };
  class const_RowRef{
    int rowNum;
    Matrix const &matrix;
  public:
  MY_INLINE const_RowRef(const Matrix  &matrix_, int rowNum_):
    rowNum(rowNum_),
      matrix(matrix_)
      {
      }
    MY_INLINE typ const &operator[](int j)
      {
	TEST assert(j>=0);
	TEST assert(j<matrix.width);
	return matrix.data[matrix.width*rowNum+j];
      }
    Vector<typ> toVector()
    {
      Vector<typ> ret(matrix.width);
      for(int j=0;j<matrix.width;j++)
	ret[j]=matrix.data[matrix.width*rowNum+j];
      return ret;
    }
    bool isZero()const
    {
      for(int j=0;j<matrix.width;j++)if(!matrix.isZero(matrix.data[matrix.width*rowNum+j]))return false;
      return true;
    }
  };
 Matrix():
  height(0),
    width(0)
  {
    data=0;
  }
 inline Matrix(int height_, int width_):
  height(height_),
    width(width_)
  {
    data=(typ*)MATRIXMALLOC(height*width*sizeof(typ)); //!!! won't work with gmp
    const int I=height*width;
    for(int i=0;i<I;i++) 
      new (data+i) typ();
    // memset(data,0,sizeof(typ)*width*height); // does not work with ShortRat
  }
 Matrix(const Matrix &m):
  height(m.height),
    width(m.width)
    {
     data=(typ*)MATRIXMALLOC(height*width*sizeof(typ));
     //for(int i=0;i<height*width;i++)data[i]=m.data[i];
     const int I=height*width;
     for(int i=0;i<I;i++) 
       new (data+i) typ(m.data[i]);
       //data[i]=m.data[i]; //!!! won't work with gmp
     //memcpy(data,m.data,sizeof(typ)*width*height);
    }
  Matrix& operator=(const Matrix& m)
    {
      if(this==&m)
	{
	  return *this;
	}
      else
	{
	  if(data)
	    {
	      if(!((width==m.width) && (height==m.height)))
		{
		  for(int i=0;i<height*width;i++) data[i].~typ();
		  MATRIXFREE(data);
		  width=m.width;
		  height=m.height;
		  data=(typ*)MATRIXMALLOC(height*width*sizeof(typ));
		  for(int i=0;i<height*width;i++) new (data+i) typ();
		}
	    }
	  else
	    {
	      width=m.width;
	      height=m.height;
	      data=(typ*)MATRIXMALLOC(height*width*sizeof(typ));
	      for(int i=0;i<height*width;i++) new (data+i) typ();
	    }

	  //for(int i=0;i<height*width;i++)data[i]=m.data[i];
	  const int I=height*width;
	  for(int i=0;i<I;i++)data[i]=m.data[i];
	  //memcpy(data,m.data,sizeof(typ)*width*height);
	  return *this;
	}
    }
  ~Matrix()
    {
      if(data)
	{
	  for(int i=0;i<height*width;i++) data[i].~typ();
	  MATRIXFREE(data);
	  data=0;
	}
    }
  static Matrix identity(int n)
  {
    Matrix ret(n,n);
    for(int i=0;i<n;i++)ret[i][i]=1;
    return ret;
  }
  /**
     Returns the number of rows of the matrix.
  */
  MY_INLINE int getHeight()const
  {
    return height;
  }
  /**
     Returns the number of columns of the matrix.
  */
  MY_INLINE int getWidth()const
  {
    return width;
  }
  MY_INLINE RowRef operator[](int i)
  {
    TEST assert(i>=0);
    TEST assert(i<height);
    return RowRef(*this,i);
  }
  MY_INLINE const_RowRef operator[](int i)const//should really return const_RowRef
  {
    TEST assert(i>=0);
    TEST assert(i<height);
    return const_RowRef(*this,i);
  }
  void multiplyAndAddColumn(int sourceColumn, typ scalar, int destinationColumn)
  {
    // for(int i=0;i<height;i++)
	  //  (*this)[i][destinationColumn]+=scalar* (*this)[i][sourceColumn];
      int width=this->width;int height=this->height;
          for(int i=0;i<height;i++)
      {
    	data[i*width+destinationColumn]+=scalar*data[i*width+sourceColumn];
	}
	  /*
    typ * __restrict  source=data+sourceColumn;
    typ * __restrict  dest=data+destinationColumn;
    for(int i=0;i<height;i++)
      {
    	(*(dest))+=scalar*(*(source));
	dest+=width;
	source+=width;
	}*/
  }
  void multiplyAndAddRow(int sourceRow, typ scalar, int destinationRow)
  {
    typ * __restrict source=data+width*sourceRow;
    typ * __restrict dest=data+width*destinationRow;
    for(int i=0;i<width;i++)
      dest[i]+=scalar*source[i];
	//    for(int i=0;i<width;i++)
	// (*this)[destinationRow][i]+=scalar* (*this)[sourceRow][i];
  }
  void replaceWithLinearCombination(int row1, typ a1, int row2, typ a2, int destinationRow)
  {
    typ * __restrict r1=data+width*row1;
    typ * __restrict r2=data+width*row2;
    typ * __restrict dest=data+width*destinationRow;
    for(int i=0;i<width;i++)
      dest[i] = a1*r1[i]+a2*r2[i];
  }
  void scaleColumn(int column, typ scalar)
  {
    for(int i=0;i<height;i++)
      (*this)[i][column]*=scalar;
  }
  void scaleRow(int row, typ scalar)
  {
    for(int i=0;i<width;i++)
      (*this)[row][i]*=scalar;
  }
  friend std::ostream& operator<<(std::ostream& s, const Matrix &m)
{
  assert(0);
  s<<m.height<<" "<<m.width<<endl;
  for(int i=0;i<m.height;i++)
    {
      for(int j=0;j<m.width;j++)
	{
	  if(j)s<<" ";
	  double temp=toDoubleForPrinting(m[i][j]);
	  //	  s<<m[i][j];
	  s<<temp;
	}
      s<<" hash:"<<(int)m.hashValue(i,m.width);
      s<<endl;
    }
  return s;
}

  friend Matrix combineOnTop(Matrix const &top, Matrix const &bottom)
  {
    TEST if(top.getWidth()!=bottom.getWidth())
      {
	cerr<<top<<bottom;
      }
    assert(top.getWidth()==bottom.getWidth());
    Matrix ret(top.getHeight()+bottom.getHeight(),top.getWidth());
    for(int j=0;j<top.getWidth();j++)
      {
	for(int i=0;i<top.getHeight();i++)ret[i][j]=top[i][j];
	for(int i=0;i<bottom.getHeight();i++)ret[i+top.getHeight()][j]=bottom[i][j];
      }
    
    //memcpy(ret.data,top.data,sizeof(typ)*top.getWidth()*top.getHeight());
    //memcpy(ret.data+top.getHeight()*top.getWidth(),bottom.data,sizeof(typ)*bottom.getWidth()*bottom.getHeight());

    return ret;
  }

  Matrix transposed()const
  {
    Matrix ret(getWidth(),getHeight());
    /*    for(int i=0;i<getHeight();i++)
      for(int j=0;j<getWidth();j++)
	ret[j][i]=(*this)[i][j];
    */
    typ * __restrict__ dest=ret.data;

    const int srcdelta=getWidth();
    int i=getWidth();
    while(i-->0)
      {
	const typ *__restrict__ src=data+(getWidth()-i-1);
	int j=getHeight();
	while(j-->0)
	  {
	    *(dest++)=*src;
	    src+=srcdelta;
	  }
      }

    return ret;
  }

  Matrix operator-()const
  {
    Matrix ret(*this);
    for(int i=0;i<getHeight()*getWidth();i++)ret.data[i]=-ret.data[i];
    return ret;
  }

  Matrix operator*(Matrix const &b)const{TEST assert(width==b.height);Matrix ret(height,b.width);for(int i=0;i<ret.height;i++)for(int j=0;j<ret.width;j++){typ s=0;for(int k=0;k<width;k++)s+=(*this)[i][k]*b[k][j];ret[i][j]=s;}return ret;}

  VectorDouble operator*(VectorDouble const &b)const{TEST assert(width==b.size());VectorDouble ret(height);for(int i=0;i<ret.size();i++){typ s=0;for(int k=0;k<width;k++)s+=(*this)[i][k]*b[k];ret[i]=s;}return ret;}


  /*
    Computes the dot product of ith column of *this with jth row of m.
   */
  MY_INLINE typ rowDotColumnOfOther(int i, Matrix const &m, int j)const
  {
    TEST assert(i>=0);
    TEST assert(i<height);
    TEST assert(j>=0);
    TEST assert(j<m.width);
    TEST assert(width==m.height);

    typ ret=0;
    typ * __restrict src1=data+width*i;
    typ * __restrict src2=m.data+j;
    for(int k=0;k<width;k++)
      {
	ret+=src1[k]*(*src2);
	src2+=m.width;
      }
    return ret;
  }

  int numberOfPivots()const
{
  int ret=0;
  int pivotI=-1;
  int pivotJ=-1;
  while(nextPivot(pivotI,pivotJ))ret++;
  return ret;
}

  int reduceAndComputeRank()
{
  reduce(false);
  return numberOfPivots();
}

  VectorDouble normalForm(VectorDouble v)const//assume reduced
{
  int pivotI=-1;
  int pivotJ=-1;
  int nonpivots=v.size();
  while(nextPivot(pivotI,pivotJ))
    {
      nonpivots--;
      v-=(v[pivotJ]/(*this)[pivotI][pivotJ])*(*this)[pivotI].toVector();
    }
  VectorDouble ret(nonpivots);
  pivotI=-1;
  pivotJ=-1;
  int i=0;
  int last=-1;
  while(nextPivot(pivotI,pivotJ))
    {
      while(pivotJ-1>last)
	{
	  ret[i++]=v[++last];
	  //	    cerr<<"("<<(i-1)<<","<<last<<")";
	}
      last=pivotJ;
    }
  last++;
  while(last<width)
    ret[i++]=v[last++];
  //if(debug)cerr<<v<<":"<<ret<<endl;
  assert(i==nonpivots);
  return ret;
}

  Matrix normalForms(Matrix const &m)const//assume reduced
{
  //cerr<<*this;
  Matrix ret(m.height,width-numberOfPivots());
  for(int i=0;i<m.height;i++)
    ret[i].set(normalForm(m[i].toVector()));
  return ret;
}

  MY_INLINE void swapRows(int a, int b)
  {for(int j=0;j<getWidth();j++){typ temp=(*this)[a][j];(*this)[a][j]=(*this)[b][j];(*this)[b][j]=temp;}}
  /*{
    assert(a>=0);
    assert(b>=0);
    assert(a<height);
    assert(b<height);
    typ *__restrict aRow=data+a*width;
    typ *__restrict bRow=data+b*width;
    for(int j=0;j<width;j++){typ temp=aRow[j];aRow[j]=bRow[j];bRow[j]=temp;}
    }*/
  int findRowIndex(int column, int currentRow)const
{
  int best=-1;
  int bestNumberOfNonZero;
  for(int i=currentRow;i<height;i++)
    if(!isZero((*this)[i][column]))
      {
	return i;//<-------------------------------------------- no need to find row with many zeros
	int nz=0;
	for(int k=column+1;k<width;k++)
	  if(!isZero((*this)[i][k]))nz++;
	if(best==-1)
	  {
	    best=i;
	    bestNumberOfNonZero=nz;
	  }
	else if(nz<bestNumberOfNonZero)
	  {
	    best=i;
	    bestNumberOfNonZero=nz;	    
	  }
      }
  return best;
}

  MY_INLINE bool nextPivot(int &i, int &j)const//;
  //bool Matrix::nextPivot(int &i, int &j)const//iterates through the pivots in a matrix in reduced row echelon form. To find the first pivot put i=-1 and j=-1 and call this routine. When no more pivots are found the routine returns false.
{
  i++;
  if(i>=height)return false;
  while(++j<width)
    {
      if(!isZero((*this)[i][j])) return true;
    }
  return false;
}

/* Run a Gauss reduction. Returns the number of swaps. The number of
   swaps is need if one wants to compute the determinant
   afterwards. In this case it is also a good idea to set the flag
   which make the routine terminate when a it is discovered the the
   determinant is zero. */
  int reduce(bool returnIfZeroDeterminant)
{
  //  cerr<<*this;
  //  if(width<=1)cerr<<height<<"x"<<width<<endl;
  int retSwaps=0;
  int currentRow=0;
  
  for(int i=0;i<width;i++)
    {
      int s=findRowIndex(i,currentRow);
      
      if(s!=-1)
	{
	  if(s!=currentRow)
	    {
	      swapRows(currentRow,s);
	      retSwaps++;
	    }
	  for(int j=currentRow+1;j<height;j++)
	      {
		multiplyAndAddRow(currentRow,-(*this)[j][i]/(*this)[currentRow][i],j);
	      }
	  currentRow++;
	}
      else
	if(returnIfZeroDeterminant)return -1;
    }

  return retSwaps;
}


  typ reduceAndComputeDeterminant()
{
  assert(height==width);
  int swaps=reduce(false);

  int r=reduceAndComputeRank();
  //cerr<<*this;
  if(r!=height)return 0;

  typ ret=(1-2*(swaps&1));

  int pivotI=-1;
  int pivotJ=-1;
  while(nextPivot(pivotI,pivotJ))ret=ret*(*this)[pivotI][pivotJ];
  return ret;
}
  void cycleColumnsLeft(int offset);

  /**
     Takes the matrix from row echelon form to reduced row echelon form.
   */
  void REformToRREform(bool scalePivotsToOne=false) //!!! next function to rewrite
  {
    int pivotI=-1;
    int pivotJ=-1;
    while(nextPivot(pivotI,pivotJ))
      {
	if(scalePivotsToOne)
	  (*this)[pivotI].set((1/(*this)[pivotI][pivotJ])*(*this)[pivotI].toVector());
	//  cerr<<*this;
	for(int i=0;i<pivotI;i++)
	  multiplyAndAddRow(pivotI,-((*this)[i][pivotJ])/((*this)[pivotI][pivotJ]),i);
    }
  }

  Matrix submatrix(int startRow, int startColumn, int endRow, int endColumn)const
  {
    TEST assert(startRow>=0);
    TEST assert(startColumn>=0);
    TEST assert(endRow>=startRow);
    TEST assert(endColumn>=startColumn);
    TEST assert(endRow<=height);
    TEST assert(endColumn<=width);
    Matrix ret(endRow-startRow,endColumn-startColumn);
    for(int i=startRow;i<endRow;i++)
      {
	/*for(int j=startColumn;j<endColumn;j++)
	  ret[i-startRow][j-startColumn]=(*this)[i][j];*/
	const typ * __restrict__ src=data+width*i+startColumn;
	typ * __restrict__ dest=ret.data+ret.width*(i-startRow);
	for(int j=endColumn-startColumn;j>0;j--)
	  *(dest++)=*(src++);
      }
    return ret;
  }
  void removeZeroRows()
{
  int n=0;
  for(int i=0;i<height;i++)
    {
      bool isZer=true;
      for(int j=0;j<width;j++)if(!isZero((*this)[i][j]))isZer=false;
      if(!isZer)n++;
    }
  Matrix ret(n,width);
  n=0;
  for(int i=0;i<height;i++)
    {
      bool isZer=true;
      for(int j=0;j<width;j++)if(!isZero((*this)[i][j]))isZer=false;
      if(!isZer)ret[n++].set((*this)[i].toVector());
    }
  *this=ret;
}

  Matrix inverse()const

{
  //  cerr<<"THIS"<<*this;
  assert(height==width);
  Matrix temp=combineOnTop(transposed(),identity(height)).transposed();
  // cerr<<"TEMP"<<temp;
  temp.reduce(false);
  //cerr<<"TEMP"<<temp;
  temp.REformToRREform(true/*!!!LType::isField()*/);
  //cerr<<"TEMP"<<temp;
  Matrix ret=temp.submatrix(0,height,height,2*height);
  ///cerr<<"RET"<<ret;
  //cerr<<"PROD"<<ret*(*this);
  return ret;    
}

  /*
    Returns a matrix with those columns removed, that would not contain a pivot in a row Echelon form.
   */
  Matrix reduceDimension()const
  {
    Matrix temp=*this;
    temp.reduce(false);
    
    //  cerr<<"IN"<<*this;
    
    int d=temp.numberOfPivots();
    Matrix ret(height,d);
    
    int pivotI=-1;
    int pivotJ=-1;
    int i=0;
    while(temp.nextPivot(pivotI,pivotJ))
      {
	for(int k=0;k<height;k++)ret[k][i]=(*this)[k][pivotJ];
	i++;
      }
    //  cerr<<"OUT"<<ret;
    return ret;
  }

  void maddRowToVector(int row, typ scalar, Vector<typ> &v)
  {
    TEST assert(width==v.size());
    int offset=width*row;
    /*    if(0)
      for(int i=0;i<width;i++)v.data[i]+=scalar*data[offset++];
      else*/
      {
	const typ* __restrict__ src=data+offset;
	typ* __restrict__ dest=v.data;
	int w=width;
	/*	int w2=width&(-2);
	int i;
	for(i=0;i<w2;i+=2)
	  {
	    dest[i]+=scalar*src[i];
	    dest[i+1]+=scalar*src[i+1];
	  }
	  for(;i<w;i++)dest[i]+=scalar*src[i];*/

	for(int i=0;i<w;i++)dest[i]+=scalar*src[i];//29.46
	//	while(w-->0)(*(dest++))+=scalar*(*(src++));//30.66
      }
    //    v.madd(scalar,(*this)[row].toVector());
  }
#if 0
  MY_INLINE unsigned char hashValue(int row, int numberOfEntriesToConsider)const
  {
    unsigned char ret=0;
    typ *d=data+row*width;
    for(int i=0;i<numberOfEntriesToConsider;i++)
      ret+=((unsigned char*)(d+i))[6];
    return ret;
  }
#else
  /**
     We use the second int word of the data, since typ most likely is a double, and the interesting part of a 64bit double happens in those 32 bit.
   */
  MY_INLINE unsigned char hashValue(int row, int numberOfEntriesToConsider)const
  {
    int ret=0;
    typ *d=data+row*width;
    for(int i=0;i<numberOfEntriesToConsider;i++)
      ret=((int*)(d+i))[1]+(ret>>7)+(ret<<25);
    return ret+(ret>>24)+(ret>>16)+(ret>>8);
  }
  MY_INLINE unsigned char hashValueOfNegative(int row, int numberOfEntriesToConsider)const
  {
    int ret=0;
    typ *d=data+row*width;
    for(int i=0;i<numberOfEntriesToConsider;i++)
      {
	typ temp=-d[i];
	ret=((int*)(&temp))[1]+(ret>>7)+(ret<<25);
      }
    return ret+(ret>>24)+(ret>>16)+(ret>>8);
  }
#endif
  bool rowsAreEqual(int row1, int row2, int numberOfEntriesToConsider)
  {
    typ *r1=data+row1*width;
    typ *r2=data+row2*width;
    for(int i=0;i<numberOfEntriesToConsider;i++)
      {
	if(!isZero(r1[i]-r2[i]))return false;
      }
    return true;
  }


};

typedef Matrix<double> MatrixDouble;
};
#endif
