#ifndef VEKTOR_H_INCLUDED
#define VEKTOR_H_INCLUDED

//#include <cmath>
#include <vector>
#include <list>
#include <assert.h>
#include <algorithm>

using namespace std;

typedef signed long long int64;

//#define VECTORMALLOC(x) malloc(x)
//#define VECTORFREE(x) free(x)

// !!! Can't be used for more advanced classes !!! 
#define VECTORMALLOC(x) mixedCells::allocator.alloc(x)
#define VECTORFREE(x) mixedCells::allocator.free(x)


void outOfRange(int i, int n);

template <class typ> class Vektor{
public:	
  typ *data;
  int length;
public:
 Vektor(const Vektor &a):
  length(a.length)
    {
      data=(typ*)VECTORMALLOC(length*sizeof(typ));
      for(int i=0;i<length;i++)	new (data+i) typ(a.data[i]);
      //data[i]=a.data[i];
    }
 Vektor(int n):
  length(n)
  {
    assert(n>=0);
    data=(typ*)VECTORMALLOC(length*sizeof(typ));
    for(int i=0;i<length;i++)	new (data+i) typ();
    //for(int i=0;i<length;i++)data[i]=0;
  };
 Vektor(const typ *p, int n):
  length(n)
  { 
    assert(n>=0);
    data=(typ*)VECTORMALLOC(length*sizeof(typ));
    for(int i=0;i<length;i++) new (data+i) typ(p[i]);
    //for(int i=0;i<length;i++)data[i]=p[i];
  };
  
  ~Vektor(){
    if(data)
      {
	for(int i=0;i<length;i++) data[i].~typ();
	VECTORFREE(data);
	data=0;
      }
  };
 Vektor():
  length(0),
    data(0)
    {
  };

  Vektor& operator=(const Vektor& v)
    {
      if(this==&v)
	{
	  return this;
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
		  for(int i=0;i<length;i++) new (data+i) typ();
		}
	    }
	  else
	    {
	      length=v.length;
	      data=(typ*)VECTORMALLOC(length*sizeof(typ));
	      for(int i=0;i<length;i++) new (data+i) typ();
	    }
	  for(int i=0;i<length;i++)data[i]=v.data[i];
	}
      return &(*this);
    }

  static Vektor standardVector(int n, int i)
    {
      Vektor v(n);
      v[i]=typ(1.0);
      return v;
    }

  static Vektor allOnes(int n)
    {
      Vektor v(n);
      for(int i=0;i<n;i++)
	v[i]=typ(1.0);
      return v;
    }

  /*  template<class T>
    Vektor(const Vektor<T>& c):v(c.size()){
    for(int i=0;i<size();i++)v[i]=typ(c[i]);}
  */
  typ& operator[](int n)
    {
      if(!(n>=0 && n<length))outOfRange(n,length);
      return (data[n]);
    }
  const typ& operator[](int n)const{assert(n>=0 && n<length);return (data[n]);}
	
  unsigned int size()const{return length;};
  typ sum()const{typ f=0;for(int i=0;i<length;i++)f+=data[i];return f;};
  //  void resize(int n){v.resize(n,0);};
  //void grow(int i){if(size()<i)resize(i);}

  inline friend Vektor operator-(const Vektor& q){
    Vektor ret(q.size());
    for(int i=0;i<q.size();i++) 
      ret[i]=-q[i];
    return ret;
  };
  inline friend Vektor operator*(typ s, const Vektor& q){Vektor p=q;for(int i=0;i<q.size();i++)p[i]*=s;return p;}
  inline friend Vektor operator/(const Vektor& q, typ s){Vektor p=q;for(int i=0;i<q.size();i++)p[i]/=s;return p;}
  inline friend Vektor operator*(const Vektor& p, const Vektor& q){assert(p.size()==q.size());Vektor p1=p;for(int i=0;i<p.size();i++)p1.v[i]*=q.v[i];return p1;}
  inline friend Vektor operator+(const Vektor& p, const Vektor& q){assert(p.size()==q.size());Vektor p1=p;for(int i=0;i<p.size();i++)p1[i]+=q[i];return p1;}
  inline friend Vektor operator-(const Vektor& p, const Vektor& q){assert(p.size()==q.size());Vektor p1=p;for(int i=0;i<p.size();i++)p1[i]-=q[i];return p1;}
  
  inline friend typ dot(const Vektor& p, const Vektor& q){assert(p.size()==q.size());typ s=0;for(int i=0;i<p.size();i++)s+=p[i]*q[i];return s;}
  inline friend int64 dotLong(const Vektor& p, const Vektor& q){assert(p.size()==q.size());int64 s=0;for(int i=0;i<p.size();i++)s+=(int64)p[i]*(int64)q[i];return s;}
  
  Vektor& operator+=(const Vektor& q){assert(size()==q.size());for(int i=0;i<size();i++)data[i]+=q.data[i];return *this;}
  Vektor& operator-=(const Vektor& q){assert(size()==q.size());for(int i=0;i<size();i++)data[i]-=q.data[i];return *this;}
  
  bool operator==(const Vektor & q)const{if(size()!=q.size())return false;for(int i=0;i<size();i++)if(data[i]!=q[i])return false;return true;}
  bool operator!=(const Vektor & q)const {return !(operator==(q));} 
  bool operator<(const Vektor & b)const
    {
      if(size()<b.size())return true;
      if(size()>b.size())return false;
      for(int i=0;i<size();i++)
	{
	  if(data[i]<b[i])return true;
	  if(b[i]<data[i])return false;
	}
      return false;
    }


  /*  int divides(const Vektor& q) const
    {
      assert(size()==q.size());
      int n=v.size();
      for(int i=0;i<n;i++)
        {
          if(v[i]>0)if(q.v[i]<v[i])return 0;
        }
      return 1;
    }
  inline friend bool relativelyPrime(const Vektor& p, const Vektor& q)
    {
      assert(p.size()==q.size());
      int n=p.size();
      for(int t=0;t<n;t++)if((p[t]>0)&&(q[t]>0)) return false;
      return true;
      }*/
  /*  int isZero() const
    {
      int n=v.size();
      for(int i=0;i<n;i++)if(v[i]!=0)return 0;
      return 1;
    }
  int isPositive() const
    {
      int n=v.size();
      for(int i=0;i<n;i++)if(v[i]<=0)return 0;
      return 1;
    }
  int isNonNegative() const
    {
      int n=v.size();
      for(int i=0;i<n;i++)if(v[i]<0)return 0;
      return 1;
    }
  int max()const
  {
    int ret=-0x7fffffff; //not completely correct, but kind of works for 64bit
    for(int i=0;i<v.size();i++)if(ret<v[i])ret=v[i];
    return ret;
  }
  int min()const
  {
    int ret=0x7fffffff;
    for(int i=0;i<v.size();i++)if(ret>v[i])ret=v[i];
    return ret;
    }*/
  /*  friend Vektor max(const Vektor& p, const Vektor& q){assert(p.size()==q.size());Vektor p1=p;for(int i=0;i<p.size();i++)if(p1[i]<q[i])p1[i]=q[i];return p1;}
  friend Vektor min(const Vektor& p, const Vektor& q){assert(p.size()==q.size());Vektor p1=p;for(int i=0;i<p.size();i++)if(p1[i]>q[i])p1[i]=q[i];return p1;}
  friend bool dependent(const Vektor& p, const Vektor& q)
    {
      typ pp=dot(p,p);
      typ qq=dot(q,q);
      typ pq=dot(p,q);
      return pq*pq==pp*qq;
    }
  Vektor supportVector()const
    {
      Vektor r(v.size());
      for(int i=0;i<size();i++)
	r[i]=(v[i]!=0);
      return r;
      }*/
  Vektor subvector(int begin, int end)const
    {
      assert(begin>=0);
      assert(end<=size());
      assert(end>=begin);
      Vektor ret(end-begin);
      for(int i=0;i<end-begin;i++)
	ret[i]=data[begin+i];
      return ret;
    }
  Vektor subvector(list<int> const &chosenIndices)const
  {
    Vektor ret(chosenIndices.size());
    int I=0;
    for(list<int>::const_iterator i=chosenIndices.begin();i!=chosenIndices.end();i++,I++)ret[I]=data[*i];
    return ret;
  }
  /*  void sort()
    {
      std::sort(v.begin(),v.end());
      }*/
  /*  bool nextPermutation()
    {
      return std::next_permutation(v.begin(),v.end());
      }*/
  /*  int indexOfLargestNonzeroEntry()const
  {
    int ret=-1;
    for(int i=0;i<v.size();i++)
      {
	if(v[i])ret=i;
      }
    return ret;
  }
  Vektor supportIndices()const
  {
    Vektor ret(0);
    for(int i=0;i<v.size();i++)
      if(v[i]!=0)ret.push_back(i);
    return ret;
  }
  void push_back(typ a)
  {
    v.push_back(a);
    }*/
  /*  friend Vektor concatenation(Vektor const &a, Vektor const &b)
  {
    Vektor ret(a.size()+b.size());
    for(int i=0;i<a.size();i++)ret[i]=a[i];
    for(int i=0;i<b.size();i++)ret[i+a.size()]=b[i];
    return ret;
    }*/
};

//typedef Vektor<double> FloatVector;
typedef Vektor<int> IntegerVector;
typedef list<IntegerVector> IntegerVectorList;

IntegerVectorList transposeIntegerVectorList(IntegerVectorList const &l);
IntegerVectorList multiplyIntegerVectorList(IntegerVectorList const &A, IntegerVectorList const &B);
IntegerVectorList subvectorsOfIntegerVectorList(IntegerVectorList const &l, list<int> const &chosen);
int gcdOfVector(IntegerVector const &v);
IntegerVector normalized(IntegerVector v);

#endif



int gcd(int r, int s);

