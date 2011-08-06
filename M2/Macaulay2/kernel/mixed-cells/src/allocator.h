static void free2(void *p){free(p);}

//static int N;
class Allocator{
  struct Chunk
  {
    bool isAllocatedInStack;//This bool has two meainings, depending on whether the chunk is on the stack or is part of area from a usual malloc
    struct Chunk *prev;
  };
  void *stack;
  Chunk *nextChunk;
  int stackSize;
  bool stackMode;
 public:
 Allocator():
  stack(0),
    stackSize(0),
    stackMode(false),
    nextChunk(0)
      {
      }
  void init(int stackSize_)
  {
    assert(stack==0);
    stackSize=stackSize_;
    stack=malloc(stackSize_);
    ((Chunk*)stack)->isAllocatedInStack=false;
    ((Chunk*)stack)->prev=0;
    nextChunk=(Chunk*)stack;
  }
  void deinit()
  {
    assert(((void*)nextChunk)==stack);
    if(stack)free2(stack);
    stack=0;
  }
  void setStackMode(bool mode){stackMode=mode;}
  MY_INLINE void *alloc(int s)
  {
    //std::cerr<<"ALLOC-";
    if(stackMode && (((char*)nextChunk-(char*)stack)+s+2*sizeof(Chunk)<=stackSize))
      {
      //      N++;
      //{static int t;if(!((t++)&65535))std::cerr<<N<<std::endl;}
	nextChunk->isAllocatedInStack=true;
	Chunk *ret=(Chunk*)(((char*)nextChunk)+sizeof(Chunk)+s);
	ret->prev=nextChunk;
	nextChunk=ret;
	return (void*)(ret->prev+1);
      }
    else
      {
	void *ret=malloc(s+sizeof(Chunk));
	((Chunk*)ret)->isAllocatedInStack=false;
	return ((char*)ret)+sizeof(Chunk);
      }
  }
  MY_INLINE void free(void *p)
  {
    if((((Chunk*)p)-1)->isAllocatedInStack)
      {
	(((Chunk*)p)-1)->isAllocatedInStack=false;
	while(nextChunk->prev && !nextChunk->prev->isAllocatedInStack){nextChunk=nextChunk->prev;/*N--;*/}
      }
      else
      {
	free2(((Chunk*)p)-1);
      }
  }
};

Allocator allocator;
