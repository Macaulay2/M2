#ifdef __MINGW32__
  #define clearThread(t) t.p = NULL, t.x = 0
  int operator==(pthread_t t, int zero) { return t.p == (void *)zero; }
  int operator==(pthread_t t, pthread_t u) { return t.p == u.p && t.x == u.x; }
  int operator!=(pthread_t t, pthread_t u) { return t.p != u.p || t.x != u.x; }
  int operator< (pthread_t t, pthread_t u) { return t.p  < u.p || t.p == u.p && t.x < u.x; }
#else
  #define clearThread(t) t = NULL
#endif
