#ifdef __MINGW32__
  #define clearThread(t) t.p = NULL, t.x = 0
  static inline int operator==(pthread_t t, int zero) { return t.p == (void *)zero; }
  static inline int operator==(pthread_t t, pthread_t u) { return t.p == u.p && t.x == u.x; }
  static inline int operator!=(pthread_t t, pthread_t u) { return t.p != u.p || t.x != u.x; }
  static inline int operator< (pthread_t t, pthread_t u) { return t.p  < u.p || t.p == u.p && t.x < u.x; }
#else
  #define clearThread(t) t = 0
#endif
