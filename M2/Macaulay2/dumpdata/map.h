typedef char bool;

typedef struct MAP {
  /* these fields filled in by machine dependent code */
  void *from, *to;
  bool r, w, x;
  /* extra fields ignored by machine dependent code, filled in later */
  unsigned int checksum;
} *map;

extern int nummaps();
extern int getmaps(int nmaps, struct MAP[nmaps]);
