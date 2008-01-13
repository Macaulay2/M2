extern void M2inits(void);
void M2inits1(void) __attribute__ ((constructor));
void M2inits1(void) { M2inits(); }
void M2inits2(void) __attribute__ ((constructor));
void M2inits2(void) { M2inits(); }
