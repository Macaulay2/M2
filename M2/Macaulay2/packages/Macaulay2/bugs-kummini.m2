-- Place bugs that you find in here, and
-- then check in the file, using (outside of M2):
--   cvs ci -m "another bug"

listUserSymbols returns the following error:
stdio:36:1:(1): after eval: expected a list, hash table, or sequence
stdio:36:1:(1): before print: --backtrace update-- 
stdio:36:1:(1): at print: --backtrace update-- 

o36 : 
stdio:36:1:(1): after print: --backtrace update-- 

This happened at Wed May  4 16:13:43 PDT 2005


2. A reference to [minors, First] is generated in the documentation from the code. 
Wed May  4 17:06:00 PDT 2005

3. clearAll results in errors; e.g.,
i16 : clearAll
stdio:16:1:(1): after eval: expected a list, hash table, or sequence
stdio:16:1:(1): before print: --backtrace update-- 
stdio:16:1:(1): at print: --backtrace update-- 

o16 : 
stdio:16:1:(1): after print: --backtrace update-- 

i17 : 

Probably this is related to the error in listUserSymbols.
Thu May  5 09:08:13 PDT 2005

4. The key Undocumented in the document file generates some error. This should
be fixed for the documentation of char.


