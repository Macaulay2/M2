-- resolved --

getn = () -> (x0 := apply(10000,i->random 2^3000);)
for i from 1 to 1000 do (<< "." << flush; getn();)


-- This snippet takes 136 MB virtual memory ----
a = 2^3000
b = 10000*1000
time for i from 1 to b do random a;
-------------------------------------------------

-- This snippet takes 136 MB virtual memory ----
b = 10000*1000
time for i from 1 to b do random (2^3000);
-------------------------------------------------

-- This snippet takes large amount of ----------
-- virtual memory and doesn't finish      
a = 0..9999;
getn = () -> (x0 := apply(a,i->random 2^3000);)
for i from 1 to 1000 do getn();
-------------------------------------------------

-- This snippet takes large amount of ----------
-- virtual memory and doesn't finish      
getn = () -> (x0 := for i from 1 to 10000 list random 2^3000;)
for i from 1 to 1000 do getn();
-------------------------------------------------

Here's the fix : gmp should use GC_malloc_atomic on the limbs:

diff -ur ../4.2.1-orig/gmp-4.2.1/gmp-impl.h ./gmp-4.2.1/gmp-impl.h
--- ../4.2.1-orig/gmp-4.2.1/gmp-impl.h	2006-04-08 14:32:18.000000000 -0500
+++ ./gmp-4.2.1/gmp-impl.h	2006-07-15 14:19:06.000000000 -0500
@@ -666,6 +666,8 @@
    but current gcc (3.0) doesn't seem to support that.  */
 __GMP_DECLSPEC extern void * (*__gmp_allocate_func) __GMP_PROTO ((size_t));
 __GMP_DECLSPEC extern void * (*__gmp_reallocate_func) __GMP_PROTO ((void *, size_t, size_t));
+__GMP_DECLSPEC extern void * (*__gmp_allocate_atomic_func) __GMP_PROTO ((size_t));
+__GMP_DECLSPEC extern void * (*__gmp_reallocate_atomic_func) __GMP_PROTO ((void *, size_t, size_t));
 __GMP_DECLSPEC extern void   (*__gmp_free_func) __GMP_PROTO ((void *, size_t));
 
 void *__gmp_default_allocate _PROTO ((size_t));
@@ -674,13 +676,18 @@
 
 #define __GMP_ALLOCATE_FUNC_TYPE(n,type) \
   ((type *) (*__gmp_allocate_func) ((n) * sizeof (type)))
-#define __GMP_ALLOCATE_FUNC_LIMBS(n)   __GMP_ALLOCATE_FUNC_TYPE (n, mp_limb_t)
+#define __GMP_ALLOCATE_ATOMIC_FUNC_TYPE(n,type) \
+  ((type *) (*__gmp_allocate_atomic_func) ((n) * sizeof (type)))
+#define __GMP_ALLOCATE_FUNC_LIMBS(n)   __GMP_ALLOCATE_ATOMIC_FUNC_TYPE (n, mp_limb_t)
 
 #define __GMP_REALLOCATE_FUNC_TYPE(p, old_size, new_size, type) \
   ((type *) (*__gmp_reallocate_func)                            \
    (p, (old_size) * sizeof (type), (new_size) * sizeof (type)))
+#define __GMP_REALLOCATE_ATOMIC_FUNC_TYPE(p, old_size, new_size, type) \
+  ((type *) (*__gmp_reallocate_atomic_func)                            \
+   (p, (old_size) * sizeof (type), (new_size) * sizeof (type)))
 #define __GMP_REALLOCATE_FUNC_LIMBS(p, old_size, new_size) \
-  __GMP_REALLOCATE_FUNC_TYPE(p, old_size, new_size, mp_limb_t)
+  __GMP_REALLOCATE_ATOMIC_FUNC_TYPE(p, old_size, new_size, mp_limb_t)
 
 #define __GMP_FREE_FUNC_TYPE(p,n,type) (*__gmp_free_func) (p, (n) * sizeof (type))
 #define __GMP_FREE_FUNC_LIMBS(p,n)     __GMP_FREE_FUNC_TYPE (p, n, mp_limb_t)
diff -ur ../4.2.1-orig/gmp-4.2.1/memory.c ./gmp-4.2.1/memory.c
--- ../4.2.1-orig/gmp-4.2.1/memory.c	2006-03-14 09:57:54.000000000 -0600
+++ ./gmp-4.2.1/memory.c	2006-07-15 14:33:06.000000000 -0500
@@ -27,8 +27,11 @@
 
 
 void *	(*__gmp_allocate_func) _PROTO ((size_t)) = __gmp_default_allocate;
+void *	(*__gmp_allocate_atomic_func) _PROTO ((size_t)) = __gmp_default_allocate;
 void *	(*__gmp_reallocate_func) _PROTO ((void *, size_t, size_t))
      = __gmp_default_reallocate;
+void *	(*__gmp_reallocate_atomic_func) _PROTO ((void *, size_t, size_t))
+     = __gmp_default_reallocate;
 void	(*__gmp_free_func) _PROTO ((void *, size_t)) = __gmp_default_free;
 
 
