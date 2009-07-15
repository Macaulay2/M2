#include <stdio.h>
#include <libxml/parser.h>
#include <libxml/tree.h>
#include <string.h>
#include "M2mem.h"
#include "M2types.h"

static char *copystring(const char *s) {
  char *p = (char *)getmem(strlen(s)+1);
  strcpy(p,s);
  return p;
}

void initxml() __attribute__ ((constructor));
void initxml() {
  xmlGcMemSetup(freemem,(void *(*)(size_t))getmem,(void *(*)(size_t))getmem_atomic,(void *(*)(void *,size_t))getmoremem1,copystring);
}

static void display(xmlNode *u) {
  xmlNode *t = u;
  if (t == NULL) return;
  for (; t; t = t->next) if (t->type == XML_ELEMENT_NODE) printf("%s\n", t->name);
  display(u->children);
}

void xml_parse(M2_string p) {
  xmlDocPtr doc = xmlReadMemory(p->array,p->len,"a string", NULL, 0);
  if (doc == NULL) {
    printf("xml syntax error\n");
  }
  else {
    display(xmlDocGetRootElement(doc));
    printf("\n");
    xmlFreeDoc(doc);
  }
}

/*
 Local Variables:
 compile-command: "make -C $M2BUILDDIR/Macaulay2/d xml-c.o "
 End:
*/
