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

static int level = 0;
static void indent() { int i; for (i=2*level; i; i--) putchar(' '); }
static void display(xmlDocPtr doc,xmlNodePtr t) {
  level++;
  for (;; t = t->next) {
    if (t == NULL) {
      level--;
      return;
    }
    indent();
    switch (t->type) {
    case XML_ELEMENT_NODE: {
      /* xmlElementPtr e = (xmlElementPtr)t; */
      xmlAttrPtr a = t->properties;
      level++;
      printf("XML_ELEMENT_NODE: "); 
      printf("%s:", t->name);
      for (;a;a=a->next) {
	printf("\n");
	indent();
	printf("%s=:\n",a->name);
	display(doc,a->children);
      }
      level--;
      break;
    }
    case XML_ATTRIBUTE_NODE: printf("XML_ATTRIBUTE_NODE: "); break;
    case XML_TEXT_NODE: printf("XML_TEXT_NODE: "); break;
    case XML_CDATA_SECTION_NODE: printf("XML_CDATA_SECTION_NODE: "); break;
    case XML_ENTITY_REF_NODE: printf("XML_ENTITY_REF_NODE: "); break;
    case XML_ENTITY_NODE: printf("XML_ENTITY_NODE: "); break;
    case XML_PI_NODE: printf("XML_PI_NODE: "); break;
    case XML_COMMENT_NODE: printf("XML_COMMENT_NODE: "); break;
    case XML_DOCUMENT_NODE: printf("XML_DOCUMENT_NODE: "); break;
    case XML_DOCUMENT_TYPE_NODE: printf("XML_DOCUMENT_TYPE_NODE: "); break;
    case XML_DOCUMENT_FRAG_NODE: printf("XML_DOCUMENT_FRAG_NODE: "); break;
    case XML_NOTATION_NODE: printf("XML_NOTATION_NODE: "); break;
    case XML_HTML_DOCUMENT_NODE: printf("XML_HTML_DOCUMENT_NODE: "); break;
    case XML_DTD_NODE: printf("XML_DTD_NODE: "); break;
    case XML_ELEMENT_DECL: printf("XML_ELEMENT_DECL: "); break;
    case XML_ATTRIBUTE_DECL: printf("XML_ATTRIBUTE_DECL: "); break;
    case XML_ENTITY_DECL: printf("XML_ENTITY_DECL: "); break;
    case XML_NAMESPACE_DECL: printf("XML_NAMESPACE_DECL: "); break;
    case XML_XINCLUDE_START: printf("XML_XINCLUDE_START: "); break;
    case XML_XINCLUDE_END: printf("XML_XINCLUDE_END: "); break;
    case XML_DOCB_DOCUMENT_NODE: printf("XML_DOCB_DOCUMENT_NODE: "); break;
    default: printf("unknown node type");
    }
    printf("\n");
    display(doc,t->children);
  }
}

void xml_parse(M2_string p) {
  xmlDocPtr doc = xmlReadMemory(p->array,p->len,"a string", NULL, 0);
  if (doc == NULL) {
    printf("xml syntax error\n");
  }
  else {
    printf("document: ");
    xmlDocDump(stdout,doc);
    printf("\n");
    display(doc,xmlDocGetRootElement(doc));
    xmlFreeDoc(doc);
  }
}

/*
 Local Variables:
 compile-command: "make -C $M2BUILDDIR/Macaulay2/d "
 End:
*/
