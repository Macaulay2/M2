#include <assert.h>
#include <stdio.h>
#include <string.h>
#include "M2mem.h"
#define TRUE 1
#define FALSE 0
#include "xml-exports.h"
#include <xml-c.h>

static char *copystring(const char *s) {
  char *p = (char *)getmem(strlen(s)+1);
  strcpy(p,s);
  return p;
}

static void initxml() __attribute__ ((constructor));
static void initxml() {
  xmlGcMemSetup(freemem,(void *(*)(size_t))getmem,(void *(*)(size_t))getmem_atomic,(void *(*)(void *,size_t))getmoremem1,copystring);
}

#if 0
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
    case XML_TEXT_NODE: printf("XML_TEXT_NODE: %s",t->content); break;
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

static void show(xmlDocPtr doc,xmlNodePtr t) {
  for (;; t = t->next) {
    if (t == NULL) return;
    switch (t->type) {
    case XML_ELEMENT_NODE: {
      xmlAttrPtr a = t->properties;
      printf("<%s", t->name);
      for (;a;a=a->next) {
	printf(" ");
	printf("%s=",a->name);
	show(doc,a->children);
      }
      printf(">");
      show(doc,t->children);
      printf("</%s>", t->name);
      break;
    }
    case XML_TEXT_NODE: printf("\"%s\"",t->content); break;
    default:;
    }
  }
}

#endif

/* void xml_parse(M2_string p) { */
/*   xmlDocPtr doc = xmlReadMemory(p->array,p->len,"a string", NULL, 0); */
/*   if (doc == NULL) { */
/*     printf("xml syntax error\n"); */
/*   } */
/*   else { */
/*     printf("document: "); */
/*     xmlDocDump(stdout,doc); */
/*     printf("\n"); */
/*     show(doc,xmlDocGetRootElement(doc)); */
/*     printf("\n"); */
/*     xmlFreeDoc(doc); */
/*   } */
/* } */

void xml_examine(xmlNode *n) {
  xmlElemDump(stdout,n->doc,n);
}

xmlNode *xml_Parse(M2_string p) {
  xmlDoc *d = xmlReadMemory(p->array,p->len,"a string", NULL, 0);
  if (d == NULL) return NULL;
  return xmlDocGetRootElement(d);
}

xmlNode *xml_NewRoot(M2_string version, M2_string name) {
  char *s = M2_tocharstar(name);
  char *v = M2_tocharstar(version);
  xmlDocPtr doc = xmlNewDoc((unsigned const char*)v);
  xmlNode *n = xmlNewNode(NULL,(unsigned const char*)s);
  xmlDocSetRootElement(doc, n);
  freemem(s);
  freemem(v);
  return n;
}

xmlAttr *xml_AddAttribute(xmlNode *n, M2_string name, M2_string value){
  char *nam = M2_tocharstar(name), *val = M2_tocharstar(value);
  xmlAttr *a = xmlNewProp(n,(unsigned const char*)nam,(unsigned const char*)val);
  freemem(nam), freemem(val);
  return a;
}

xmlNode *xml_AddElement(xmlNode *parent, M2_string name){
  char *nam = M2_tocharstar(name);
  xmlNode *r = xmlNewChild(parent,NULL,(unsigned const char*)nam,NULL);
  freemem(nam);
  return r;
}

xmlNode *xml_AddText(xmlNode *parent, M2_string content){
  char *cont = M2_tocharstar(content);
  xmlNode *r = xmlNewText((unsigned const char*)cont);
  xmlAddChild(parent,r);
  freemem(cont);
  return r;
}

M2_string xml_toString(xmlNode *n) {
  M2_string s;
  xmlBuffer *buf = xmlBufferCreate();
  int len = xmlNodeDump(buf,n->doc,n,2,TRUE);
  s = M2_tostringn((char*)buf->content,len);
  xmlBufferFree(buf);
  return s;
}

M2_string xml_DocDump(xmlNode *n) {
  xmlChar *mem = 0;
  int size = 0;
  /* xmlDocDumpFormatMemory(n->doc,&mem,&size,TRUE); */
  xmlDocDumpMemoryEnc(n->doc,&mem,&size,"UTF-8");
  M2_string s = M2_tostringn((char*)mem,size);
  xmlFree(mem);
  return s;
}

/*
 Local Variables:
 compile-command: "make -C $M2BUILDDIR/Macaulay2/d xml-c.o "
 End:
*/
