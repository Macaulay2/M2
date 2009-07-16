#include <stdio.h>
#include <string.h>
#include "M2mem.h"
#include "M2types.h"
#include "xml-c.h"
#define TRUE 1
#define FALSE 0

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

void xml_examine(xml_node *n) {
  xmlElemDump(stdout,n->doc,n->node);
}

xml_node *xml_Parse(M2_string p) {
  xmlDoc *d = xmlReadMemory(p->array,p->len,"a string", NULL, 0);
  xml_node *r;
  if (d == NULL) return NULL;
  r = (xml_node *)getmem(sizeof(*r));
  r->doc = d;
  r->node = xmlDocGetRootElement(r->doc);
  return r;
}

xml_attr *xml_Attributes(xml_node *n) {
  xml_attr *r;
  if (n->node->type != XML_ELEMENT_NODE || n->node->properties == NULL) return NULL;
  r = (xml_attr *)getmem(sizeof(*r));
  r->doc = n->doc;
  r->attr = n->node->properties;
  return r;
}

int xml_isElement(xml_node *n){
  return n->node->type == XML_ELEMENT_NODE;
}

int xml_isText(xml_node *n){
  return n->node->type == XML_TEXT_NODE;
}

M2_string xml_getElementName(xml_node *n){
  if (n->node->name == NULL) return NULL;
  return tostring((char const *)n->node->name);
}

M2_string xml_getAttrName(xml_attr *a){
  if (a->attr->name == NULL) return NULL;
  return tostring((char const *)a->attr->name);
}

M2_string xml_getContent(xml_node *n){
  if (n->node->content == NULL) return NULL;
  return tostring((char const *)n->node->content);
}

xml_node *xml_getNextNode(xml_node *n){
  xml_node *r;
  if (n->node->next == NULL) return NULL;
  r = (xml_node *)getmem(sizeof(*r));
  r->doc = n->doc;
  r->node = n->node->next;
  return r;
}

xml_attr *xml_getNextAttr(xml_attr *a){
  xml_attr *r;
  if (a->attr->next == NULL) return NULL;
  r = (xml_attr *)getmem(sizeof(*r));
  r->doc = a->doc;
  r->attr = a->attr->next;
  return r;
}

xml_node *xml_getNodeChildren(xml_node *n){
  xml_node *r;
  if (n->node->children == NULL) return NULL;
  r = (xml_node *)getmem(sizeof(*r));
  r->doc = n->doc;
  r->node = n->node->children;
  return r;
}

xml_node *xml_getAttrChildren(xml_attr *a){
  xml_node *r;
  if (a->attr->children == NULL) return NULL;
  r = (xml_node *)getmem(sizeof(*r));
  r->doc = a->doc;
  r->node = a->attr->children;
  return r;
}

xml_node *xml_NewDoc(M2_string version, M2_string name) {
  xml_node *r = (xml_node *)getmem(sizeof(*r));
  char *s = tocharstar(name);
  r->doc = xmlNewDoc((unsigned const char*)"1.0");
  r->node = xmlNewNode(NULL,(unsigned const char*)s);
  xmlDocSetRootElement(r->doc, r->node);
  GC_FREE(s);
  return r;
}

xml_attr *xml_NewProp(xml_node *n, M2_string name, M2_string value){
  xml_attr *r = (xml_attr *)getmem(sizeof(*r));
  char *nam = tocharstar(name), *val = tocharstar(value);
  r->doc = n->doc;
  r->attr = xmlNewProp(n->node,(unsigned const char*)nam,(unsigned const char*)val);
  GC_FREE(nam), GC_FREE(value);
  return r;
}

/* void xml_AddChild(xml_node *parent, xml_node *cur){ */
/*   xmlAddChild(parent->node,cur->node); */
/* } */

/* xml_node *xml_NewNode(xml_node n,M2_string name){ /\* n is any node in the document to which we will attach the new node *\/ */
/*   char *nam = tocharstar(name); */
/*   xml_node *r = (xml_node *)getmem(sizeof(*r)); */
/*   r->doc = n->doc; */
/*   r->node = xmlNewNode(NULL,(unsigned const char*)nam); */
/*   GC_FREE(nam); */
/*   return r; */
/* } */

xml_node *xml_NewChild(xml_node *parent, M2_string name){
  char *nam = tocharstar(name);
  xml_node *r = (xml_node *)getmem(sizeof(*r));
  r->doc = parent->doc;
  r->node = xmlNewChild(parent->node,NULL,(unsigned const char*)nam,NULL);
  GC_FREE(nam);
  return r;
}

xml_node *xml_NewText(xml_node *parent, M2_string content){
  char *cont = tocharstar(content);
  xml_node *r = (xml_node *)getmem(sizeof(*r));
  r->doc = parent->doc;
  r->node = xmlNewText((unsigned const char*)cont);
  xmlAddChild(parent->node,r->node);
  GC_FREE(cont);
  return r;
}

M2_string xml_toString(xml_node *n) {
  M2_string s;
  xmlBuffer *buf = xmlBufferCreate();
  int len = xmlNodeDump(buf,n->doc,n->node,2,TRUE);
  if (len == 0) return NULL;
  s = tostringn((const char*)buf->content,len);
  xmlBufferFree(buf);
  return s;
}

/*
 Local Variables:
 compile-command: "make -C $M2BUILDDIR/Macaulay2/d "
 End:
*/
