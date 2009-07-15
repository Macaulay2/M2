#include <stdio.h>
#include <string.h>
#include "M2mem.h"
#include "M2types.h"
#include "xml-c.h"

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

void xml_parse(M2_string p) {
  xmlDocPtr doc = xmlReadMemory(p->array,p->len,"a string", NULL, 0);
  if (doc == NULL) {
    printf("xml syntax error\n");
  }
  else {
    printf("document: ");
    xmlDocDump(stdout,doc);
    printf("\n");
    show(doc,xmlDocGetRootElement(doc));
    printf("\n");
    xmlFreeDoc(doc);
  }
}

struct xml_node *xml_Parse(M2_string p) {
  struct xml_node *r = (struct xml_node *)getmem(sizeof(*r));
  r->doc = xmlReadMemory(p->array,p->len,"a string", NULL, 0);
  r->node = xmlDocGetRootElement(r->doc);
  return r;
}

struct xml_attr *xml_Attributes(struct xml_node *n) {
  struct xml_attr *r;
  if (n->node->type != XML_ELEMENT_NODE || n->node->properties == NULL) return NULL;
  r = (struct xml_attr *)getmem(sizeof(*r));
  r->doc = n->doc;
  r->attr = n->node->properties;
  return r;
}

int xml_isElement(struct xml_node *n){
  return n->node->type == XML_ELEMENT_NODE;
}

int xml_isText(struct xml_node *n){
  return n->node->type == XML_TEXT_NODE;
}

M2_string xml_getElementName(struct xml_node *n){
  return tostring((char const *)n->node->name);
}

struct xml_node *nxml_getNextNode(struct xml_node *n){
  struct xml_node *r;
  if (n->node->next == NULL) return NULL;
  r = (struct xml_node *)getmem(sizeof(*r));
  r->doc = n->doc;
  r->node = n->node->next;
  return r;
}

struct xml_attr *axml_getNextAttr(struct xml_attr *a){
  struct xml_attr *r;
  if (a->attr->next == NULL) return NULL;
  r = (struct xml_attr *)getmem(sizeof(*r));
  r->doc = a->doc;
  r->attr = a->attr->next;
  return r;
}

struct xml_node *nxml_getNodeChildren(struct xml_node *n){
  struct xml_node *r;
  if (n->node->children == NULL) return NULL;
  r = (struct xml_node *)getmem(sizeof(*r));
  r->doc = n->doc;
  r->node = n->node->children;
  return r;
}

struct xml_node *nxml_getAttrChildren(struct xml_attr *a){
  struct xml_node *r;
  if (a->attr->children == NULL) return NULL;
  r = (struct xml_node *)getmem(sizeof(*r));
  r->doc = a->doc;
  r->node = a->attr->children;
  return r;
}


/*
 Local Variables:
 compile-command: "make -C $M2BUILDDIR/Macaulay2/d "
 End:
*/
