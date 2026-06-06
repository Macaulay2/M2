#include <assert.h>
#include <stdio.h>
#include <string.h>

#include <M2/config.h>

#include "M2mem.h"
#include "xml-exports.h"
#include "xml-c.h"

#ifdef WITH_XML

static char *copystring(const char *s) {
  char *p = (char *)getmem(strlen(s)+1);
  strcpy(p,s);
  return p;
}

static void initxml() __attribute__ ((constructor));
static void initxml() {
  xmlMemSetup((xmlFreeFunc) freemem,
	      (xmlMallocFunc) getmem,
	      (xmlReallocFunc) getmoremem1,
	      (xmlStrdupFunc) copystring);
}


void xml_examine(xmlNode *n) {
  xmlElemDump(stdout,NULL,n);
}

xmlNode *xml_Parse(M2_string p) {
     xmlDoc *d = xmlReadMemory((char *)p->array,p->len,"a string", NULL, 0);
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
  xmlOutputBuffer *buf;
  const xmlChar* content;
  int len;

  buf = xmlAllocOutputBuffer(NULL);
  xmlNodeDumpOutput(buf, n->doc, n, 2, 1, NULL);
  content = xmlOutputBufferGetContent(buf);
  len = xmlOutputBufferGetSize(buf);
  s = M2_tostringn((const char *)content, len);
  xmlOutputBufferClose(buf);

  return s;
}

M2_string xml_DocDump(xmlNode *n) {
  xmlChar *mem = 0;
  int size = 0;
  /* xmlDocDumpFormatMemory(n->doc,&mem,&size,1); */
  xmlDocDumpMemoryEnc(n->doc,&mem,&size,"UTF-8");
  M2_string s = M2_tostringn((char*)mem,size);
  xmlFree(mem);
  return s;
}

#endif /* WITH_XML */

/*
 Local Variables:
 compile-command: "make -C $M2BUILDDIR/Macaulay2/d xml-c.o "
 End:
*/
