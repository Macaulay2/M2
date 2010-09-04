#include <libxml/parser.h>
#include <libxml/tree.h>

/* parsing xml */
extern xmlNode *xml_Parse(M2_string);
/* examining trees */
extern xmlAttr *xml_Attributes(xmlNode *);
extern M2_string xml_getElementName(xmlNode *);
extern M2_string xml_getAttrName(xmlAttr *);
extern M2_string xml_getContent(xmlNode *);
extern xmlNode *xml_getNextNode(xmlNode *);
extern xmlAttr *xml_getNextAttr(xmlAttr *);
extern xmlNode *xml_getNodeChildren(xmlNode *);
extern xmlNode *xml_getAttrChildren(xmlAttr *);
/* building trees */
extern xmlNode *xml_NewDoc(M2_string version, M2_string name); /* creates a new doc with a new root node */
extern xmlAttr *xml_NewProp(xmlNode *, M2_string name, M2_string value);
/* extern void xml_AddChild(xmlNode *parent, xmlNode *cur); */
/* extern xmlNode *xml_NewNode(M2_string name); */
extern xmlNode *xml_NewChild(xmlNode *parent, M2_string name);
extern xmlNode *xml_NewText(xmlNode *parent, M2_string content);
M2_string xml_toString(xmlNode *);

/*
 Local Variables:
 compile-command: "make -C $M2BUILDDIR/Macaulay2/d xml-c.o "
 End:
*/
