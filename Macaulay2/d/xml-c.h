#include <libxml/parser.h>
#include <libxml/tree.h>

typedef struct xml_node { xmlDoc *doc; xmlNode *node; } xml_node;
typedef struct xml_attr { xmlDoc *doc; xmlAttr *attr; } xml_attr;
/* parsing xml */
extern xml_node *xml_Parse(M2_string);
/* examining trees */
extern xml_attr *xml_Attributes(xml_node *);
extern int xml_isElement(xml_node *);
extern int xml_isText(xml_node *);
extern M2_string xml_getElementName(xml_node *);
extern M2_string xml_getAttrName(xml_attr *);
extern M2_string xml_getContent(xml_node *);
extern xml_node *xml_getNextNode(xml_node *);
extern xml_attr *xml_getNextAttr(xml_attr *);
extern xml_node *xml_getNodeChildren(xml_node *);
extern xml_node *xml_getAttrChildren(xml_attr *);
/* building trees */
extern xml_node *xml_NewDoc(M2_string version, M2_string name); /* creates a new doc with a new root node */
extern xml_attr *xml_NewProp(xml_node *, M2_string name, M2_string value);
/* extern void xml_AddChild(xml_node *parent, xml_node *cur); */
/* extern xml_node *xml_NewNode(M2_string name); */
extern xml_node *xml_NewChild(xml_node *parent, M2_string name);
extern xml_node *xml_NewText(xml_node *parent, M2_string content);
