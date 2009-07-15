#include <libxml/parser.h>
#include <libxml/tree.h>

struct xml_node { xmlDocPtr doc; xmlNodePtr node; };
struct xml_attr { xmlDocPtr doc; xmlAttrPtr attr; };
extern struct xml_node *xml_Parse(M2_string);
extern struct xml_attr *xml_Attributes(struct xml_node *);
extern int xml_isElement(struct xml_node *);
extern int xml_isText(struct xml_node *);
extern M2_string xml_getElementName(struct xml_node *);
extern M2_string xml_getContent(struct xml_node *);
extern struct xml_node *xml_getNextNode(struct xml_node *);
extern struct xml_attr *xml_getNextAttr(struct xml_attr *);
extern struct xml_node *xml_getNodeChildren(struct xml_node *);
extern struct xml_node *xml_getAttrChildren(struct xml_attr *);
