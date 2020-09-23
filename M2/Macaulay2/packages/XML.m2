-- -*- coding: utf-8 -*-

newPackage("XML",
    	Version => "1.1", 
    	Date => "September 1, 2010",
    	Authors => {{Name => "Daniel R. Grayson", Email => "dan@math.uiuc.edu", HomePage => "http://www.math.uiuc.edu/~dan/"}},
    	Headline => "an XML parser",
	Keywords => {"Miscellaneous"},
    	DebuggingMode => false)
export {
     "XMLnode", "tag", "children", "parse", "toXMLnode", "Trim", "toLibxmlNode",
     "getChildren", "getAttributes","xmlTypeTable","xmlTypeDescription", "xmlTypeIndex"
     }
scan(pairs Core#"private dictionary", (k,v) -> if match("^(Lib)?xml[A-Z]",k) then (
	  XML#"private dictionary"#k = v;
	  export k))

xmlTypeTable = new HashTable from xmlTypes()
xmlTypeIndex = new HashTable from apply(xmlTypes(),(a,b) -> (b,a))

getChildren = method(TypicalValue => VerticalList)
getChildren LibxmlNode := getChildren LibxmlAttribute := node -> VerticalList (
     c := xmlFirstChild node;
     while c =!= null list first (c, c = xmlGetNext c))
getAttributes = method(TypicalValue => VerticalList)
getAttributes LibxmlNode := node -> VerticalList (
     a := xmlFirstAttribute node;
     while a =!= null list first (a, a = xmlGetNext a))

XMLnode = new Type of MutableHashTable
net XMLnode := n -> (
     attributes := concatenate apply(pairs n, (k,v) -> if instance(k,String) then (" ",k,"=",format v) else "");
     top := "<" | n.tag | attributes;
     if n.?children then (
	  cn := apply(n.children,c -> if instance(c,String) then format c else net c);
	  if #attributes === 0 and all(n.children,c -> instance(c,String)) then concatenate(top," ",between(" ",cn))
	  else stack(top,"  " | stack cn))
     else top)

nonnull = x -> select(x, i -> i =!= null)
trimwhite = s -> (
     s = replace("^[[:space:]]+","",s);
     s = replace("[[:space:]]+$","",s);
     if s =!= "" then s)
trimopt := identity					    -- not re-entrant!
settrim = opts -> trimopt = if opts.Trim then trimwhite else identity
toXMLnode0 = node -> (
     if xmlIsElement node then (
	  attr := xmlFirstAttribute node;
	  child := xmlFirstChild node;
	  new XMLnode from splice nonnull {
	       symbol tag => xmlGetName node,
	       if attr =!= null then toSequence (
		    while attr =!= null list first(
		    	 xmlGetName attr => ( c := xmlFirstChild attr; concatenate while null =!= c list first( xmlGetContent c, c = xmlGetNext c)),
		    	 attr = xmlGetNext attr)),
	       if child =!= null then symbol children => nonnull while child =!= null list first(toXMLnode0 child, child = xmlGetNext child)
	       }
	  )
     else if xmlIsText node then trimopt xmlGetContent node)
toXMLnode = method(Options => { Trim => true })
toXMLnode LibxmlNode := opts -> node -> (
     settrim opts;
     toXMLnode0 node)

net LibxmlAttribute := x -> xmlGetName x | " = " | net xmlFirstChild x

net LibxmlNode := x -> (
     if xmlIsText x 
     then net format toString x
     else net toString x)

xmlTypeDescription = method()
xmlTypeDescription LibxmlNode := t -> if xmlTypeTable#?(xmlType t) then xmlTypeTable#(xmlType t) else "unknown type"

LibxmlNode#{Standard,AfterPrint} = x -> (
     << endl				  -- double space
     << concatenate(interpreterDepth:"o") << lineNumber
     << " : " << class x << " (" << xmlTypeDescription << ")"
     << endl;
     )

toLibxmlNode = method(TypicalValue => LibxmlNode)
populate = (d,x) -> (
     scan(pairs x, (k,v) -> if instance(k,String) then xmlAddAttribute(d,k,v));
     if x.?children then scan(x.children, child -> (
	       if instance(child,String) then xmlAddText(d,child)
	       else if instance(child,XMLnode) then populate(xmlAddElement(d,child.tag),child)
	       else error "unrecognized child type"));
     d)
toLibxmlNode XMLnode := x -> populate(xmlNewRoot x.tag,x)
parse = method(Options => { Trim => true }, TypicalValue => XMLnode)
parse String := opts -> s -> toXMLnode(xmlParse s,opts)

beginDocumentation()

multidoc ///
Node
  Key
    XML
  Headline
    an XML parser
  Description
    Text

      This experimental and tentative package provides an interface to the library {\em libxml2} (see @ HREF
      "http://www.xmlsoft.org/" @), which is a parser for XML files (see
      @ HREF "http://en.wikipedia.org/wiki/XML" @).  The package offers two ways of representing the
      result: as an object of class @ TO LibxmlNode @, which is a pointer to the internal structure created and accessed by the
      library; or as a hashtable of class @ TO XMLnode @ (which currently represents only elements, attributes, and nodes).
      
      This package was written to support the packages @ TO "OpenMath::OpenMath" @ and @ TO "SCSCP::SCSCP" @.
Node
  Key
    XMLnode
  Headline
   the class of all XML trees created by the library libxml2
  Description
    Text
      An object of class @ TO XMLnode @ is a hashtable used to contain a representation of a node in an XML tree.

      Each such hashtable represents a node of the tree, in which: the keys that are strings provide the values of
      attributes; the symbol {\bf children} provides the list of children, each of which is @ ofClass{String,XMLnode} @;
      and the symbol {\bf tag} provides the name of the element.

      Currently, only element nodes and text nodes are implemented.

    Example
       new XMLnode from {
	    tag => "foo",
	    children => {
		 new XMLnode from {
		      tag => "bar",
		      children => { " chicken coop " } },
		 " hi there ",
		 new XMLnode from { tag => "bar" } } }
Node
  Key
    (parse,String)
    parse
    [parse,Trim]
  Headline
    parse XML
  Usage
    parse s
  Inputs
    s:
  Outputs
    :XMLnode
      a tree of hash tables resulting from parsing the contents of {\tt s} as XML
  Description
   Text
    This function parses XML into a tree of hash tables of type @ TO XMLnode @.
   Example
    x = parse ////<foo a="hi there">ho there<bar/></foo>////
    peek'_3 x
    x#"a"
    x.children
    class \ oo
    x.tag
Node
 Key
  LibxmlNode
 Headline
  the class of all XML nodes created by libxml2
 Description
  Text

   Each XML node created by {\tt libxml2} has: if it is an {\em element} (as determined by @ TO xmlIsElement @), an
   optional element {\em name}, which is a string and is obtained with @ TO xmlGetName @; if it is {\em text} (as
   determined by @ TO xmlIsText @), an optional {\em content} string, obtained with @ TO xmlGetContent @; a linked list of
   {\em attributes} of type @ TO LibxmlAttribute @; a linked list of {\em children} (which are XML nodes), obtained with @
   TO xmlFirstChild @; and a pointer its next {\em sibling}, obtained with @ TO xmlGetNext @.
   
   XML nodes are mutable.

   Internally, a pointer to the XML document containing the node accompanies the node.

   Let's use @ TO xmlParse @ to make an XML node.
  Example
   n = xmlParse ////<foo> aabc <bar id="foo" name="too"> asdf </bar><coo/><coo>hi</coo><coo a="b">hi</coo></foo>////
   xmlIsElement n, xmlIsText n
  Text
   Since it is an element, we may use @ TO xmlGetName @ to get its name.
  Example
   xmlGetName n
  Text
   We use @ TO xmlFirstChild @ to get the first node in the linked list of children, which happens to be text:
  Example
   c = xmlFirstChild n
   xmlIsElement c, xmlIsText c
  Text
   We may follow the linked list of children of @ TT "n" @.
  Example
   c
   bar = xmlGetNext oo
   xmlGetNext oo
   xmlGetNext oo
   xmlGetNext oo
   xmlGetNext oo
  Text
   Let's examine the attributes of @ TT "bar" @.
  Example
   xmlFirstAttribute bar
   a = xmlGetNext oo
   xmlGetNext oo
  Text
   We may disassemble an attribute as follows.
  Example
   xmlGetName a
   b = xmlFirstChild a
   xmlGetNext oo
   xmlIsText b
   toString b
  Text
   There are other functions that retrieve the entire list of attributes or children.
  Example
   getChildren n
   class \ oo
   getAttributes bar
   class \ oo
Node
 Key
  tag
 Description
  Text
   This symbol is used as a key in hash tables of type @ TO XMLnode @.
 SeeAlso
   XMLnode
   parse
   toXMLnode
Node
 Key
  children
 Description
  Text
   This symbol is used as a key in hash tables of type @ TO XMLnode @.
 SeeAlso
   XMLnode
   parse
   toXMLnode
Node
 Key
  (toXMLnode,LibxmlNode)
  toXMLnode
  [toXMLnode,Trim]
 Headline
  convert an object of type LibxmlNode to a hashtable of type XMLnode
 Usage
  toXMLnode n
 Inputs
  n:
 Outputs
  :XMLnode
   a representation of the XML tree represented by {\tt n}
  Trim => Boolean
   whether to trim leading and trailing spaces from the lines in text nodes
 Description
  Text
   This function translates an XML tree created by the libxml library into a tree of hash tables of type
   @ TO XMLnode @.
  Example
   n = xmlParse ////<foo a="hi there">  ho there  <bar/></foo>////
   x = toXMLnode n
   peek'_3 x
   x#"a"
   x.children
   class \ oo
   x.tag
   x = toXMLnode(n,Trim=>false)
 SeeAlso
  xmlParse
Node
 Key
  Trim
Node
 Key
  (toLibxmlNode,XMLnode)
  toLibxmlNode
 Headline
  convert a hashtable of type XMLnode to an object of type LibxmlNode
 Usage
  toLibxmlNode x
 Inputs
  x:
 Outputs
  :
 Description
  Example
   new XMLnode from {
	tag => "foo",
	children => {
	     new XMLnode from {
	     	  tag => "bar",
	     	  children => { " chicken coop " } },
	     " hi there ",
	     new XMLnode from { tag => "bar" } } }
   toLibxmlNode oo
Node
 Key
  (xmlIsElement,LibxmlNode)
  xmlIsElement
 Headline
  whether an XML node is an element
 Usage
  xmlIsElement n
 Inputs
  n:
 Outputs
  :Boolean
   whether {\tt n} is an element
 Description
  Example
   xmlParse ////<bar/>////
   xmlIsElement oo
 SeeAlso
  xmlIsElement
  xmlGetContent
Node
 Key
  (xmlIsText,LibxmlNode)
  xmlIsText
 Headline
  whether an XML node is text
 Usage
  xmlIsText n
 Inputs
  n:
 Outputs
  :Boolean
   whether {\tt n} is a text node
 Description
  Example
   n = xmlParse ////<bar>hi there</bar>////
   c = xmlFirstChild n
   xmlIsText c
 SeeAlso
  xmlIsElement
  xmlGetContent
Node
 Key
  (xmlParse,String)
  xmlParse
 Headline
  parse a string containing XML
 Usage
  xmlParse s
 Inputs
  s:String
 Outputs
  :LibxmlNode
 Description
  Example
   xmlParse ////<foo>aabc<bar id="foo" name="too">asdf</bar></foo>////
Node
 Key
  getChildren
  (getChildren,LibxmlAttribute)
  (getChildren,LibxmlNode)
 Headline
  get the list of children of an XML node
 Usage
  getChildren n
 Inputs
  n:
   @ ofClass{LibxmlNode,LibxmlAttribute} @
 Outputs
  :
   the list of children of {\tt n}
 Description
  Example
   xmlParse ////<foo>aabc<bar id="foo" name="too">asdf</bar></foo>////
   c = getChildren oo
   class \ c
   xmlIsText \ c
   xmlIsElement \ c
Node
 Key
  (getAttributes,LibxmlNode)
  getAttributes
 Headline
  get the list of attributes of an XML node
 Description
  Example
   xmlParse ////<bar id="foo" name="too">asdf</bar>////
   a = getAttributes oo
   class \ a
Node
 Key
  (xmlAddAttribute,LibxmlNode,String,String)
  xmlAddAttribute
 Headline
  add an attribute to an XML node
 Usage
  p = xmlAddAttribute(n,a,x)
 Inputs
  n:
  a:
  x:
 Outputs
  p:LibxmlAttribute
   a new attribute with name given by {\tt a} and content given by {\tt x}
 Consequences
  Item
   {\tt p} is added to the list of attributes of {\tt n}
 Description
  Example
   n = xmlParse ////<bar>asdf</bar>////
   a = xmlAddAttribute(n,"foo","the value")
   xmlGetName a
   n
Node
 Key
  (xmlAddText,LibxmlNode,String)
  xmlAddText
 Headline
  add text to an XML node
 Usage
  m = xmlAddText(n,s)
 Inputs
  n:
  s:
 Outputs
  m:LibxmlNode
   a new text node containing the string {\tt s}.
 Consequences
  Item
   The new node {\tt m} is attached as the last child of {\tt n}.
 Caveat
  The list of children should not contain two adjacent text nodes.
 Description
  Example
   n = xmlParse ////<bar id="foo" name="too"></bar>////
   xmlAddText(n,"hi there")
   xmlIsText oo
   n
Node
 Key
  xmlGetName
  (xmlGetName,LibxmlAttribute)
  (xmlGetName,LibxmlNode)
 Usage
  xmlGetName n
 Headline
  get the name of an XML node or attribute
 Inputs
  n:
   @ ofClass{LibxmlNode,LibxmlAttribute} @
 Outputs
  :String
   the name of the node
 Description
  Example
   n = xmlParse ////<bar id="foo" name="too"></bar>////
   xmlGetName n
   xmlFirstAttribute n
   xmlGetName oo
Node
 Key
  xmlGetNext
  (xmlGetNext,LibxmlAttribute)
  (xmlGetNext,LibxmlNode)
 Usage
  xmlGetNext n
 Headline
  get the next XML node or attribute
 Inputs
  n:
 Outputs
  :
   the next node in the linked list, or @ TO null @, if there is none
 Description
  Example
   xmlParse ////<bar><a/><b/></bar>////
   xmlFirstChild oo
   xmlGetNext oo
   xmlGetNext oo
   xmlParse ////<bar id="foo" name="too"/>////
   xmlFirstAttribute oo
   xmlGetNext oo
   xmlGetNext oo
Node
 Key
  (xmlFirstAttribute,LibxmlNode)
  xmlFirstAttribute
 Usage
  xmlFirstAttribute n
 Headline
  get the first attribute of an XML node
 Inputs
  n:
 Outputs
  :LibxmlAttribute
   The first attribute in the list of attributes of {\tt n}.
 Description
  Example
   xmlParse ////<bar id="foo" name="too"/>////
   xmlFirstAttribute oo
 SeeAlso
  xmlGetNext   
Node
 Key
  (xmlGetContent,LibxmlNode)
  xmlGetContent
 Headline
  get the string content of an XML text node
 Usage
  xmlGetContent n
 Inputs
  n:
   a text node, as determined by @ TO xmlIsText @
 Outputs
  :String
   the content string contained in {\tt n}
 Description
  Example
   xmlParse ////<bar>hi there</bar>////
   xmlFirstChild oo
   xmlGetContent oo
   class oo
Node
 Key
  (xmlAddElement,LibxmlNode,String)
  xmlAddElement
 Usage
  m = xmlAddElement(n,s)
 Headline
  add an element to an XML node
 Inputs
  n:
  s:
 Outputs
  m:LibxmlNode
   a new node named {\tt s}
 Consequences
  Item
   The new node {\tt m} is attached as the last child of {\tt n}.
 Description
  Example
   n = xmlParse ////<bar></bar>////
   xmlAddElement(n,"a")
   xmlAddElement(n,"b")
   xmlAddElement(oo,"b1")
   n
Node
 Key
  (xmlNewRoot,String)
  xmlNewRoot
 Headline
  create the root node of an XML document
 Usage
  xmlNewRoot v
 Inputs
  v:
 Outputs
  :LibxmlNode
   the new root node of a new XML document, with name given by {\tt v}
 Description
  Example
   xmlNewRoot "foo"
   xmlDocDump oo
 SeeAlso
  xmlDocDump
Node
 Key
  xmlFirstChild
  (xmlFirstChild,LibxmlAttribute)
  (xmlFirstChild,LibxmlNode)
 Headline
  get the first child of an XML node or attribute
 Usage
  xmlFirstChild n
 Inputs
  n:
   @ ofClass{LibxmlNode,LibxmlAttribute} @
 Outputs
  :LibxmlNode
 Description
  Example
   n = xmlParse ////<bar a="123"><foo/></bar>////  
   xmlFirstChild n
   xmlFirstAttribute n
   xmlFirstChild oo
 SeeAlso
  xmlGetNext
Node
 Key
  LibxmlAttribute
 Headline
  the class of all XML attributes created by the library libxml
Node
 Key
  symbol xmlTypeTable
 Usage
  xmlTypeTable
 Headline
  a hash table of descriptions for XML node type codes
 SeeAlso
  symbol xmlTypeIndex
 Description
  Example
   xmlTypeTable
Node
 Key
  xmlDocDump
 Headline
  dump a document
 Description
  Example
   n = xmlNewRoot "foo"
   xmlAddElement(n,"bar")
   xmlAddText(oo,"hi there")
   xmlAddElement(n,"key")
   xmlAddText(oo,"frobble 你好")
   n
   xmlDocDump n   
 SeeAlso
  xmlNewRoot
Node
 Key
  xmlTypes
  (xmlTypes,Sequence)
 Headline
  a hash table of descriptions for XML node type codes
Node
 Key
  (xmlType,LibxmlNode)
  xmlType
 Headline
  the type code for an XML node
Node
 Key
  (xmlTypeDescription,LibxmlNode)
  xmlTypeDescription
 Headline
  the type description for an XML node
 Usage
  xmlTypeDescription n
 Inputs
  n:
 Outputs
  :String
   the description of the type of {\tt n}
 Description
  Example
   xmlParse "<foo>the cat<bar/><!-- a comment--></foo>"
   getChildren oo
   xmlTypeDescription \ oo
Node
 Key
  symbol xmlTypeIndex
 Headline
  get XML type code from its description
 Usage
  xmlTypeIndex
 Description
  Example
   xmlTypeIndex
 SeeAlso
  symbol xmlTypeTable
///

undocumented {
  (net,LibxmlAttribute),
  (net,LibxmlNode),
  (net,XMLnode)
  }

-*
generateAssertions ///
x = parse ////<foo a="hi there">ho there<bar/></foo>////
#x
x.tag
x.children#0
x#"a"
y = x.children#1;
#y
y.tag
///
*-

TEST ///
     x = parse ////<foo a="hi there">ho there<bar/></foo>////
     assert( (#x) === 3 )
     assert( (x.tag) === "foo" )
     assert( (x.children#0) === "ho there" )
     assert( (x#"a") === "hi there" )
     y = x.children#1;
     assert( (#y) === 1 )
     assert( (y.tag) === "bar" )
///

-- Local Variables:
-- fill-column: 122
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/packages RemakePackages=true RerunExamples=true IgnoreExampleErrors=false RemakeAllDocumentation=true errorDepth=1 all-XML check-XML "
-- End:
