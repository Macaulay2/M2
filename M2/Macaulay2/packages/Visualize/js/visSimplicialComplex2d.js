  // Initialize variables.
  var width  = null,
      height = null,
      colors = null,
      faceColors = null;

  var svg = null;
  var nodes = null,
    lastNodeId = null,
    links = null,
    faces = null;

  var constrString = null;
  var incMatrix = null;
  var adjMatrix = null;
  var incMatrixString = null;
  var adjMatrixString = null;

  var force = null;

  var drag_line = null;

  // Handles to link and node element groups.
  var path = null,
      circle = null,
      polygon = null;

  // Mouse event variables.
  var selected_node = null,
      selected_link = null,
      selected_face = null,
      selected_face_node_1 = null,
      selected_face_node_2 = null,
      mousedown_link = null,
      mousedown_node = null,
      mousedown_face = null,
      mouseup_node = null;

  var fHeld = false;

  var drag = null;

 // Helps determine what menu button was clicked.
  var clickTest = null; 

  var scriptSource = (function(scripts) {
    var scripts = document.getElementsByTagName('script'),
        script = scripts[scripts.length - 1];

    if (script.getAttribute.length !== undefined) {
        return script.src
    }

    return script.getAttribute('src', -1)
    }());
    
    // Just get the current directory that contains the html file.
    scriptSource = scriptSource.substring(0, scriptSource.length - 28);
      
    console.log(scriptSource);

function initializeBuilder() {
    
  // Set up SVG for D3.
  width  = window.innerWidth-document.getElementById("side").clientWidth;
  height = window.innerHeight-10;
  colors = d3.scale.category10();
  faceColors = d3.scale.category20();

  svg = d3.select('body')
    .append('svg')
    .attr('width', width)
    .attr('height', height)
    .attr('id', 'canvasElement2d');

  // Set up initial nodes and links
  //  - nodes are known by 'id', not by index in array.
  //  - links are always source < target; edge directions are set by 'left' and 'right'.
  
  lastNodeId = labelData.length;
    
  nodes = [];
  links = [];
  faces = [];
    
  // Create the nodes, links, and faces with their appropriate names and id's.
  for (var i = 0; i < labelData.length; i++) {
      nodes.push( {name: labelData[i], id: i, highlighted:false, x: 0, y: 0 } );
  }

  for (var i = 0; i < edgeData.length; i++) {
      links.push( { source: nodes[edgeData[i][0]], target: nodes[edgeData[i][1]], left: false, right: false, highlighted:false} );
  }

  for (var i = 0; i < faceData.length; i++) {
      faces.push( { v1: nodes[faceData[i][0]], v2: nodes[faceData[i][1]], v3: nodes[faceData[i][2]], highlighted:false} );
  }

  // Initialize D3 force layout.
  force = d3.layout.force()
      .nodes(nodes)
      .links(links)
      .size([width, height])
      .linkDistance(forceLinkDist)
      .charge(forceCharge)
      .on('tick', tick);
    
  // After the force variable is initialized, set the sliders to update the force variables.
  chargeSlider.noUiSlider.on('slide', updateForceCharge);
  linkDistSlider.noUiSlider.on('slide', updateForceLinkDist);

  // When a node begins to be dragged by the user, call the function dragstart.
  drag = force.drag()
    .on("dragstart", dragstart);

  // Line displayed when dragging new nodes.
  drag_line = svg.append('svg:path')
    .attr('class', 'link dragline hidden')
    .attr('d', 'M0,0L0,0');

  // Handles to link and node element groups.
  polygon = svg.append('svg:g').selectAll('face');
  path = svg.append('svg:g').selectAll('path');
  circle = svg.append('svg:g').selectAll('g');

  // Mouse event variables.
  selected_node = null;
  selected_link = null;
  mousedown_link = null;
  mousedown_node = null;
  mouseup_node = null;
    
  // Define which functions should be called for various mouse events on the svg.
  svg.on('mousedown', mousedown)
    .on('mousemove', mousemove)
    .on('mouseup', mouseup);

  // Define which functions should be called when a key is pressed and released.
  d3.select(window)
    .on('keydown', keydown)
    .on('keyup', keyup);
    
  // The restart() function updates the graph.
  restart();
  
  // Brett: Need to fix this.
  /*
  var maxLength = d3.max(nodes, function(d) { return d.name.length; });

  console.log("maxLength: " + maxLength + "\n");

  if(maxLength < 4){
        document.getElementById("nodeText").style.fill = 'white';
  } else {
        document.getElementById("nodeText").style.fill = 'black';
  }
  */

}

function resetComplex() {
  // Set the 'fixed' attribute to false for all nodes and then restart the force layout.
  forceOn = false;
  toggleForce();
  restart();
}

function dragstart(d) {
  // When dragging a node, set it to be fixed so that the user can give it a static position.
  d3.select(this).classed(d.fixed = true);
}

function resetMouseVars() {
  // Reset all mouse variables.
  mousedown_node = null;
  mouseup_node = null;
  mousedown_link = null;
  mousedown_face = null;
}

// Update force layout (called automatically by the force layout simulation each iteration).
function tick() {
  // Update the 2-dimensional faces.
  polygon.attr("points",function(d) {
    return (d.v1.x > width-15 ? width-15 : ((d.v1.x < 15 ? 15 : d.v1.x))) +
      "," + (d.v1.y > height-15 ? height-15 : ((d.v1.y < 15 ? 15 : d.v1.y))) +
      " " + (d.v2.x > width-15 ? width-15 : ((d.v2.x < 15 ? 15 : d.v2.x))) +
      "," + (d.v2.y > height-15 ? height-15 : ((d.v2.y < 15 ? 15 : d.v2.y))) +
      " " + (d.v3.x > width-15 ? width-15 : ((d.v3.x < 15 ? 15 : d.v3.x))) +
      "," + (d.v3.y > height-15 ? height-15 : ((d.v3.y < 15 ? 15 : d.v3.y)));
  });

  // Draw directed edges with proper padding from node centers.
  path.attr('d', function(d) {
      
    // For each edge, set the attribute 'd' to have the form "MsourcexCoord,sourceyCoord LtargetxCoord,targetyCoord".
    // Then the appropriate coordinates to use for padding the directed edges away from the nodes can be obtained by
    // the 'd' attribute.
    return 'M' + (d.source.x > width-15 ?
                  width-15 :((d.source.x < 15 ? 15 :d.source.x))) + ',' +
      (d.source.y > height-15 ? height-15 : ((d.source.y < 15 ? 15 :
                                              d.source.y))) + 'L' +
      (d.target.x > width-15 ? width-15 : ((d.target.x < 15 ? 15 : d.target.x)))
      + ',' + (d.target.y > height-15 ? height-15 :
               ((d.target.y < 15 ? 15 : d.target.y)));
  });

  // Restrict the nodes to be contained within a 15 pixel margin around the svg.
  circle.attr('transform', function(d) {
    if (d.x > width - 15) {
      d.x = width - 15;
    }
    else if (d.x < 15) {
      d.x = 15;
    }
    if (d.y > height - 15) {
      d.y = height - 15;
    }
    else if (d.y < 15) {
      d.y = 15;
    }
    
    // Visually update the locations of the nodes based on the force simulation.
    return 'translate(' + d.x + ',' + d.y + ')';
  });
    
}

// Update graph (called when needed).
function restart() {
    
  polygon = polygon.data(faces);

  polygon.classed('highlighted', function(d) { return d.highlighted;})
  .style("fill", function(d,i) { return d.highlighted ? '#FF0000' : faceColors(i); })
  .style('opacity', function(d) { return d.highlighted ? .6 : .4;})
  .style('stroke', function(d){ return d === selected_face ? '#999' : 'none';})
  .style('stroke-width', function(d){return d === selected_face ? 6 : 0;})
  .style('stroke-dasharray', function(d){return d === selected_face ? '5,5' : '0';});
    
   var faceGroup = polygon.enter().append('svg:polygon');

   faceGroup.attr('class', 'face')
    .attr("points", function(d) { return d.v1.x + "," + d.v1.y + " " + d.v2.x + "," + d.v2.y + " " + d.v3.x + "," + d.v3.y; })
    .style("fill", function(d,i) { return d.highlighted ? '#FF0000' : faceColors(i); })
    .style('opacity', function(d) { return d.highlighted ? .6 : .4;})
    .style('stroke-width', function(d) {return d === selected_face ? 4 : 0;})
    .classed('highlighted',function(d) {return d.highlighted;})
    .on('mousedown', function(d) {
      // If the user clicks on a face while either the shift key is pressed or curEdit is false, do nothing.
      if(d3.event.shiftKey || !curEdit) return;

      // If the user clicks on a path while the shift key is not pressed and curEdit is true, set mousedown_face
      // to be the path that the user clicked on.
      mousedown_face = d;
      
      // If the face was already selected, then unselect it.
      if(mousedown_face === selected_face) selected_face = null;
      
      // If the face was not already selected, then select it.
      else selected_face = mousedown_face;
      
      // Since we selected or unselected a link, set all nodes and links to be unselected.
      selected_node = null;
      selected_link = null;
      // If highlighting neighbors is turned on, un-highlight all nodes and links since there is no currently selected node.
      if(curHighlight) unHighlightAll();
      
      // Update all properties of the graph.
      restart();
    });
    
    // remove old faces
    polygon.exit().remove();
    
    
  // Construct the group of edges from the 'links' array.
  path = path.data(links);

  // Update existing links.
  // If a link is currently selected, set 'selected: true'.  If a link should be highlighted, set 'highlighted: true'.
  path.classed('highlighted', function(d) {return d.highlighted; })
    .classed('selected', function(d) { return d === selected_link; })
    // If the edge is directed towards the source or target, attach an arrow.
    .style('marker-start', function(d) { return d.left ? 'url(#start-arrow)' : ''; })
    .style('marker-end', function(d) { return d.right ? 'url(#end-arrow)' : ''; });

  // Add new links.
  path.enter().append('svg:path')
    .attr('class', 'link')
    // If a link should be highlighted, set 'highlighted: true'.
    .classed('highlighted', function(d) {return d.highlighted; })
    // If a link is currently selected, set 'selected: true'.
    .classed('selected', function(d) { return d === selected_link; })
    // If the edge is directed towards the source or target, attach an arrow.
    .style('marker-start', function(d) { return d.left ? 'url(#start-arrow)' : ''; })
    .style('marker-end', function(d) { return d.right ? 'url(#end-arrow)' : ''; })
    .on('mousedown', function(d) {
      // If the user clicks on a path while either the shift key is pressed or curEdit is false, do nothing.
      if(d3.event.shiftKey || !curEdit || fHeld) return;

      // If the user clicks on a path while the shift key is not pressed and curEdit is true, set mousedown_link
      // to be the path that the user clicked on.
      mousedown_link = d;
      
      // If the link was already selected, then unselect it.
      if(mousedown_link === selected_link) selected_link = null;
      
      // If the link was not already selected, then select it.
      else selected_link = mousedown_link;
      
      // Since we selected or unselected a link, set all nodes and faces to be unselected.
      selected_node = null;
      selected_face = null;
      
      // If highlighting neighbors is turned on, un-highlight all nodes and links since there is no currently selected node.
      if(curHighlight) unHighlightAll();
      
      // Update all properties of the graph.
      restart();
    });

  // Remove old links.
  path.exit().remove();

  // Create the circle (node) group.
  // Note: the function argument is crucial here!  Nodes are known by id, not by index!
  circle = circle.data(nodes, function(d) { return d.id; });

  // Update existing nodes (highlighted & selected visual states).
  circle.selectAll('circle')
    // If a node is currently selected, then make it brighter.
    .style('fill', function(d) { return (d === selected_node || d === selected_face_node_1 || d === selected_face_node_2) ? d3.rgb(colors(d.id)).brighter().toString() : (d.highlighted ? '#FF0000' : colors(d.id)); })
    .classed('highlighted', function(d) { return d.highlighted; });

  // Add new nodes.
  var g = circle.enter().append('svg:g');

  g.append('svg:circle')
    .attr('class', 'node')
    .attr('r', 12)
    .style('fill', function(d) { return (d === selected_node) ? d3.rgb(colors(d.id)).brighter().toString() : (d.highlighted ? '#FF0000' : colors(d.id)); })
    //.style('stroke', function(d) { return d3.rgb(colors(d.id)).darker().toString(); })
    .classed('highlighted',function(d) {return d.highlighted;})
    .on('mouseover', function(d) {
      // If no node has been previously clicked on or if the user has not dragged the cursor to a different node after clicking,
      // then do nothing.
      if (!mousedown_node || d === mousedown_node) return;
      // Otherwise enlarge the target node.
      d3.select(this).attr('transform', 'scale(1.1)');
    })
    .on('mouseout', function(d) {
      // If no node has been previously clicked on or if the user has not dragged the cursor to a different node after clicking,
      // then do nothing.
      if (!mousedown_node || d === mousedown_node) return;
      // Otherwise unenlarge the target node.  (The user has chosen to not create an edge to this node and has moved the cursor elsewhere.)
      d3.select(this).attr('transform', '');
    })
    .on('mousedown', function(d) {
      // If either the shift key is held down or editing is disabled, do nothing.
      // Brett: Add back in the following line if we don't want selected nodes brightened in non-editing mode.
      //if(d3.event.shiftKey || !curEdit) return;
      if(d3.event.shiftKey) return;
      
      if(fHeld) {
        if(!selected_face_node_1){
            selected_node = null;
            selected_link = null;
            selected_face_node_1 = d;
        }  else if(selected_face_node_1 && !selected_face_node_2){
            if(selected_face_node_1 == d){
                selected_face_node_1 = null;
            } else {
                selected_face_node_2 = d;
            }
        } else if(selected_face_node_1 && selected_face_node_2){
            if(selected_face_node_1 == d || selected_face_node_2 == d){
                selected_face_node_2 = null;
            } else {
                var face = faces.filter(function(f) {
                  return ((f.v1 === selected_face_node_1 ||
                           f.v2 === selected_face_node_1 ||
                           f.v3 === selected_face_node_1) &&
                          (f.v1 === selected_face_node_2 ||
                           f.v2 === selected_face_node_2 ||
                           f.v3 === selected_face_node_2) &&
                          (f.v1 === d || f.v2 === d || f.v3 === d));})[0];
                if(!face){
                  // If there was not already a face on the three chosen vertices, create one.
                  face = {v1: selected_face_node_1, v2: selected_face_node_2, v3: d, highlighted:false};
                  faces.push(face);
                  selected_face = face;
                  
                  // Check to be sure all three links involving the selected nodes either already belong to links or are added to links.
                  var minNodeId = d3.min([selected_face_node_1.id,selected_face_node_2.id,d.id]);
                  var minNode = nodes.filter(function(n){return n.id == minNodeId;})[0];
                  var maxNodeId = d3.max([selected_face_node_1.id,selected_face_node_2.id,d.id]);
                  var maxNode = nodes.filter(function(n){return n.id == maxNodeId;})[0];
                  var midNodeId = selected_face_node_1.id + selected_face_node_2.id + d.id - minNodeId - maxNodeId;
                  var midNode = nodes.filter(function(n){return n.id == midNodeId;})[0];
                  var link1 = links.filter(function(l) {return l.source.id === minNodeId && l.target.id === midNodeId;})[0];
                  if(!link1){
                      links.push( {source: minNode, target: midNode, highlighted: false} );
                  }
                  var link2 = links.filter(function(l) {return l.source.id === minNodeId && l.target.id === maxNodeId;})[0];
                  if(!link2){
                      links.push( {source: minNode, target: maxNode, highlighted: false} );
                  }
                  var link3 = links.filter(function(l) {return l.source.id === midNodeId && l.target.id === maxNodeId;})[0];
                  if(!link3){
                      links.push( {source: midNode, target: maxNode, highlighted: false} );
                  }
                  // Reset menu options to defaults.
                  menuDefaults();
                }
                selected_face_node_1 = null;
                selected_face_node_2 = null;
            }
        }
        restart();
        return;
      }

      // Otherwise, select node.
      mousedown_node = d;
      
      // If the node that the user clicked was already selected, then unselect it.
      if(mousedown_node === selected_node) { selected_node = null; 
            if(curHighlight) unHighlightAll(); }
      //Brett: Add the following line back in if we don't want nodes to be brightened in non-editing mode.
      //else if(curEdit) { selected_node = mousedown_node;
      else {selected_node = mousedown_node;
            if(curHighlight) highlightAllNeighbors(selected_node);
      };
      selected_link = null;
      selected_face = null;

      // reposition drag line
      if(curEdit){
        drag_line
          .style('marker-end', 'url(#end-arrow)')
          .classed('hidden', false)
          .attr('d', 'M' + mousedown_node.x + ',' + mousedown_node.y + 'L' + mousedown_node.x + ',' + mousedown_node.y);
      }
          
      restart();
    })
    .on('mouseup', function(d) {
      if(!mousedown_node) return;

      // needed by FF
      drag_line
        .classed('hidden', true)
        .style('marker-end', '');

      // check for drag-to-self
      mouseup_node = d;
      if(mouseup_node === mousedown_node) { resetMouseVars(); return; }

      // unenlarge target node
      d3.select(this).attr('transform', '');

      // add link to graph (update if exists)
      // NB: links are strictly source < target; arrows separately specified by booleans
      var source, target, direction;
      if(mousedown_node.id < mouseup_node.id) {
        source = mousedown_node;
        target = mouseup_node;
        direction = 'right';
      } else {
        source = mouseup_node;
        target = mousedown_node;
        direction = 'left';
      }
      
      var link;
      link = links.filter(function(l) {
        return (l.source === source && l.target === target);
      })[0];

      // Graph Changed :: adding new links
      if(link) {
        link[direction] = true;
      } else {
        link = {source: source, target: target, left: false, right: false};
        link[direction] = true;
        links.push(link);
        // Graph is updated here so we change some items to default.
        menuDefaults();
      }

      // select new link
      if (curEdit) selected_link = link;
      selected_node = null;
      selected_face = null;
      if (curHighlight) unHighlightAll();
      restart();
    })

  .on('dblclick', function(d) {
      name = "";
      var letters = /^[0-9a-zA-Z_]+$/;
      while (name=="") {
        name = prompt('Enter new label name.', d.name);
        // Check whether the user has entered any illegal characters (including spaces).
        if (!(letters.test(name))) {
            alert('Please input alphanumeric characters only with no spaces.');
            name = "";
        }
        if (name==d.name) {
          return;
        }
        else if (checkName(name)) {
          alert('Sorry, a node with that name already exists.')
          name = "";
        }
      }
      
      if(name != "null") {
        d.name = name;
        d3.select(this.parentNode).select("text").text(function(d) {return d.name});
        changedNodes = true;
      }

      //document.getElementById("constructorString").innerHTML = "Macaulay2 Constructor: " + digraph2M2Constructor(nodes,links);

    });

  if(labelsOn){
  // show node IDs
    g.append('svg:text')
        .attr('x', 0)
        .attr('y', 4) 
        .attr('class', 'id noselect')
        .attr("pointer-events", "none")
        .text(function(d) { return d.name; });
  }
  /*
  var maxLength = d3.max(nodes, function(d) {
        return d.name.length;
  });
      
  if(maxLength < 4){
        document.getElementById("nodeText").style.fill = 'white';
  } else {
        document.getElementById("nodeText").style.fill = 'black';
  }
  */

  // remove old nodes
  circle.exit().remove();
    
    // set the graph in motion
    force.start();
}

function checkName(name) {
  for (var i = 0; i<nodes.length; i++) {
    if (nodes[i].name == name) {
      return true;
    }
  }
  return false;
}

function getNextAlpha(alpha) {
  return String.fromCharCode(alpha.charCodeAt(0) + 1);
}

function mousedown() {
  // prevent I-bar on drag
  //d3.event.preventDefault();

  // because :active only works in WebKit?
  svg.classed('active', true);

  if(!curEdit || d3.event.shiftKey || mousedown_node || mousedown_link || mousedown_face || fHeld) return;

  // insert new node at point

  var point = d3.mouse(this);
  var curName = intToAlphabet(lastNodeId);
  while(checkName(curName.toString())){
      curName += 'a';
  }
  curName = curName.toString();
  /*
  if (checkName(curName)) {
    curName += 'a';
  }
  while (checkName(curName)) {
    curName = curName.substring(0, curName.length - 1) + getNextAlpha(curName.slice(-1));
  }
  */

  // Graph Changed :: adding nodes
  node = {id: lastNodeId++, name: curName, highlighted: false};
  node.x = point[0];
  node.y = point[1];
  if (!forceOn) {
    node.fixed = true;
  }
  nodes.push(node);
  changedNodes = true;
  // Graph is updated here so we change some items to default 
  // d3.select("#isCM").html("isCM");
  menuDefaults();

  //document.getElementById("constructorString").innerHTML = "Macaulay2 Constructor: " + digraph2M2Constructor(nodes,links);
    
  // (Brett) Removing incidence and adjacency matrices for now.
  /*document.getElementById("incString").innerHTML = "Incidence Matrix: " + arraytoM2Matrix(getIncidenceMatrix(nodes,links));
  document.getElementById("adjString").innerHTML = "Adjacency Matrix: " + arraytoM2Matrix(getAdjacencyMatrix(nodes,links));*/

  restart();
}

function mousemove() {
  if(!mousedown_node) return;

  // update drag line
  drag_line.attr('d', 'M' + mousedown_node.x + ',' + mousedown_node.y + 'L' + d3.mouse(this)[0] + ',' + d3.mouse(this)[1]);

  restart();
}

function mouseup() {
  if(mousedown_node) {
    // hide drag line
    drag_line
      .classed('hidden', true)
      .style('marker-end', '');
  }

  // because :active only works in WebKit?
  svg.classed('active', false);

  // clear mouse event vars
  resetMouseVars();

  restart();

}

function spliceLinksForNode(node) {
  var toSplice = links.filter(function(l) {
    return (l.source === node || l.target === node);
  });
  toSplice.map(function(l) {
    links.splice(links.indexOf(l), 1);
  });
}

function spliceFacesForNode(node) {
  var toSplice = faces.filter(function(f) {
    return (f.v1 === node || f.v2 === node || f.v3 === node);
  });
  toSplice.map(function(f) {
    faces.splice(faces.indexOf(f), 1);
  });
}

function spliceFacesForLink(link) {
  var toSplice = faces.filter(function(f) {
    return ((f.v1 === link.source || f.v2 === link.source || f.v3 === link.source) && (f.v1 === link.target || f.v2 === link.target || f.v3 === link.target));
  });
  toSplice.map(function(f) {
    faces.splice(faces.indexOf(f), 1);
  });
}

// only respond once per keydown
var lastKeyDown = -1;

function keydown() {
  //d3.event.preventDefault();

  if(lastKeyDown !== -1) return;
  lastKeyDown = d3.event.keyCode;

  // shift
  if(d3.event.keyCode === 16) {
    circle.call(drag);
    svg.classed('shift', true);
  }
    
  // While the user holds 'f', allow the user to start creating a face.
  if(d3.event.keyCode === 70) {
    fHeld = true;
  }

  if(!selected_node && !selected_link && !selected_face) return;
  switch(d3.event.keyCode) {
    case 8: // backspace
    case 46: // delete
      if(curEdit && selected_node) {
        nodes.splice(nodes.indexOf(selected_node), 1);
        spliceLinksForNode(selected_node);
        spliceFacesForNode(selected_node);
        changedNodes = true;
        if(curHighlight) unHighlightAll();
      } else if(curEdit && selected_link) {
        links.splice(links.indexOf(selected_link), 1);
        spliceFacesForLink(selected_link);
        if(curHighlight) unHighlightAll();
      } else if(curEdit && selected_face) {
        faces.splice(faces.indexOf(selected_face), 1);
        if(curHighlight) unHighlightAll();
      }
      selected_link = null;
      selected_face = null;
      if(curEdit) {selected_node = null;}

      // Graph Changed :: deleted nodes and links
      // as a result we change some items to default
      // d3.select("#isCM").html("isCM");      
      menuDefaults();

      //document.getElementById("constructorString").innerHTML = "Macaulay2 Constructor: " + digraph2M2Constructor(nodes,links);
      // (Brett) Removing incidence and adjacency matrices for now.
      /*document.getElementById("incString").innerHTML = "Incidence Matrix: " + arraytoM2Matrix(getIncidenceMatrix(nodes,links));
      document.getElementById("adjString").innerHTML = "Adjacency Matrix: " + arraytoM2Matrix(getAdjacencyMatrix(nodes,links));*/

      restart();
      break;    
  }
  restart();
}

function keyup() {
  lastKeyDown = -1;

  // shift
  if(d3.event.keyCode === 16) {
    circle
      .on('mousedown.drag', null)
      .on('touchstart.drag', null);
    svg.classed('shift', false);
  }
    
  // If the user releases 'f', exit face addition mode.
  if(d3.event.keyCode === 70) {
    selected_face_node_1 = null;
    selected_face_node_2 = null;
    fHeld = false;
  }
    
}

function disableEditing() {
  circle.call(drag);
  svg.classed('shift', true);
  //selected_node = null;
  selected_link = null;
  selected_face = null;
  //if(curHighlight) unHighlightAll();
  restart();
}

function enableEditing() {
  circle
      .on('mousedown.drag', null)
      .on('touchstart.drag', null);
  svg.classed('shift', false);
}

function enableHighlight() {
  // If there is no currently selected node, then just return (negating the value of curHighlight).
  if(selected_node == null) return;
  highlightAllNeighbors(selected_node);
  console.log("curHighlight: "+curHighlight);
}

function unHighlightAll() {
    // Un-highlight all nodes.
    for (var i = 0; i<nodes.length; i++) {
       nodes[i].highlighted = false;
    }
    
    // Un-highlight all links.
    for (var i = 0; i<links.length; i++) {
       links[i].highlighted = false;
    }
    
    // Un-highlight all faces.
    for (var i = 0; i<faces.length; i++) {
       faces[i].highlighted = false;
    }
    
    // Update graph based on changes to nodes and links.
    restart();
}

function highlightAllNeighbors(n) {
    // Highlight all nodes that are neighbors with the given node n.
    for (var i = 0; i<nodes.length; i++) {
       nodes[i].highlighted = areNeighbors(nodes[i],n);
    }
    
    // Highlight all links that have the given node n as a source or target.
    for (var i = 0; i<links.length; i++) {
       links[i].highlighted = ((links[i].source === n) || (links[i].target === n));
    }
    
    // Highlight all faces that have the given node n as a vertex.
    for (var i = 0; i<faces.length; i++) {
       faces[i].highlighted = ((faces[i].v1 === n) || (faces[i].v2 === n) || (faces[i].v3 === n));
    }
    
    // Update graph based on changes to nodes and links.
    restart();
}

function areNeighbors(node1,node2) {
    return links.some( function(l) {return (((l.source === node1) && (l.target === node2)) || ((l.target === node1) && (l.source === node2)));});
}

function setAllNodesFixed() {
  for (var i = 0; i<nodes.length; i++) {
    nodes[i].fixed = true;
  }
}

function setAllNodesUnfixed() {
  for (var i = 0; i<nodes.length; i++) {
    nodes[i].fixed = false;
  }
}

function hideLabels() {
    circle.select("text").remove();    
}

function showLabels() {
     circle.append('svg:text')
      .attr('x', 0)
      .attr('y', 4)
      .attr('class', 'id noselect')
      .attr("pointer-events", "none")
      .text(function(d) { return d.name; });
}

function updateWindowSize2d() {
    console.log("resizing window");
    //var svg = document.getElementById("canvasElement2d");
    
    // get width/height with container selector (body also works)
    // or use other method of calculating desired values
    if(!menuOpen){
        width = window.innerWidth-10;
    } else {
        width = window.innerWidth - document.getElementById("side").clientWidth;
    }
    height = window.innerHeight-10;

    // set attrs and 'resume' force 
    svg.attr('width', width);
    svg.attr('height', height);

    force.size([width, height]).resume();
}

// Function to construct the M2 constructor for the simplicial
// complex.  Note: If the user has modified the nodes in the
// simplicial complex, this will need to pass the new base ring back
// to Macaulay2 which will cause issues if we want to add in boolean
// tests/numerical invariants at any point.

function simplicialComplex2M2Constructor( nodeSet, edgeSet , faceSet ){
    if(nodeSet.length==0){
        return "visRing = ZZ[a]; simplicialComplex{1_visRing}";
    }
    if(changedNodes){
        var ringStr = "visRing = ZZ[";
        for(var i=0; i < nodeSet.length-1; i++){
            ringStr = ringStr+nodeSet[i].name.toString()+",";
        }
        ringStr = ringStr+nodeSet[nodeSet.length-1].name.toString()+"]; ";
    }
    var facetStr = "simplicialComplex{";
    var facets = simplicialComplexFacets(nodeSet,edgeSet,faceSet);
    for(var i=0; i < facets.length-1; i++){
        for(var j=0; j < facets[i].length-1; j++){
            facetStr = facetStr + nodeSet[facets[i][j]].name.toString() + "*";
        }
        facetStr = facetStr + nodeSet[facets[i][facets[i].length-1]].name.toString() + ",";
    }
    
    for(var j=0; j < facets[facets.length-1].length-1; j++){
        facetStr = facetStr + nodeSet[facets[facets.length-1][j]].name.toString() + "*";
    }
    
    facetStr = facetStr + nodeSet[facets[facets.length-1][facets[facets.length-1].length-1]].name.toString() + "}";
    
    if(changedNodes){
        return ringStr+facetStr;
    } else {
        return facetStr;
    }
}

// determines if a graph contains singletons, if it does it returns an array containing their id, if not returns empty array
function singletons(nodeSet, edgeSet){

  var singSet = [];
  var n = nodeSet.length;
        var e = edgeSet.length;
  var curNodeName = -1;
  var occur = 0;
  for( var i = 0; i < n; i++){
    curNodeName = (nodeSet[i]).name;
    for( var j = 0; j < e; j++ ){
      if ( (edgeSet[j].source.name == curNodeName) || (edgeSet[j].target.name == curNodeName) ){
        occur=1;
        break;
      }
    }//end for
    if (occur == 0){
      singSet.push(curNodeName); // add node id to singleton set
    }
    occur = 0; //reset occurrences for next node id
  }
  return singSet;
}

function positionByID(nodeSet,n){
    for(var i=0; i < nodeSet.length; i++){
        if(nodeSet[i].id == n){return i};
    }
}

function simplicialComplexFacets(nodeSet,edgeSet,faceSet){
    var results = [];
    // If a vertex is not involved in any 1-dimensional face, then it is a facet.
    for(var i=0; i < nodeSet.length; i++){
        if(edgeSet.every(function(d){return ((d.source != nodeSet[i]) && (d.target != nodeSet[i]));})){results.push([i]);}
    }
    
    // If a 1-dimensional face is not a subset of any 2-dimensional face, then it is a facet.
    for(var i=0; i < edgeSet.length; i++){
      if(faceSet.every(
        function(d){return (((d.v1 != edgeSet[i].source) &&
                             (d.v2 != edgeSet[i].source) &&
                             (d.v3 != edgeSet[i].source)) ||
                            ((d.v1 != edgeSet[i].target) &&
                             (d.v2 != edgeSet[i].target) &&
                             (d.v3 != edgeSet[i].target)));})){
        results.push([positionByID(nodeSet,edgeSet[i].source.id),positionByID(
          nodeSet,edgeSet[i].target.id)]);}
    }

    // All 2-dimensional faces are facets since the simplicial complex is 2-dimensional.
    faceSet.forEach(function(d){results.push([positionByID(nodeSet,d.v1.id),positionByID(nodeSet,d.v2.id),positionByID(nodeSet,d.v3.id)]);});
    
    return results;
    
}

function isPureComplex(nodeSet,edgeSet,faceSet){
    var facets = simplicialComplexFacets(nodeSet,edgeSet,faceSet);
    // If the simplicial complex is the irrelevant complex, return true.
    if(facets.length == 0){return true;}
    // If two facets have different sizes, return false.
    if(facets.some(function(d){return d.length != facets[0].length;})){return false;}
    return true;    
}

// Given an array and an element, this method returns true if the element is contained in the array, else it returns false.
function containedIn(n,arr){
    for(var i=0; i < arr.length; i++){
        if(arr[i] == n){return true;}
    }
    return false;
}


// Turns an integer into a string of lower-case letters.
function intToAlphabet(i) {
    return (i >= 26 ? intToAlphabet((i / 26 >> 0) - 1) : '') +
        'abcdefghijklmnopqrstuvwxyz'[i % 26 >> 0];
}

// Constructs the incidence matrix for a graph as a multidimensional array.
function getIncidenceMatrix (nodeSet, edgeSet){

  var incMatrix = [];

  // The next two loops create an initial (nodes.length) x (links.length) matrix of zeros.
  for(var i = 0;i < nodeSet.length; i++){
    incMatrix[i] = [];

    for(var j = 0; j < edgeSet.length; j++){
      incMatrix[i][j] = 0;
    }
  }

  for (var i = 0; i < edgeSet.length; i++) {
    incMatrix[(edgeSet[i].source.id)][i] = 1; // Set matrix entries corresponding to incidences to 1.
    incMatrix[(edgeSet[i].target.id)][i] = 1;
  }

  return incMatrix;
}

// Constructs the adjacency matrix for a graph as a multidimensional array.
function getAdjacencyMatrix (nodeSet, edgeSet){
  var adjMatrix = []; // The next two loops create an initial (nodes.length) x (nodes.length) matrix of zeros.
  for(var i = 0; i < nodeSet.length; i++){
    adjMatrix[i] = [];
    for(var j = 0; j < nodeSet.length; j++){
      adjMatrix[i][j] = 0;
    }
  }

  for (var i = 0; i < edgeSet.length; i++) {
    if(edgeSet[i].right) { adjMatrix[edgeSet[i].source.id][edgeSet[i].target.id] = 1;} // Set matrix entries corresponding to adjacencies to 1.
    if(edgeSet[i].left) { adjMatrix[edgeSet[i].target.id][edgeSet[i].source.id] = 1;}
  }

  return adjMatrix;
}

function updateForceCharge(){
    if(!forceOn){toggleForce()};
    forceCharge = -chargeSlider.noUiSlider.get();
    force.charge(forceCharge).start();
}

function updateForceLinkDist(){
    if(!forceOn){toggleForce()};
    forceLinkDist = linkDistSlider.noUiSlider.get();
    force.linkDistance(forceLinkDist).start();
}

// Takes a rectangular array of arrays and returns a string which can be copy/pasted into M2.
function arraytoM2Matrix (arr){
  var str = "matrix{{";
  for(var i = 0; i < arr.length; i++){
    for(var j = 0; j < arr[i].length; j++){
      str = str + arr[i][j].toString();
      if(j == arr[i].length - 1){
        str = str + "}";
            } else {
        str = str + ",";
      }
    }
    if(i < arr.length-1){
      str = str + ",{";
    } else {
      str = str + "}";
    }
  }

  return str;
}

// Takes a rectangular array of arrays and returns a list which can be copy/pasted into M2.
function arraytoM2List (arr){
  var str = "{{";
  for(var i = 0; i < arr.length; i++){
    for(var j = 0; j < arr[i].length; j++){
      str = str + arr[i][j].toString();
      if(j == arr[i].length - 1){
        str = str + "}";
            } else {
        str = str + ",";
      }
    }
    if(i < arr.length-1){
      str = str + ",{";
    } else {
      str = str + "}";
    }
  }

  return str;
}

// for making unique timestamps in LaTeX. Numbers are not allowed in macros.
function makeid()
{
    var randomtext = "";
    var randompossible = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz";

    for( var i=0; i < 5; i++ )
        randomtext += randompossible.charAt(Math.floor(Math.random() * randompossible.length));

    return randomtext;
}


function exportTikz (event){
  var points = [];
  for(var i = 0; i < nodes.length; i++){
    points[i] = [nodes[i].x.toString()+"/"+nodes[i].y.toString()+"/"+nodes[i].id+"/"+nodes[i].name];
  }

  var edges = [];
  for(var j = 0; j < links.length; j++){
    edges[j] = [ links[j].source.id.toString()+"/"+links[j].target.id.toString() ];
  }

  var tikzFaces = [];
  var tikzShade = 10;
  for(var j = 0; j < faces.length; j++){
    tikzFaces[j] = [ faces[j].v1.id.toString()+"/"+faces[j].v2.id.toString()+"/"+faces[j].v3.id.toString()+"/"+tikzShade.toString() ];
    tikzShade = tikzShade + 50/(faces.length-1);
  }

  var timestamp = makeid();

  // Branden: Actual tikz code; displayed in black and white. We could do colors, but I am afriad of shading issues when printing.
  var tikzTex = "";
  tikzTex =  "\\begin{tikzpicture}\n         \\newcommand*\\points"+timestamp+
    "{"+points+"}\n          \\newcommand*\\edges"+timestamp+"{"+edges+
    "}\n          \\newcommand*\\faces"+timestamp+"{"+tikzFaces+
    "}\n          \\newcommand*\\scale"+timestamp+"{0.02}\n          "+
    "\\foreach \\x/\\y/\\z/\\w in \\points"+timestamp+" {\n          "+
    "\\node (\\z) at (\\scale"+timestamp+"*\\x,-\\scale"+timestamp+
    "*\\y) [circle,draw,fill=white,inner sep=1pt] {$\\w$};\n          }\n"+
        "\\foreach \\x/\\y/\\z/\\w in \\faces"+timestamp+
    " {\n    \\fill[black!\\w]\n    (\\x.center) -- (\\y.center) -- "+
    "(\\z.center) -- cycle;\n         }\n       \\foreach \\x/\\y/\\z/\\w"+
    "in \\points"+timestamp+" {\n          \\node (\\z) at (\\scale"+
    timestamp+"*\\x,-\\scale"+timestamp+"*\\y) [circle,draw,"+
    "fill=white,inner sep=1pt] {$\\w$};\n          }\n             "+
    "\\foreach \\x/\\y in \\edges"+timestamp+" {\n          \\draw (\\x)"+
    "-- (\\y);\n          }\n                       \\end{tikzpicture}\n"+
    "% \\points"+timestamp+" is point set in the form x-coord/y-coord/node"+
    "ID/node label\n     % \\edges"+timestamp+" is edge set in the form "+
    "Source ID/Target ID\n      % \\scale"+timestamp+" makes the picture"+
    "able to be viewed on the page\n      % \\faces"+timestamp+
    " is a set in the form (node 1)/(node 2)/(node 3)/fill percent\n";
    
  if(!tikzGenerated){
    var tikzDiv = document.createElement("div");
    tikzDiv.id = "tikzHolder";
    tikzDiv.className = "list-group-item";
    tikzDiv.setAttribute('href','#');
    var tikzInput = document.createElement("textarea");
    tikzInput.value = "";
    tikzInput.id = "tikzTextBox";
    tikzInput.rows = 20;
    tikzInput.style = "vertical-align:middle; width: 100%;";
    var tikzButton = document.createElement("button");
    tikzButton.id = "copyButton";
    tikzButton.style = "vertical-align:middle;";
    //tikzButton.dataClipboardTarget = "#tikzTextBox";
    tikzButton.type = "button";
    var clipboardImg = document.createElement("img");
    clipboardImg.src = scriptSource+"images/32px-Octicons-clippy.png";
    clipboardImg.alt = "Copy to clipboard";
    clipboardImg.style = "width:19px;height:19px;";
    tikzButton.appendChild(clipboardImg);
    tikzDiv.appendChild(tikzInput);
    tikzDiv.appendChild(tikzButton);
    var listGroup = document.getElementById("menuList");
    listGroup.insertBefore(tikzDiv,listGroup.childNodes[14]);
    document.getElementById("copyButton").setAttribute("data-clipboard-target","#tikzTextBox");
    clipboard = new ClipboardJS('#copyButton');
    clipboard.on('error', function(e) {
        window.alert("Press enter, then CTRL-C or CMD-C to copy")
    });  
    tikzGenerated = true;
  }
  document.getElementById("tikzTextBox").value = tikzTex;    
}

// -----------------------------------------
// Begin Server Stuff
// -----------------------------------------


// Add a response for each id from the side menu
function onclickResults(m2Response) {
    
    if (clickTest == "hasEulerianTrail"){
      d3.select("#hasEulerianTrail").html("&nbsp;&nbsp; hasEulerianTrail :: <b>"+m2Response+"</b>");
    } 
    
}


// Anytime the graph is edited by user we call this function.
// It changes the menu items to default.
function menuDefaults() {
  d3.select("#isPure").html("&nbsp;&nbsp; isPure");
  if (tikzGenerated) {
      d3.select("#tikzHolder").node().remove();
      tikzGenerated = false;
  }
}


// Create the XHR object.
function createCORSRequest(method, url) {
  var xhr = new XMLHttpRequest();                    
  if ("withCredentials" in xhr) {
    // XHR for Chrome/Firefox/Opera/Safari.
    xhr.open(method, url, true);
  } else if (typeof XDomainRequest != "undefined") {
    // XDomainRequest for IE.
    xhr = new XDomainRequest();
    xhr.open(method, url);
  } else {
    // CORS not supported.
    xhr = null;
  }

  return xhr;
}
 
// Make the actual CORS request.
function makeCorsRequest(method,url,browserData) {
  // All HTML5 Rocks properties support CORS.
  // var url ='http://localhost:8000/fcn2/';
 
  var xhr = createCORSRequest(method, url);
  if (!xhr) {
    alert('CORS not supported');
    return;
  }
 
  // Response handlers.
  xhr.onload = function() {
    var responseText = xhr.responseText;

    onclickResults(responseText);      

  };
 
  //xhr.onerror = function() {
  //  alert('Woops, there was an error making the request.');
  //};

  xhr.send(browserData);
}

// -----------------------------------------
// End Server Stuff
// -----------------------------------------
