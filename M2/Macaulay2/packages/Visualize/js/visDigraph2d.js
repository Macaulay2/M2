  // Initialize variables.
  var width  = null,
      height = null,
      colors = null;

  var svg = null;
  var nodes = null,
    lastNodeId = null,
    links = null;

  var constrString = null;
  var incMatrix = null;
  var adjMatrix = null;
  var incMatrixString = null;
  var adjMatrixString = null;

  var force = null;

  var drag_line = null;

  // Handles to link and node element groups.
  var path = null,
      circle = null;

  // Mouse event variables.
  var selected_node = null,
      selected_link = null,
      mousedown_link = null,
      mousedown_node = null,
      mouseup_node = null;

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
    scriptSource = scriptSource.substring(0, scriptSource.length - 18);
      
    //console.log(scriptSource);

function initializeBuilder() {
    
  //console.log("building graph");
    
  // Set up SVG for D3.
  width  = window.innerWidth-document.getElementById("side").clientWidth;
  height = window.innerHeight-10;
  colors = d3.scale.category10();

  svg = d3.select('body')
    .append('svg')
    .attr('width', width)
    .attr('height', height)
    .attr('id', 'canvasElement2d');

  // Set up initial nodes and links
  //  - nodes are known by 'id', not by index in array.
  //  - reflexive edges are indicated on the node (as a bold black circle).
  //  - links are always source < target; edge directions are set by 'left' and 'right'.
  var data = dataData;
  var names = labelData;

  lastNodeId = data.length;
  nodes = [];
  links = [];
    
  // Create the nodes with their appropriate names and id's, and set reflexive to true if and only if there is a 1 in the appropriate
  // spot on the main diagonal.  This represents a loop in the digraph.
  for (var i = 0; i < data.length; i++) {
      nodes.push( {name: names[i], id: i, reflexive: data[i][i] == 1, highlighted:false } );
  }

  for (var i = 0; i < data.length - 1; i++) {
      for (var j = i+1; j < data.length; j++) {
          var leftEdge = (data[j][i] == 1);
          var rightEdge = (data[i][j] == 1);
          if (leftEdge || rightEdge) {
              links.push( { source: nodes[i], target: nodes[j], left: leftEdge, right: rightEdge, highlighted:false} );
          }
      }
  }
  /*
  for (var i = 1; i<data.length; i++) {
      for (var j = 0; j < i; j++) {
          if (data[i][j] != 0) {
 
            var foundLink = false;
            var l = links.length;
            for(var k = 0; k < l; k++) {              
              
              if((links[k].source.id == j) && (links[k].target.id == i)){
                links[k].left = true;
                console.log("Added left arrow to existing link.");
                foundLink = true;
              } 
              if(foundLink = false) {
                links.push( { source: nodes[j], target: nodes[i], left: true, right: false} );
                console.log("Added new link with left arrow only.");
              }
            }
          }
      }
  }
  */

  //console.log(nodes);
  //console.log(links);
    
  //constrString = digraph2M2Constructor(nodes,links);
    
  // (Brett) Removing incidence and adjacency matrices.
  /*incMatrix = getIncidenceMatrix(nodes,links);
  adjMatrix = getAdjacencyMatrix(nodes,links);
  incMatrixString = arraytoM2Matrix(incMatrix);
  adjMatrixString = arraytoM2Matrix(adjMatrix);*/

  // Add a paragraph containing the Macaulay2 graph constructor string below the svg.
  /* d3.select("body").append("p")
  	.text("Macaulay2 Constructor: " + constrString)
  	.attr("id","constructorString");
  */

  // (Brett) Removing incidence and adjacency matrices.
    
/*  d3.select("body").append("p")
  	.text("Incidence Matrix: " + incMatrixString)
  	.attr("id","incString");

  d3.select("body").append("p")
  	.text("Adjacency Matrix: " + adjMatrixString)
  	.attr("id","adjString");*/

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
    
  // define arrow markers for graph links
  svg.append('svg:defs').append('svg:marker')
    .attr('id', 'end-arrow')
    .attr('viewBox', '0 -5 10 10')
    .attr('refX', 6)
    .attr('markerWidth', 3)
    .attr('markerHeight', 3)
    .attr('orient', 'auto')
    .append('svg:path')
    .attr('d', 'M0,-5L10,0L0,5')
    .attr('fill', '#000');

  svg.append('svg:defs').append('svg:marker')
    .attr('id', 'start-arrow')
    .attr('viewBox', '0 -5 10 10')
    .attr('refX', 4)
    .attr('markerWidth', 3)
    .attr('markerHeight', 3)
    .attr('orient', 'auto')
    .append('svg:path')
    .attr('d', 'M10,-5L0,0L10,5')
    .attr('fill', '#000');

  // When a node begins to be dragged by the user, call the function dragstart.
  drag = force.drag()
    .on("dragstart", dragstart);

  // Line displayed when dragging new nodes.
  drag_line = svg.append('svg:path')
    .attr('class', 'link dragline hidden')
    .attr('d', 'M0,0L0,0');

  // Handles to link and node element groups.
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

function resetGraph() {
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
}

// Update force layout (called automatically by the force layout simulation each iteration).
function tick() {
  // Draw directed edges with proper padding from node centers.
  path.attr('d', function(d) {
    // For each edge, calculate the distance from the source to the target
    // then normalize the x- and y-distances between the source and target.
    var deltaX = d.target.x - d.source.x,
        deltaY = d.target.y - d.source.y,
        dist = Math.sqrt(deltaX * deltaX + deltaY * deltaY),
        normX = deltaX / dist,
        normY = deltaY / dist,
        // If the edge is directed towards the source, then create extra padding (17) away from the source node to show the arrow,
        // else set the sourcePadding to 12.
        sourcePadding = d.left ? 17 : 12,
        // If the edge is directed towards the target, then create extra padding (17) away from the target node to show the arrow,
        // else set the targetPadding to 12.
        targetPadding = d.right ? 17 : 12,
        // Create new x and y coordinates for the source and the target based on whether extra padding was needed
        // to account for directed edges.
        sourceX = d.source.x + (sourcePadding * normX),
        sourceY = d.source.y + (sourcePadding * normY),
        targetX = d.target.x - (targetPadding * normX),
        targetY = d.target.y - (targetPadding * normY);
    
    // Restrict the padded x and y coordinates of the source and target to be within a 15 pixel margin around the svg.
    if (sourceX > width - 15) {
      sourceX = width - 15;
    }
    else if (sourceX < 15) {
      sourceX = 15;
    }
    if (targetX > width - 15) {
      targetX = width -15;
    }
    else if (targetX < 15) {
      targetX = 15;
    }
    if (sourceY > height - 15) {
      sourceY = height - 15;
    }
    else if (sourceY < 15) {
      sourceY = 15;
    }
    if (targetY  > height - 15) {
      targetY = height - 15;
    }
    else if (targetY  < 15) {
      targetY = 15;
    }
    // For each edge, set the attribute 'd' to have the form "MsourcexCoord,sourceyCoord LtargetxCoord,targetyCoord".
    // Then the appropriate coordinates to use for padding the directed edges away from the nodes can be obtained by
    // the 'd' attribute.
    return 'M' + sourceX + ',' + sourceY + 'L' + targetX + ',' + targetY;
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
      if(d3.event.shiftKey || !curEdit) return;

      // If the user clicks on a path while the shift key is not pressed and curEdit is true, set mousedown_link
      // to be the path that the user clicked on.
      mousedown_link = d;
      
      // If the link was already selected, then unselect it.
      if(mousedown_link === selected_link) selected_link = null;
      
      // (Brett) Isn't 'if (curEdit)' redundant since we already checked it above?  Remove this line?
//      else if (curEdit) selected_link = mousedown_link;
      
      // If the link was not already selected, then select it.
      else selected_link = mousedown_link;
      
      // Since we selected or unselected a link, set all nodes to be unselected.
      selected_node = null;
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

  // Update existing nodes (reflexive & selected visual states).
  circle.selectAll('circle')
    // If a node is currently selected, then make it brighter.
    .style('fill', function(d) { return (d === selected_node) ? d3.rgb(colors(d.id)).brighter().toString() : (d.highlighted ? '#FF0000' : colors(d.id)); })
    // Set the 'reflexive' attribute to true for all reflexive nodes.
    .classed('reflexive', function(d) { return d.reflexive; });

  // Add new nodes.
  var g = circle.enter().append('svg:g');

  g.append('svg:circle')
    .attr('class', 'node')
    .attr('r', 12)
    .style('fill', function(d) { return (d === selected_node) ? d3.rgb(colors(d.id)).brighter().toString() : colors(d.id); })
    .style('stroke', function(d) { return d3.rgb(colors(d.id)).darker().toString(); })
    .classed('reflexive', function(d) { return d.reflexive; })
    .classed('digraph-highlighted',function(d) {return d.highlighted;})
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

      //document.getElementById("constructorString").innerHTML = "Macaulay2 Constructor: " + digraph2M2Constructor(nodes,links);
      
      // (Brett) Removing incidence and adjacency matrices for now.
      /*document.getElementById("incString").innerHTML = "Incidence Matrix: " + arraytoM2Matrix(getIncidenceMatrix(nodes,links));
      document.getElementById("adjString").innerHTML = "Adjacency Matrix: " + arraytoM2Matrix(getAdjacencyMatrix(nodes,links));*/

      // select new link
      if (curEdit) selected_link = link;
      selected_node = null;
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

  if(!curEdit || d3.event.shiftKey || mousedown_node || mousedown_link) return;

  // insert new node at point

  var point = d3.mouse(this);
  var curName = lastNodeId + 1;
  while(checkName(curName.toString())){
      curName += 1;
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
  node = {id: ++lastNodeId, name: curName, reflexive: false, highlighted: false};
  node.x = point[0];
  node.y = point[1];
  if (!forceOn) {
    node.fixed = true;
  }
  nodes.push(node);
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

  if(!selected_node && !selected_link) return;
  switch(d3.event.keyCode) {
    case 8: // backspace
    case 46: // delete
      if(curEdit && selected_node) {
        nodes.splice(nodes.indexOf(selected_node), 1);
        spliceLinksForNode(selected_node);
        if(curHighlight) unHighlightAll();
      } else if(curEdit && selected_link) {

        links.splice(links.indexOf(selected_link), 1);
        if(curHighlight) unHighlightAll();
      }
      selected_link = null;
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
          
      case 66: // B
      if(selected_link) {
        // set link direction to both left and right
        selected_link.left = true;
        selected_link.right = true;
      }
      restart();
      break;
    case 76: // L
      if(selected_link) {
        // set link direction to left only
        selected_link.left = true;
        selected_link.right = false;
      }
      restart();
      break;
    case 82: // R
      if(selected_node && curEdit) {
        // toggle node reflexivity
        selected_node.reflexive = !selected_node.reflexive;
      } else if(selected_link && curEdit) {
        // set link direction to right only
        selected_link.left = false;
        selected_link.right = true;
      }
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
}

function disableEditing() {
  circle.call(drag);
  svg.classed('shift', true);
  //selected_node = null;
  selected_link = null;
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
  //console.log("curHighlight: "+curHighlight);
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
    
    // Update graph based on changes to nodes and links.
    restart();
}

function highlightAllNeighbors(n) {
    // Highlight all nodes that are neighbors with the given node n.
    for (var i = 0; i<nodes.length; i++) {
       //console.log(areNeighbors(nodes[i],n));
       nodes[i].highlighted = areNeighbors(nodes[i],n);
    }
    
    // Highlight all links that have the given node n as a source or target.
    for (var i = 0; i<links.length; i++) {
       links[i].highlighted = ((links[i].source === n) || (links[i].target === n));
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
    //console.log("resizing window");
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

// Functions to construct M2 constructors for graph, incidence matrix, and adjacency matrix.

function digraph2M2Constructor( nodeSet, edgeSet ){
  var d = nodeSet.length;
  // Handle the empty digraph first.
  if (d==0) {
      return "digraph({})";
  }
  var strEdges = "{";
  var e = edgeSet.length;
  var strNodes = "{";
  var foundReflexive = false;
  for( var i = 0; i < d; i++ ){
    if(i != (d-1)){
      strNodes = strNodes + "\"" + (nodeSet[i].name).toString() + "\", ";
    }
    else {
      strNodes = strNodes + "\"" + (nodeSet[i].name).toString() + "\"}";
    }
    // Add any reflexive nodes as an edge from the vertex to itself.
    if(nodeSet[i].reflexive){
        strEdges = strEdges + "{\"" + nodeSet[i].name.toString() + "\", \"" + nodeSet[i].name.toString() + "\"}, ";
        foundReflexive = true;
    }
  }

  for( var i = 0; i < e; i++ ){
    var leftEdge = edgeSet[i].left;
    var rightEdge = edgeSet[i].right;
    if(i != (e-1)){
      if(leftEdge){ strEdges = strEdges + "{\"" + (edgeSet[i].target.name).toString() + "\",\"" + (edgeSet[i].source.name).toString() + "\"}, ";}
      if(rightEdge){ strEdges = strEdges + "{\"" + (edgeSet[i].source.name).toString() + "\",\"" + (edgeSet[i].target.name).toString() + "\"}, ";}
    }
    else{
      if(leftEdge && rightEdge){ strEdges = strEdges + "{\"" + (edgeSet[i].target.name).toString() + "\",\"" + (edgeSet[i].source.name).toString() + "\"}, " + "{\"" + (edgeSet[i].source.name).toString() + "\",\"" + (edgeSet[i].target.name).toString() + "\"}}";}
      else if(leftEdge){ strEdges = strEdges + "{\"" + (edgeSet[i].target.name).toString() + "\",\"" + (edgeSet[i].source.name).toString() + "\"}}";}
      else if(rightEdge){ strEdges = strEdges + "{\"" + (edgeSet[i].source.name).toString() + "\",\"" + (edgeSet[i].target.name).toString() + "\"}}";}
    }
  }
    
  // Handle some extremal cases such as the empty digraph, or digraphs with no edges.
  if(foundReflexive && (edgeSet.length == 0)){
      strEdges = strEdges.slice(0,-2) + "}";
  }
  if((!foundReflexive) && (edgeSet.length == 0)){
      strEdges = "{}";
  }
    
  return "digraph(" + strNodes + "," + strEdges + ")";

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
  var loops = [];
  for(var i = 0; i < nodes.length; i++){
    points[i] = [nodes[i].x.toString()+"/"+nodes[i].y.toString()+"/"+nodes[i].id+"/"+nodes[i].name];
    if (nodes[i].reflexive) {
        loops.push(nodes[i].id);
    }
  }

  // arrow direction for TikZ
  var leftright;

  var edges = [];
  for(var j = 0; j < links.length; j++){
    // determines the arrow direction for TikZ
    if (links[j].left  && (links[j].left == links[j].right)) {
      leftright = "<->";
    } else if (links[j].left) {
      leftright = "<-";
    } else {
      leftright = "->";
    }
    edges[j] = [ links[j].source.id.toString()+"/"+links[j].target.id.toString()+"/"+leftright ];
  }

//console.log(links);

  var timestamp = makeid();

  var tikzTex = "";
  tikzTex =  "\\begin{tikzpicture}\n" +
    "\\newcommand*\\points" + timestamp + "{" + points + "}\n" +
    "\\newcommand*\\edges" + timestamp + "{" + edges + "}\n" +
    "\\newcommand*\\loops" + timestamp + "{" + loops + "}\n" +
    "\\newcommand*\\scale" + timestamp + "{0.02}\n" +
    "\\foreach \\x/\\y/\\z/\\w in \\points" + timestamp + "\n" +
    "  \\node (\\z) at (\\scale" + timestamp + "*\\x,-\\scale" + timestamp +
    "*\\y) [circle,draw,inner sep=0pt] {$\\w$};\n" +
    "\\foreach \\x/\\y/\\z in \\edges" + timestamp + "\n" +
    "  \\draw[\\z] (\\x) -- (\\y);\n" +
    "\\foreach \\x in \\loops" + timestamp + "\n" +
    "  \\path (\\x) edge [->,in=120,out=60,loop,looseness=6] node {} (\\x);" +
    "\n\\end{tikzpicture}\n" +
    "% \\points" + timestamp +
    " is point set in the form x-coord/y-coord/node ID/node label\n" +
    "% \\edges" + timestamp +
    " is edge set in the form Source ID/Target ID/arrow direction\n" +
    "% \\scale" + timestamp +
    " makes the picture able to be viewed on the page\n";

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

    else if (clickTest == "isEulerian") {
      d3.select("#isEulerian").html("&nbsp;&nbsp; isEulerian :: <b>"+m2Response+"</b>");    
    }    

    else if (clickTest == "isStronglyConnected") {
      d3.select("#isStronglyConnected").html("&nbsp;&nbsp; isStronglyConnected :: <b>"+m2Response+"</b>");    
    }    

    else if (clickTest == "isWeaklyConnected") {
      d3.select("#isWeaklyConnected").html("&nbsp;&nbsp; isWeaklyConnected :: <b>"+m2Response+"</b>");    
    }    
    
}


// Anytime the graph is edited by user we call this function.
// It changes the menu items to default.
function menuDefaults() {
  d3.select("#hasEulerianTrail").html("&nbsp;&nbsp; hasEulerianTrail");
  d3.select("#isEulerian").html("&nbsp;&nbsp; isEulerian");  
  d3.select("#isStronglyConnected").html("&nbsp;&nbsp; isStronglyConnected");  
  d3.select("#isWeaklyConnected").html("&nbsp;&nbsp; isWeaklyConnected");
    
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