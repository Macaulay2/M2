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

  var maxGroup = 1;
  var rowSep = 0;
  var hPadding = 30;
  var vPadding = 30;

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
    scriptSource = scriptSource.substring(0, scriptSource.length - 14);

function initializeBuilder() {
  // Set up SVG for D3.
  width  = window.innerWidth-document.getElementById("side").clientWidth;
  height = window.innerHeight-10;
  colors = d3.scale.category10();
  
  svg = d3.select('body')
    .append('svg')
    .attr('width', width)
    .attr('height', height)
    .attr('id', 'canvasElement2d');
    
  if(fixExtremalNodes){
      document.getElementById("extremalNodeToggle").text = "Don't fix extremal nodes";
  }

  // Compute the minimal covering relations from the poset relation matrix.  This is necessary before computing the group for each node.
  dataCovRel = minimalPosetRelations(dataRelMatrix);
    
  // Calculate appropriate groups for elements based on rank or height and store them in the global variable dataGroupList.
  dataGroupList = computeNodeGroups(dataRelMatrix);
    
  // Compute the maximum level of the nodes in the poset.
  maxGroup = d3.max(dataGroupList);
  // Compute the distance between levels in the poset.  Make sure the denominator is positive.
  rowSep = (height-2*vPadding)/d3.max([maxGroup,1]);
      
  // Set up initial nodes and links
  //  - nodes are known by 'id', not by index in array.
  //  - links are always source < target; edge directions are set by 'left' and 'right'.
  //var data = dataData;
  //var names = labelData;
    
  // Initialize nodes so that they will be equally spaced along each level.
  nodes = nodesFromLabelsGroups(dataLabels,dataGroupList);
  lastNodeId = nodes.length;
  setAllNodesFixed();

  // Initialize the links based on the minimal covering relations obtained from the relation matrix.
  links = linksFromNodesRelations(nodes,dataCovRel);

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

function resetPoset() {
  
  // Set the x-coordinate of each node to the original fixed layout.
  var groupFreq = [];
  var groupCount = [];
  for (var i=0; i<maxGroup+1; i++){
    groupFreq.push(0);
    groupCount.push(0);
  }
  nodes.forEach(function(d){groupFreq[d.group]=groupFreq[d.group]+1;});
    
  for( var i = 0; i < nodes.length; i++ ){
    groupCount[nodes[i].group]=groupCount[nodes[i].group]+1;
    nodes[i].x = groupCount[nodes[i].group]*((width-2*hPadding)/(groupFreq[nodes[i].group]+1));
    nodes[i].px = groupCount[nodes[i].group]*((width-2*hPadding)/(groupFreq[nodes[i].group]+1));
  }
   
  setAllNodesFixed();
  
  // Calling tick() here is crucial so that the locations of the nodes are updated.
  tick();
    
  // Update the side menu bar to reflect that all nodes are now fixed in their original positions.
  if(forceOn) toggleForce();
  
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
  
  // Make sure all nodes stay at the height corresponding to their group.
  for( var i = 0; i < nodes.length; i++ ){
    nodes[i].y = height-vPadding-(nodes[i].group)*rowSep;
    nodes[i].py = height-vPadding-(nodes[i].group)*rowSep;
  }
    
    // Restrict the nodes to be contained within a 15 pixel margin around the svg.
  circle.attr('transform', function(d) {
    if (d.x > width - 15) {
      d.x = width - 15;
    }
    else if (d.x < 15) {
      d.x = 15;
    }
    
    // Visually update the locations of the nodes based on the force simulation.
    return 'translate(' + d.x + ',' + d.y + ')';
  });
    
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

  // Update existing nodes (highlighted & selected visual states).
  circle.selectAll('circle')
    // If a node is currently selected, then make it brighter.
    .style('fill', function(d) { return (d === selected_node) ? d3.rgb(colors(d.id)).brighter().toString() : (d.highlighted ? '#FF0000' : colors(d.id)); })
    .classed('highlighted', function(d) { return d.highlighted; })
    .attr('group', function(d) {return d.group;});
  
  // Update the text on each circle with the name of the corresponding node.
  //circle.select("text").text(function(d) {console.log(d.name); return d.name;});

  // Add new nodes.
  var g = circle.enter().append('svg:g');

  g.append('svg:circle')
    .attr('class', 'node')
    .attr('r', 12)
    .style('fill', function(d) { return (d === selected_node) ? d3.rgb(colors(d.id)).brighter().toString() : (d.highlighted ? '#FF0000' : colors(d.id)); })
    .style('stroke', function(d) { return d3.rgb(colors(d.id)).darker().toString(); })
    .classed('highlighted',function(d) {return d.highlighted;})
    .on('mouseover', function(d) {
      // If no node has been previously clicked on or if the user has not dragged the cursor to a different node after clicking,
      // then do nothing.
      if (!mousedown_node || d === mousedown_node) return;
      // Otherwise enlarge the target node.
      d3.select(this).attr('transform', 'scale(1.1)');
    })
    .on('mouseout', function(d) {
      // If no node has been previously clicked on or if the user has not dragged the cursor to a different node after clicking, then do nothing.
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
            if(curHighlight) highlightAllComparable(selected_node);
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

      // Hide the drag line.
      drag_line
        .classed('hidden', true)
        .style('marker-end', '');

      // If the user dragged a line from a node to itself, reset all mouse variables.
      mouseup_node = d;
      if(mouseup_node === mousedown_node) { resetMouseVars(); return; }

      // Unenlarge the node that the mouse was released on.
      d3.select(this).attr('transform', '');

      // The user has attempted to add a new relation.
      var link = null;
      var sourceID = mousedown_node.id;
      var targetID = mouseup_node.id;
      // If the source node is already less than the target node, do nothing.
      if(dataRelMatrix[sourceID][targetID] != 1){
        // Otherwise, determine whether the relation can be added.
        var tempMatrix = completeTransitiveClosure(dataRelMatrix,sourceID,targetID);
        if(!tempMatrix){
            alert("Creating this relation would violate antisymmetry.");
        } else {
            force.stop();
            dataRelMatrix = tempMatrix.slice();
            dataCovRel = minimalPosetRelations(dataRelMatrix);
            dataGroupList = computeNodeGroups(dataRelMatrix);
            maxGroup = d3.max(dataGroupList);
            // Make sure the denominator is positive.
            rowSep = (height-2*vPadding)/d3.max([maxGroup,1]);
            
            // Running restart on empty arrays here clears out the circle and path groups so that they are rebuilt from scratch referring to the new node and links below.
            nodes = [];
            links = [];
            restart();
            
            nodes = nodesFromLabelsGroups(dataLabels,dataGroupList);
            lastNodeId = nodes.length;
            setAllNodesFixed();
            links = linksFromNodesRelations(nodes,dataCovRel);
            force.nodes(nodes)
              .links(links);
            tick();
            resetMouseVars();
            menuDefaults();
            // 'link' is the relation that was just added.
            link = links.filter(function(l) {
                return (l.source.id == d3.min([sourceID,targetID]) && l.target.id == d3.max([sourceID,targetID]));
            })[0];
                        
            // Update the side menu bar to reflect that all nodes are now fixed in their original positions.
            if(forceOn) toggleForce();
        }
      }
      
      // Select the newly created link and unselect the previously selected node.
      if (curEdit) selected_link = link;
      selected_node = null;
      if (curHighlight) unHighlightAll();
      restart();
    })

  .on('dblclick', function(d) {
      name = "";
      // The "/^" in the following regular expression allows ^ as a possible character in names, which may be useful if we want to label the poset elements by monomials, for example.
      var letters = /^[/^0-9a-zA-Z_]+$/;
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
        // Check to see whether there already exists a node with the given name.
        else if (checkName(name)) {
          alert('Sorry, a node with that name already exists.')
          name = "";
        }
      }
            
      if(name != "null") {
        d.name = name;
        d3.select(this.parentNode).select("text").text(function(d) {return d.name});
        // Update the appropriate entry of the global variable dataLabels with the new node name.
        dataLabels[d.id] = name;
      }

    });

  if(labelsOn){
  // Add text for names for any new nodes.
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

  // Remove the old nodes.
  circle.exit().remove();

  // Set the force layout in motion.
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

  // If editing is not on, the shift key is being held down, or a node or link has been selected, do nothing.
  if(!curEdit || d3.event.shiftKey || mousedown_node || mousedown_link) return;

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

  // Adding a new minimal node with x-value given by the mouse position on click.
  var node = {id: lastNodeId++, group: 0, name: curName, highlighted: false};
  node.x = point[0];
  node.y = height-vPadding;
  if (!forceOn) {
    node.fixed = true;
  }
  nodes.push(node);
  // Add the name of the new node to the global array dataLabels.
  dataLabels.push(curName);
  // Add a new row and column to the global array dataRelMatrix with a 1 in the bottom row corresponding to the new node.
  dataRelMatrix = extendMatrix(dataRelMatrix);
    
  // Graph is updated here so we change some items to default.
  menuDefaults();

  restart();
}

function mousemove() {
  // If the user has not selected a node, do nothing.
  if(!mousedown_node) return;

  // Update the drag line.
  if(curEdit){
    drag_line.attr('d', 'M' + mousedown_node.x + ',' + mousedown_node.y + 'L' + d3.mouse(this)[0] + ',' + d3.mouse(this)[1]);
  }
    
  restart();
}

function mouseup() {
  if(mousedown_node) {
    // Hide the drag line.
    drag_line
      .classed('hidden', true)
      .style('marker-end', '');
  }

  // because :active only works in WebKit?
  svg.classed('active', false);

  // Reset mouse event variables.
  resetMouseVars();

  restart();

}

// Remove all links involving a given node.
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

  // If no key has been pressed, do nothing.
  if(lastKeyDown !== -1) return;
  lastKeyDown = d3.event.keyCode;

  // shift
  if(d3.event.keyCode === 16) {
    circle.call(drag);
    svg.classed('shift', true);
  }

  // The rest of the key presses only apply when a node or link is currently selected.
  if(!selected_node && !selected_link) return;
    
  // -----------------------------------------------------
    
  switch(d3.event.keyCode) {
    case 8: // backspace
    case 46: // delete
      if(curEdit && selected_node) {
        // Delete the selected node and update the poset.
        dataLabels.splice(selected_node.id,1);
        force.stop();
        dataRelMatrix = deleteRowCol(dataRelMatrix,selected_node.id);
        dataCovRel = minimalPosetRelations(dataRelMatrix);
        dataGroupList = computeNodeGroups(dataRelMatrix);
        maxGroup = d3.max(dataGroupList);
        // Make sure the denominator is positive.
        rowSep = (height-2*vPadding)/d3.max([maxGroup,1]);
        
        nodes = [];
        links = [];
        restart();
          
        nodes = nodesFromLabelsGroups(dataLabels,dataGroupList);
        lastNodeId = nodes.length;
        setAllNodesFixed();
        links = linksFromNodesRelations(nodes,dataCovRel);
        force.nodes(nodes)
          .links(links);
        resetMouseVars();

        if(curHighlight) unHighlightAll();
        // Update the side menu bar to reflect that all nodes are now fixed in their original positions.
        if(forceOn) toggleForce();
          
      } else if(curEdit && selected_link) {
        // Delete the selected link and update the poset.
        var sourceID = selected_link.source.id;
        var targetID = selected_link.target.id;
        // Make the nodes from the selected covering relation incomparable.
        dataRelMatrix[sourceID][targetID] = 0;
        dataRelMatrix[targetID][sourceID] = 0;
        // Update the minimal covering relation and node ranks.
        dataCovRel = minimalPosetRelations(dataRelMatrix);
        dataGroupList = computeNodeGroups(dataRelMatrix);
        maxGroup = d3.max(dataGroupList);
        // Make sure the denominator is positive.
        rowSep = (height-2*vPadding)/d3.max([maxGroup,1]);
        
        nodes = [];
        links = [];
        restart();
        
        nodes = nodesFromLabelsGroups(dataLabels,dataGroupList);
        lastNodeId = nodes.length;
        setAllNodesFixed();
        links = linksFromNodesRelations(nodes,dataCovRel);
        force.nodes(nodes)
          .links(links);
        resetMouseVars();
        
        if(curHighlight) unHighlightAll();
        
        // Update the side menu bar to reflect that all nodes are now fixed in their original positions.
        if(forceOn) toggleForce();
      }
      selected_link = null;
      if(curEdit) {selected_node = null;}

      // Graph Changed :: deleted nodes and links
      // as a result we change some items to default    
      menuDefaults();
     
      restart();
      break;
  }
  // ------------------------------------------------------
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
  dataMaxChains = posetMaximalChains(dataRelMatrix);
  if(!selected_node) return;
  highlightAllComparable(selected_node);
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

// This function uses the global variable dataRelMatrix to determine all of the nodes which are comparable to the given node, then highlights all of these nodes and any links involving two such nodes.
function highlightAllComparable(n) {
    unHighlightAll();
    var rel = allPosetRelations(dataRelMatrix);
    var compElt = [];
    for(var i=0; i < rel.length; i++){
        // Create array of all elements comparable to n.
        if(rel[i][1] == n.id){compElt.push(rel[i][0]);}
        if(rel[i][0] == n.id){compElt.push(rel[i][1]);}
    }
    // Highlight all comparable nodes.
    for(var i=0; i < compElt.length; i++){
        nodes[compElt[i]].highlighted = true;
    }
    
    // Highlight all links whose source and target are both comparable to n.
    for (var i = 0; i<links.length; i++) {
       links[i].highlighted = (containedIn(links[i].source.id,compElt) && containedIn(links[i].target.id,compElt));
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

// This method returns the groups of the nodes based on rank or height, as appropriate.
function computeNodeGroups(relMatrix){
  if(fixExtremalNodes){
      return posetRelHeightFunction(relMatrix);
  } else {
      if(posetIsRanked(dataRelMatrix)){
          return posetRankFunction(relMatrix);
      } else {
          return posetHeightFunction(relMatrix);
      }
  }
}

// This function returns an array of nodes with appropriate x and y values so that the poset elements will be equally distributed along each level.
function nodesFromLabelsGroups(labelList,groupList) {
  var temp = [];
  for(var i=0; i < labelList.length; i++){
      temp.push({id: i, name: labelList[i], group: groupList[i], highlighted: false});
  }
  var groupFreq = [];
  var groupCount = [];
  for (var i=0; i<maxGroup+1; i++){
    groupFreq.push(0);
    groupCount.push(0);
  }
  temp.forEach(function(d){groupFreq[d.group]=groupFreq[d.group]+1;});
  
  for(var i=0; i < temp.length; i++){
      // Set the nodes as fixed by default and specify their initial x and y values to be evenly spaced along their level.
	  temp[i].y = height-vPadding-temp[i].group*rowSep;
      temp[i].py = height-vPadding-temp[i].group*rowSep;
      groupCount[temp[i].group]=groupCount[temp[i].group]+1; 
      temp[i].x = groupCount[temp[i].group]*((width-2*hPadding)/(groupFreq[temp[i].group]+1));
      temp[i].px = groupCount[temp[i].group]*((width-2*hPadding)/(groupFreq[temp[i].group]+1));
  }
  return temp;
}

// This function returns a list of the links representing minimal covering relations based on the constructed nodes and a list of minimal relations.
function linksFromNodesRelations(nodeList,relList){
  var temp = [];
  for(var i=0; i < relList.length; i++){
      // Since the links always go from smaller node id to larger node id, determine which elements in each covering relation have smaller and larger node id.
      var minNode = d3.min(dataCovRel[i]);
      var maxNode = d3.max(dataCovRel[i]);
      
      // If the first element in the covering relation has the smaller node id, then we should set the link to point 'right' (from smaller node id to larger id, which matches the order given in the covering relation).
      //var rightLink = (minNode == dataCovRel[i][0]);
      //var leftLink = !rightLink;
      
      // Brett: Setting left or right to true creates a little padding around the target node.  Do we need to do this now that we are keeping track of the relation matrix?
      // links.push({source: minNode, target: maxNode, left: leftLink, right: rightLink});
      
      temp.push({source: nodeList[minNode], target: nodeList[maxNode], left: false, right: false, highlighted: false});
  }
  return temp;
}

function updateWindowSize2d() {
    //var svg = document.getElementById("canvasElement2d");
    
    // get width/height with container selector (body also works)
    // or use other method of calculating desired values
    if(!menuOpen){
        width = window.innerWidth-10;
    } else {
        width = window.innerWidth - document.getElementById("side").clientWidth;
    }
    height = window.innerHeight-10;
    // Make sure the denominator is positive.
    rowSep = (height-2*vPadding)/d3.max([maxGroup,1]);

    // set attrs and 'resume' force 
    svg.attr('width', width);
    svg.attr('height', height);

    if(!forceOn){resetPoset();}
    force.size([width, height]).resume();
}

// Functions to construct M2 constructors for poset, incidence matrix, and adjacency matrix.  This references the global variable dataCovRel, which is updated with the minimal covering relations each time the poset is updated.

function poset2M2Constructor( labels ){
  if (labels.length == 0) {
      return "error \"No constructor for an empty poset.\"";
  }
  var covRel = idRelationsToLabelRelations(dataCovRel,labels);
  var relString = nestedArraytoM2List(covRel);
  var labelString = "{";
  var m = labels.length;
  for( var i = 0; i < m; i++ ){
    if(i != (m-1)){
      labelString = labelString + "\"" + labels[i].toString() + "\", ";
    }
    else{
      labelString = labelString + "\"" + labels[i].toString() + "\"}";
    }
  }
  return "poset("+labelString+","+relString+")";
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
    adjMatrix[edgeSet[i].source.id][edgeSet[i].target.id] = 1; // Set matrix entries corresponding to adjacencies to 1.
    adjMatrix[edgeSet[i].target.id][edgeSet[i].source.id] = 1;
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


// ----------- Functions for computations with posets ---------

// Given the relation matrix for a poset, this function returns null if the poset is not ranked and otherwise returns a list of the ranks of the elements.  This algorithm is taken from Posets.m2.
function posetRankFunction(relMatrix){
    var rk = [];
    // Initialize rk with arrays of the form [node index,0].  The second entry will be updated in the next loop and will eventually be the rank that the node is assigned.
    for(var i=0; i < relMatrix.length; i++){
        rk.push([i,0]);
    }
    var covRel = dataCovRel; // Current minimal covering relations.
    for(var i=0; i < covRel.length; i++){
        // 
        var tmp = rk[covRel[i][1]][rk[covRel[i][1]].length-1] - rk[covRel[i][0]][rk[covRel[i][1]].length-1] - 1;
        var u = rk[covRel[i][0]][0];
        var v = rk[covRel[i][1]][0];
        if(u == v){
            if(tmp == 0) {
                continue;
            } else {
                return null;
            }
        }
        var temprk = [];
        if(tmp > 0){
            for(var j=0; j < rk.length; j++){
                if(rk[j][0] == u){
                    temprk.push([v,rk[j][rk[j].length-1] + tmp]);
                } else {
                    temprk.push(rk[j]);
                }
            }
        } else {
            for(var j=0; j < rk.length; j++){
                if(rk[j][0] == v){
                    temprk.push([u,rk[j][rk[j].length-1] - tmp]);
                } else {
                    temprk.push(rk[j]);
                }
            }
        }
        rk = temprk;
    }
    var rkOutput = [];
    for(var i=0; i < rk.length; i++){
        rkOutput.push(rk[i][1]);
    }
    return rkOutput;
}

// Given the relation matrix for a poset, this function determines whether the poset is ranked or not.
function posetIsRanked(relMatrix){
    return posetRankFunction(relMatrix) != null;
}

// Given the relation matrix for a poset, this function computes a filtration of the poset of the form [F_0,F_1,...].  F_0 consists of the minimal elements of the poset, F_1 consists of the minimal elements of P - F_0, and so on.  This algorithm is taken from Posets.m2, which was in turn taken from John Stembridge's Maple package for computations with posets.
function posetFiltration(relMatrix){
    //var covRel = minimalPosetRelations(relMatrix);
    var covRel = dataCovRel;
    var cnt = [];
    var cvrby = [];
    // For each element of the poset, determine how many times it occurs as the larger element in any minimal covering relation (listed in cnt).  Also, for each element of the poset, determine the minimal elements that cover it (listed in cvrby).
    for(var i=0; i < relMatrix.length; i++){
        var tempCount = 0;
        var tempCvrs = [];
        for(var j=0; j < covRel.length; j++){
            if(covRel[j][1] == i){tempCount = tempCount+1;}
            if(covRel[j][0] == i){tempCvrs.push(covRel[j][1]);}
        }
        cnt.push(tempCount);
        cvrby.push(tempCvrs);
    }
    // Find indices of all elements that do not minimally cover any element (i.e., the minimal elements in the poset).
    var neu = [];
    for(var i=0; i < cnt.length; i++){
        if(cnt[i] == 0){neu.push(i)};        
    }
    // We need neu.slice() here so that the neu array is cloned and ret points to the new occurrence of neu;
    var ret = [neu.slice()];
    while(neu.length > 0){
        var tempMinCvrs = [];
        for(var i=0; i < neu.length; i++){
            tempMinCvrs.push(cvrby[neu[i]]);
        }
        tempMinCvrs = flattenArray(tempMinCvrs);
        var tempArr = [];
        for(var i=0; i < tempMinCvrs.length; i++){
            if(cnt[tempMinCvrs[i]] == 1){
                tempArr.push(tempMinCvrs[i]);
            } else {
                cnt[tempMinCvrs[i]] = cnt[tempMinCvrs[i]] - 1;
                continue;
            }
        }
        neu = tempArr.slice();
        ret.push(tempArr.slice());
    }
    ret.splice(ret.length-1,1);
    return ret;
}

// Given the relation matrix for a poset, compute the "height" of each element, which is simply the corresponding level of the filtration of the poset that contains it.  This is meant to be used in place of a rank function if the poset is not ranked.
function posetHeightFunction(relMatrix){
    var filt = posetFiltration(relMatrix);
    var ht = [];
    for(var i=0; i < relMatrix.length; i++){
        for(var j=0; j < filt.length; j++){
            for(var k=0; k < filt[j].length; k++){
                if(filt[j][k] == i){ht.push(j);}
            }
        }
    }
    return ht;
}

// Given the relation matrix for a poset, compute the minimal elements.
function posetMinimalElements(relMatrix){
    var output = [];
    for(var i=0; i < relMatrix.length; i++){
        var isMin = true;
        for(var j=0; j < relMatrix.length; j++){
            // If the ith element covers another element, then it is not minimal.
            if((relMatrix[j][i] == 1) && (i != j)){isMin = false; break;}
        }
        if(isMin){output.push(i)};
    }
    return output;
}

// Given the relation matrix for a poset, compute the maximal elements.
function posetMaximalElements(relMatrix){
    var output = [];
    for(var i=0; i < relMatrix.length; i++){
        var isMax = true;
        for(var j=0; j < relMatrix.length; j++){
            // If the ith element is covered by another element, then it is not maximal.
            if((relMatrix[i][j] == 1) && (i != j)){isMax = false; break;}
        }
        if(isMax){output.push(i)};
    }
    return output;
}

// Given the relation matrix for a poset, compute the maximal chains.
function posetMaximalChains(relMatrix){
    var minElt = posetMinimalElements(relMatrix);
    var nonMaximalChains = [];
    for(var i=0; i < minElt.length; i++){
        nonMaximalChains.push([minElt[i]]);
    }
    var covRel = dataCovRel;
    var cvrby = [];
    for(var i=0; i < relMatrix.length; i++){
        var tempCvrs = [];
        for(var j=0; j < covRel.length; j++){
            if(covRel[j][0] == i){tempCvrs.push(covRel[j][1]);}
        }
        cvrby.push(tempCvrs);
    }
    var maxChains = [];
    while(nonMaximalChains.length > 0){
        var tempArr2 = [];
        for(var i=0; i < nonMaximalChains.length; i++){
            var tempArr = [];
            tempCvrs = cvrby[nonMaximalChains[i][nonMaximalChains[i].length-1]];
            if(tempCvrs.length == 0){
                // If a maximal chain is found, add it to maxChains.
                maxChains.push(nonMaximalChains[i].slice());
                continue;
            } else {
                // Otherwise, the chain can be extended further.  Create all possible extensions of the current chain with minimal covering elements.
                for (var j=0; j < tempCvrs.length; j++){
                    tempArr.push(nonMaximalChains[i].concat(tempCvrs[j]));
                }
                tempArr2.push(tempArr.slice());
            }
        }
        nonMaximalChains = flattenArray(tempArr2.slice());
    }
    return maxChains;
}

// // Given the relation matrix for a poset, compute the "relative height" of each element, which tries to keep elements evenly spaced relative to the length of the maximal chains in the poset.  This is meant to be used in place of a rank function is the poset is not ranked.
function posetRelHeightFunction(relMatrix){
    var maxChains = posetMaximalChains(relMatrix);
    var heightList = posetHeightFunction(relMatrix);
    // For each element of the poset, create a list of all maximal chains that involve that element.
    var maxChainList = [];
    for(var i=0; i < relMatrix.length; i++){
        var tempArr = [];
        for(var j=0; j < maxChains.length; j++){
            for(var k=0; k < maxChains[j].length; k++){
                // If the ith element appears in the jth maximal chain, then push the jth maximal chain to tempArr.
                if(maxChains[j][k] == i){tempArr.push(maxChains[j]);}
            }
        }
        maxChainList.push(tempArr.slice());
    }
    // For each element of the poset, find the lengths of all maximal chains that involve it.
    var chainLengthList = [];
    for(var i=0; i < maxChainList.length; i++){
        tempArr = [];
        for(var j=0; j < maxChainList[i].length; j++){
            tempArr.push(maxChainList[i][j].length);
        }
        chainLengthList.push(tempArr.slice());
    }
    var relHeightList = [];
    for(var i=0; i < chainLengthList.length; i++){
        relHeightList.push(d3.max(chainLengthList[i]) - 1);
    }
    var totalHeight = lcm(relHeightList);
    var output = [];
    for(var i=0; i < relMatrix.length; i++){
        output.push((totalHeight/relHeightList[i])*heightList[i]);
    }
    return output;
}

// Given the relation matrix for a set under a binary relation, this function determines whether the relation is antisymmetric (required for a poset) or not.
function posetIsAntisymmetric(relMatrix){
    var n = relMatrix.length;
    for(var i=0; i < n-1; i++){
        for(var j=i+1; j < n; j++){
            if((relMatrix[i][j] == 1) && (relMatrix[j][i] == 1)){
                return false;
            }            
        }
    }
    return true;
}

// Given the relation matrix for a poset, this function returns an array consisting of all relations in the poset (with nodes labeled by id).  This assumes that the rows and columns in the relation matrix are indexed according to the id of the nodes.  The relMatrix is given such that relMatrix[i][j] == 1 if and only if node_i <= node_j in the partial order.
function allPosetRelations (relMatrix){
    var tempArr = [];
    var n = relMatrix.length;
    for(var i=0; i < n; i++){
        for(var j=i; j < n; j++){
            // relMatrix[i][j] and relMatrix[j][i] can't both be 1 if i != j or else the poset would not be antisymmetric.
            if(relMatrix[i][j] == 1){tempArr.push([i,j]);}
            else {
                if(relMatrix[j][i] == 1){tempArr.push([j,i]);}
            }
        }
    }
    
    return tempArr;
}

// Given the relation matrix for a poset, this function returns an array consisting of minimal covering relations in the poset (with nodes labeled by id).  This assumes that the rows and columns in the relation matrix are indexed according to the id of the nodes.  The relMatrix is given such that relMatrix[i][j] == 1 if and only if node_j <= node_i in the partial order.  This algorithm is the same as the one used in Posets.m2.
function minimalPosetRelations (relMatrix){
    var n = relMatrix.length;
    var outputArr = [];
    var gtp = [];
    for(var i=0; i < n; i++){
        var temp = [];
        for(var j=0; j < n; j++){
            if((i != j) && (relMatrix[i][j] == 1)){temp.push(j)};
        }
        gtp.push(temp);
    }
    for(var i=0; i < n; i++){
        var gtgtp = [];
        var tempIndices = gtp[i];
        for(var j=0; j < tempIndices.length; j++){
            gtgtp.push(gtp[tempIndices[j]]);            
        }
        gtgtp = eliminateDuplicates(flattenArray(gtgtp));
        var trimIndices = setDifference(tempIndices,gtgtp);
        for(var j=0; j < trimIndices.length; j++){
            outputArr.push([i,trimIndices[j]]);            
        }        
    }
    
    return outputArr;    
}

// Given a square array of arrays (representing the relation matrix of a poset) and two distinct non-negative integers r and s, set the (r,s) entry equal to 1 (adding the relation r <= s) and also set all other (i,j) entries such that i <= r and s <= j (under the poset relation) equal to 1.  The method returns null if the resulting matrix is the relation matrix for a non-antisymmetric relation.
function completeTransitiveClosure(relMatrix,r,s){
    // If we already have s <= r in the poset, then adding the relation r <= s will make it non-antisymmetric.
    if(relMatrix[s][r] == 1){return null;}
    
    var rel = allPosetRelations(relMatrix);
    var pred = [];
    var anc = [];
    for(var i=0; i < rel.length; i++){
        // Create arrays of all predecessors of r and all successors of s.
        if(rel[i][1] == r){pred.push(rel[i][0]);}
        if(rel[i][0] == s){anc.push(rel[i][1]);}
    }
    for(var i=0; i < pred.length; i++){
        for(var j=0; j < anc.length; j++){
            // If some successor of s is already <= some predecessor of r then the resulting relation will not be antisymmetric, so return null.
            if(relMatrix[anc[j]][pred[i]] == 1){return null;}
            relMatrix[pred[i]][anc[j]] = 1;
        }
    }
    return relMatrix;
}

// Given a list of relations labeled by node id, return the corresponding list of relations labeled by the name of the corresponding node.
function idRelationsToLabelRelations (relArr,labelArr){
    var out = [];
    for(var i=0; i < relArr.length; i++){
        out.push([labelArr[relArr[i][0]],labelArr[relArr[i][1]]]);
    }
    return out;
}

// ----------- Helper functions for dealing with arrays. ----------

// Given a nested array of arrays, this flattens the array by one level.
function flattenArray (arr) {
    return [].concat.apply([], arr);
}

// Eliminate all duplicate entries in an array.
function eliminateDuplicates(arr) {
  var i,
      len=arr.length,
      out=[],
      obj={};

  for (i=0;i<len;i++) {
    obj[arr[i]]=0;
  }
  for (i in obj) {
    out.push(i);
  }
  return out;
}

// Take the set-theoretic difference of two arrays.  (i.e., Return the result of removing from the first array all common elements of the second array.)
function setDifference(arr1,arr2) {
    var len1 = arr1.length;
    var len2 = arr2.length;
    var delIndices = [];
    for(var i =0; i < len1; i++){
        for(var j=0; j < len2; j++){
            // If j[i] appears in arr2, then delete it.
            if(arr1[i] == arr2[j]){delIndices.push(i);}            
        }
    }
    delIndices = eliminateDuplicates(delIndices);
    var offset = 0;
    // Remove all elements of arr1 where common elements with arr2 were found.
    for(var i=0; i < delIndices.length; i++){
        arr1.splice(delIndices[i]-offset,1);
        offset = offset+1;        
    }
    
    return arr1;
}

// Compute the lcm of an array of integers.
function lcm(A) {
    var n = A.length, a = Math.abs(A[0]);
    for (var i = 1; i < n; i++)
     { var b = Math.abs(A[i]), c = a;
       while (a && b){ a > b ? a %= b : b %= a; } 
       a = Math.abs(c*A[i])/(a+b);
     }
    return a;
}

// Takes a rectangular array of arrays and returns a string which can be copy/pasted into M2.
function nestedArraytoM2List (arr){
  var str = "{{";
  for(var i = 0; i < arr.length; i++){
    for(var j = 0; j < arr[i].length; j++){
      str = str + "\"" + arr[i][j].toString() + "\"";
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

// Given an array and an element, this method returns true if the element is contained in the array, else it returns false.
function containedIn(n,arr){
    for(var i=0; i < arr.length; i++){
        if(arr[i] == n){return true;}
    }
    return false;
}

// Given a rectangular array of arrays (representing the relation matrix of a poset), this method appends a row and column with a 1 in the bottom right spot.
function extendMatrix (relMatrix){
    var tempRow = [];
    for(var i=0; i < relMatrix.length; i++){
        relMatrix[i].push(0);
        tempRow.push(0);
    }
    tempRow.push(1);
    relMatrix.push(tempRow);
    return relMatrix;
}

// Given a rectangular array of arrays (representing the relation matrix of a poset) and a non-negative integer n, this method deletes the nth row and nth column of the matrix.
function deleteRowCol (relMatrix,n){
    relMatrix.splice(n,1);
    for(var i=0; i < relMatrix.length; i++){
        relMatrix[i].splice(n,1);
    }
    return relMatrix;
}

// -----------------------------------------------------------


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

  var timestamp = makeid();

  var tikzTex = "";
  tikzTex =  "\\begin{tikzpicture}\n" +
    "\\newcommand*\\points" +timestamp + "{" + points + "}\n" +
    "\\newcommand*\\edges" + timestamp + "{" + edges + "}\n" +
    "\\newcommand*\\scale" + timestamp + "{0.02}\n" +
    "\\foreach \\x/\\y/\\z/\\w in \\points" + timestamp + "\n" +
    "  \\node (\\z) at (\\scale" + timestamp + "*\\x,-\\scale" + timestamp +
    "*\\y) [circle,draw,inner sep=0pt] {$\\w$};\n" +
    "\\foreach \\x/\\y in \\edges" + timestamp + "\n" +
    "  \\draw (\\x) -- (\\y);\n" +
    "\\end{tikzpicture}\n" +
    "% \\points" + timestamp +
    " is point set in the form x-coord/y-coord/node ID/node label\n" +
    "% \\edges" + timestamp +
    " is edge set in the form Source ID/Target ID\n" +
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
    tikzInput.style = "vertical-align:middle; width: 100%";
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
    listGroup.insertBefore(tikzDiv,listGroup.childNodes[16]);
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
    
    if (clickTest == "isAtomic"){
      d3.select("#isAtomic").html("&nbsp;&nbsp; isAtomic :: <b>"+m2Response+"</b>");
    } 
    
    else if (clickTest == "isBounded"){
      d3.select("#isBounded").html("&nbsp;&nbsp; isBounded :: <b>"+m2Response+"</b>");
    } 
    
    else if (clickTest == "isDistributive"){
      d3.select("#isDistributive").html("&nbsp;&nbsp; isDistributive :: <b>"+m2Response+"</b>");
    } 
    
    else if (clickTest == "isGeometric"){
      d3.select("#isGeometric").html("&nbsp;&nbsp; isGeometric :: <b>"+m2Response+"</b>");
    } 
    
    else if (clickTest == "isGraded"){
      d3.select("#isGraded").html("&nbsp;&nbsp; isGraded :: <b>"+m2Response+"</b>");
    } 
    
    else if (clickTest == "isLattice"){
      d3.select("#isLattice").html("&nbsp;&nbsp; isLattice :: <b>"+m2Response+"</b>");
    } 
    
    else if (clickTest == "isLowerSemilattice"){
      d3.select("#isLowerSemilattice").html("&nbsp;&nbsp; isLowerSemilattice :: <b>"+m2Response+"</b>");
    } 
    
    else if (clickTest == "isLowerSemimodular"){
      d3.select("#isLowerSemimodular").html("&nbsp;&nbsp; isLowerSemimodular :: <b>"+m2Response+"</b>");
    } 
    
    else if (clickTest == "isModular"){
      d3.select("#isModular").html("&nbsp;&nbsp; isModular :: <b>"+m2Response+"</b>");
    } 
    
    else if (clickTest == "isRanked"){
      d3.select("#isRanked").html("&nbsp;&nbsp; isRanked :: <b>"+m2Response+"</b>");
    } 
    
    else if (clickTest == "isSperner"){
      d3.select("#isSperner").html("&nbsp;&nbsp; isSperner :: <b>"+m2Response+"</b>");
    } 
    
    else if (clickTest == "isStrictSperner"){
      d3.select("#isStrictSperner").html("&nbsp;&nbsp; isStrictSperner :: <b>"+m2Response+"</b>");
    } 
    
    else if (clickTest == "isUpperSemilattice"){
      d3.select("#isUpperSemilattice").html("&nbsp;&nbsp; isUpperSemilattice :: <b>"+m2Response+"</b>");
    } 
    
    else if (clickTest == "isUpperSemimodular"){
      d3.select("#isUpperSemimodular").html("&nbsp;&nbsp; isUpperSemimodular :: <b>"+m2Response+"</b>");
    } 
    
    else if (clickTest == "dilworthNumber"){
      d3.select("#dilworthNumber").html("&nbsp;&nbsp; dilworthNumber :: <b>"+m2Response+"</b>");
    } 
}

// Anytime the graph is edited by user we call this function.
// It changes the menu items to default.
function menuDefaults() {
  d3.select("#isAtomic").html("&nbsp;&nbsp; isAtomic");
  d3.select("#isBounded").html("&nbsp;&nbsp; isBounded");
  d3.select("#isDistributive").html("&nbsp;&nbsp; isDistributive");
  d3.select("#isGeometric").html("&nbsp;&nbsp; isGeometric");
  d3.select("#isGraded").html("&nbsp;&nbsp; isGraded");
  d3.select("#isLattice").html("&nbsp;&nbsp; isLattice");
  d3.select("#isLowerSemilattice").html("&nbsp;&nbsp; isLowerSemilattice");
  d3.select("#isLowerSemimodular").html("&nbsp;&nbsp; isLowerSemimodular");
  d3.select("#isModular").html("&nbsp;&nbsp; isModular");
  d3.select("#isRanked").html("&nbsp;&nbsp; isRanked");
  d3.select("#isSperner").html("&nbsp;&nbsp; isSperner");
  d3.select("#isStrictSperner").html("&nbsp;&nbsp; isStrictSperner");
  d3.select("#isUpperSemilattice").html("&nbsp;&nbsp; isUpperSemilattice");
  d3.select("#isUpperSemimodular").html("&nbsp;&nbsp; isUpperSemimodular");
  d3.select("#dilworthNumber").html("&nbsp;&nbsp; dilworthNumber");
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