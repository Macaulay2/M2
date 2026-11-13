import * as d3 from "d3";

// Pure utility functions

export function getNextAlpha(alpha) {
  return String.fromCharCode(alpha.charCodeAt(0) + 1);
}

export function makeid() {
  var randomtext = "";
  var randompossible = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz";
  for (var i = 0; i < 5; i++)
    randomtext += randompossible.charAt(
      Math.floor(Math.random() * randompossible.length),
    );
  return randomtext;
}

function createCORSRequest(method, url) {
  var xhr = new XMLHttpRequest();
  if ("withCredentials" in xhr) {
    xhr.open(method, url, true);
  } else if (typeof XDomainRequest != "undefined") {
    xhr = new XDomainRequest();
    xhr.open(method, url);
  } else {
    xhr = null;
  }
  return xhr;
}

export function makeCorsRequest(method, url, browserData, onclickResults) {
  var xhr = createCORSRequest(method, url);
  if (!xhr) {
    alert("CORS not supported");
    return;
  }
  xhr.onload = function () {
    onclickResults(xhr.responseText);
  };
  xhr.send(browserData);
}

export function arraytoM2Matrix(arr) {
  var str = "matrix{{";
  for (var i = 0; i < arr.length; i++) {
    for (var j = 0; j < arr[i].length; j++) {
      str = str + arr[i][j].toString();
      if (j == arr[i].length - 1) {
        str = str + "}";
      } else {
        str = str + ",";
      }
    }
    if (i < arr.length - 1) {
      str = str + ",{";
    } else {
      str = str + "}";
    }
  }
  return str;
}

export function arraytoM2List(arr) {
  var str = "{{";
  for (var i = 0; i < arr.length; i++) {
    for (var j = 0; j < arr[i].length; j++) {
      str = str + arr[i][j].toString();
      if (j == arr[i].length - 1) {
        str = str + "}";
      } else {
        str = str + ",";
      }
    }
    if (i < arr.length - 1) {
      str = str + ",{";
    } else {
      str = str + "}";
    }
  }
  return str;
}

// D3 drag event handlers (no external state dependencies)

export function dragstart(d) {
  d.fx = d.x;
  d.fy = d.y;
}

export function dragged(d) {
  d.fx = d3.event.x;
  d.fy = d3.event.y;
}

// State-dependent functions (take explicit arguments)

export function checkName(name, nodes) {
  for (var i = 0; i < nodes.length; i++) {
    if (nodes[i].name == name) return true;
  }
  return false;
}

export function spliceLinksForNode(node, nodes, links) {
  var toSplice = links.filter(function (l) {
    return l.source === node || l.target === node;
  });
  toSplice.map(function (l) {
    links.splice(links.indexOf(l), 1);
  });
}

export function setAllNodesFixed(nodes) {
  for (var i = 0; i < nodes.length; i++) {
    nodes[i].fx = nodes[i].x;
    nodes[i].fy = nodes[i].y;
  }
}

export function setAllNodesUnfixed(nodes) {
  for (var i = 0; i < nodes.length; i++) {
    nodes[i].fx = null;
    nodes[i].fy = null;
  }
}

export function hideLabels(circle) {
  circle.select("text").remove();
}

export function showLabels(circle) {
  circle
    .append("svg:text")
    .attr("x", 0)
    .attr("y", 4)
    .attr("class", "id noselect")
    .attr("pointer-events", "none")
    .text(function (d) {
      return d.name;
    });
}

// updateForceCharge and updateForceLinkDist read/write the globals forceOn,
// forceCharge, forceLinkDist which are set by the HTML template.
export function updateForceCharge(force, chargeSlider, toggleForceFn) {
  if (!forceOn) {
    toggleForceFn();
  }
  forceCharge = -chargeSlider.noUiSlider.get();
  force
    .force("charge", d3.forceManyBody().strength(forceCharge))
    .alpha(1)
    .restart();
}

export function updateForceLinkDist(force, linkDistSlider, toggleForceFn) {
  if (!forceOn) {
    toggleForceFn();
  }
  forceLinkDist = linkDistSlider.noUiSlider.get();
  force.force("link").distance(forceLinkDist);
  force.alpha(1).restart();
}

// DOM setup helpers

export function initSliders() {
  var chargeSlider = document.getElementById("charge-slider");
  noUiSlider.create(chargeSlider, {
    start: [1500],
    range: { min: [0], max: [6000] },
  });
  var linkDistSlider = document.getElementById("linkdist-slider");
  noUiSlider.create(linkDistSlider, {
    start: [100],
    range: { min: [0], max: [400] },
  });
  return { chargeSlider: chargeSlider, linkDistSlider: linkDistSlider };
}

export function initSideMenu(updateWindowSizeFn) {
  $("#side").BootSideMenu({
    side: "right",
    closeOnClick: false,
    width: "230px",
  });
  document.getElementsByClassName("toggler")[0].addEventListener(
    "mousedown",
    function () {
      menuOpen = !menuOpen;
      updateWindowSizeFn();
    },
    false,
  );
  window.addEventListener("resize", updateWindowSizeFn, false);
}
