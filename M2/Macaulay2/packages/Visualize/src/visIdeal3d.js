import * as THREE from "three";

$('#side').BootSideMenu({side:"right", closeOnClick: false, width: "230px"});

$(document).ready(function(){

  // show the ideal generators when clicked
  $("#idealGens").on("click", function(){
    $(this).html("Ideal Generators: "  + labels);
  });
});

const makeXYZstring = function (x, y, z) {
  let xstr, ystr, zstr;
  if (x === 0) {
    xstr = "";
  } else if (x === 1) {
    xstr = "x";
  } else {
    xstr = "x<sup>" + x.toString() + "</sup>";
  }
  if (y === 0) {
    ystr = "";
  } else if (y === 1) {
    ystr = "y";
  } else {
    ystr = "y<sup>" + y.toString() + "</sup>";
  }
  if (z === 0) {
    zstr = "";
  } else if (z === 1) {
    zstr = "z";
  } else {
    zstr = "z<sup>" + z.toString() + "</sup>";
  }
  return xstr + ystr + zstr;
};

const labels = [];
for (let i = 0; i < dataGenz.length; i++) {
  labels.push(makeXYZstring(dataGenz[i][0], dataGenz[i][1], dataGenz[i][2]));
}

// TODO:
// * exportTikz
// * endSession

// TODO: upgrade to newer three.js (still at r116, released Apr 2020)
// problems:
// * THREE.Geometry has been removed, so need some rewriting
// * Debian three package is still at r111

let camera, scene, renderer, geometry, material, mesh, ideal, group;

let targetXRotation = 0;
let targetYRotation = 0;
let targetXRotationOnMouseDown = 0;
let targetYRotationOnMouseDown = 0;

let mouseX = 0;
let mouseY = 0;
let mouseXOnMouseDown = 0;
let mouseYOnMouseDown = 0;

let windowHalfX = window.innerWidth / 2;
let windowHalfY = window.innerHeight / 2;

let maxX = Math.max(...data.map((d) => d[0]));
maxX = maxX == 0 ? 1 : maxX;

let maxY = Math.max(...data.map((d) => d[1]));
maxY = maxY == 0 ? 1 : maxY;

let maxZ = Math.max(...data.map((d) => d[2]));
maxZ = maxZ == 0 ? 1 : maxZ;

const maxExponent = Math.max(maxX, maxY, maxZ);

function newMesh(point, geometry, materials) {
  let mesh;
  if (geometry.type == "BoxGeometry") {
    mesh = new THREE.Mesh(geometry, materials);
  } else {
    mesh = new THREE.LineSegments(geometry, materials);
  }
  mesh.position.x = point[0] - 1 / 2;
  mesh.position.y = point[1] - 1 / 2;
  mesh.position.z = point[2] - 1 / 2;
  mesh.updateMatrix();
  return mesh;
}

function processCubes(data) {
  let cubes = [];
  data.forEach(function (d) {
    cubes.push([d[1], d[2], d[0]]);
  });
  return cubes;
}

function init() {
  scene = new THREE.Scene();
  scene.background = new THREE.Color(0xffffff);
  camera = new THREE.PerspectiveCamera(
    45,
    window.innerWidth / window.innerHeight,
    0.1,
    1000,
  );
  // camera.position.set(7,10,15);
  camera.position.set(maxExponent * 2, 10, 15);
  camera.lookAt(new THREE.Vector3(0, 2, 0));

  renderer = new THREE.WebGLRenderer({ antialias: true });
  // renderer = new THREE.CanvasRenderer();
  renderer.setSize(window.innerWidth, window.innerHeight);

  document.body.appendChild(renderer.domElement);

  const line_material = new THREE.LineBasicMaterial({ linewidth: 3 });
  material = new THREE.MeshBasicMaterial({ color: 0x333333 });
  group = new THREE.Object3D();
  geometry = new THREE.Geometry();
  geometry.vertices.push(new THREE.Vector3(0, 0, 0));
  const s = 1;
  const cube = new THREE.CubeGeometry(s, s, s);
  const edges = new THREE.EdgesGeometry(cube);

  // wireframe using gl.TRIANGLES (interpreted as quads)

  const attributesQuads = {
    center: { type: "v4", boundTo: "faceVertices", value: [] },
  };
  const valuesQuads = attributesQuads.center.value;

  setupAttributes(cube, valuesQuads);
  const cubes_data = processCubes(data);

  cubes_data.forEach(function (cube_data) {
    mesh = newMesh(cube_data, cube, material);
    group.add(mesh);
    mesh = newMesh(cube_data, edges, line_material);
    group.add(mesh);
  });

  ideal = new THREE.Line(geometry, line_material);
  group.add(ideal);
  /* geometry = new THREE.CubeGeometry(1,1,1); */
  /* cube = new THREE.Mesh(geometry, material); */

  maxX = Math.max(...cubes_data.map((d) => d[2]));
  maxY = Math.max(...cubes_data.map((d) => d[0]));
  maxZ = Math.max(...cubes_data.map((d) => d[1]));

  let lwidth = 3;
  let lineMaterial = new THREE.LineBasicMaterial({
    color: 0x044000,
    opacity: 0.8,
    linewidth: lwidth,
  });

  let lineGeometry = new THREE.Geometry();
  lineGeometry.vertices.push(new THREE.Vector3(-1, -1, -1));
  lineGeometry.vertices.push(new THREE.Vector3(-1, -1, maxX + 3));
  let line = new THREE.Line(lineGeometry, lineMaterial);
  group.add(line);
  lineMaterial = new THREE.LineBasicMaterial({
    color: 0xaa0000,
    opacity: 0.8,
    linewidth: lwidth,
  });
  lineGeometry = new THREE.Geometry();
  lineGeometry.vertices.push(new THREE.Vector3(-1, -1, -1));
  lineGeometry.vertices.push(new THREE.Vector3(maxY + 3, -1, -1));
  line = new THREE.Line(lineGeometry, lineMaterial);
  group.add(line);
  lineMaterial = new THREE.LineBasicMaterial({
    color: 0x0000dd,
    opacity: 0.8,
    linewidth: lwidth,
  });
  lineGeometry = new THREE.Geometry();
  lineGeometry.vertices.push(new THREE.Vector3(-1, -1, -1));
  lineGeometry.vertices.push(new THREE.Vector3(-1, maxZ + 3, -1));
  line = new THREE.Line(lineGeometry, lineMaterial);
  group.add(line);

  line = new THREE.Line(lineGeometry, lineMaterial);
  group.add(line);
  scene.add(group);

  document.addEventListener("mousedown", onDocumentMouseDown, false);
  document.addEventListener("touchstart", onDocumentTouchStart, false);
  document.addEventListener("touchmove", onDocumentTouchMove, false);
  document.addEventListener("mousewheel", onMouseWheel, false);

  window.addEventListener("resize", onWindowResize, false);
}

function setupAttributes(geometry, values) {
  for (let f = 0; f < geometry.faces.length; f++) {
    const face = geometry.faces[f];

    if (face instanceof THREE.Face3) {
      values[f] = [
        new THREE.Vector4(1, 0, 0, 0),
        new THREE.Vector4(0, 1, 0, 0),
        new THREE.Vector4(0, 0, 1, 0),
      ];
    } else {
      values[f] = [
        new THREE.Vector4(1, 0, 0, 1),
        new THREE.Vector4(1, 1, 0, 1),
        new THREE.Vector4(0, 1, 0, 1),
        new THREE.Vector4(0, 0, 0, 1),
      ];
    }
  }
}

function animate() {
  requestAnimationFrame(animate);

  render();
}

function render() {
  group.rotation.x = group.rotation.x +=
    (targetXRotation - group.rotation.x) * 0.05;
  group.rotation.y = group.rotation.y +=
    (targetYRotation - group.rotation.y) * 0.05;

  renderer.render(scene, camera);
}

init();
animate();
function onWindowResize() {
  windowHalfX = window.innerWidth / 2;
  windowHalfY = window.innerHeight / 2;

  camera.aspect = window.innerWidth / window.innerHeight;
  camera.updateProjectionMatrix();

  renderer.setSize(window.innerWidth, window.innerHeight);
}

//

function onDocumentMouseDown(event) {
  //event.preventDefault();

  document.addEventListener("mousemove", onDocumentMouseMove, false);
  document.addEventListener("mouseup", onDocumentMouseUp, false);
  document.addEventListener("mouseout", onDocumentMouseOut, false);

  mouseXOnMouseDown = event.clientX - windowHalfX;
  mouseYOnMouseDown = event.clientY - windowHalfY;
  targetYRotationOnMouseDown = targetYRotation;
  targetXRotationOnMouseDown = targetXRotation;
}

function onDocumentMouseMove(event) {
  mouseX = event.clientX - windowHalfX;
  mouseY = event.clientY - windowHalfY;

  targetYRotation =
    targetYRotationOnMouseDown + (mouseX - mouseXOnMouseDown) * 0.02;
  targetXRotation =
    targetXRotationOnMouseDown + (mouseY - mouseYOnMouseDown) * 0.02;
}

function onDocumentMouseUp(event) {
  document.removeEventListener("mousemove", onDocumentMouseMove, false);
  document.removeEventListener("mouseup", onDocumentMouseUp, false);
  document.removeEventListener("mouseout", onDocumentMouseOut, false);
}

function onDocumentMouseOut(event) {
  document.removeEventListener("mousemove", onDocumentMouseMove, false);
  document.removeEventListener("mouseup", onDocumentMouseUp, false);
  document.removeEventListener("mouseout", onDocumentMouseOut, false);
}

function onDocumentTouchStart(event) {
  if (event.touches.length == 1) {
    event.preventDefault();

    mouseXOnMouseDown = event.touches[0].pageX - windowHalfX;
    targetRotationOnMouseDown = targetRotation;
  }
}

function onDocumentTouchMove(event) {
  if (event.touches.length == 1) {
    event.preventDefault();

    mouseX = event.touches[0].pageX - windowHalfX;
    targetRotation =
      targetRotationOnMouseDown + (mouseX - mouseXOnMouseDown) * 0.05;
  }
}

function onMouseWheel(event) {
  event = event ? event : window.event;
  var wheelData = event.detail ? event.detail : event.wheelDelta;
  console.log(event);
}

