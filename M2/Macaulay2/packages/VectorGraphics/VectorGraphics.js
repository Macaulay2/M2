function gfxInitMouse(el) {
    el.onmousedown = gfxMouseDown;
    el.onmouseleave = gfxMouseLeave;
    el.onmouseup = gfxMouseUp;
    el.onmousemove = gfxMouseMove;
    el.onclick = gfxMouseClick;
}

function gfxInitData(el) {
    if (el.namespaceURI!="http://www.w3.org/2000/svg") return;
    if (el.classList.contains("gfxauto")) return;
    if (!el.gfxdata) {
	el.gfxdata={};
	for (var v in el.dataset)
	    el.gfxdata[v]=eval("("+el.dataset[v]+")");
	// just in case, start the computation of matrices... (see gfxRecompute() for details)
	var mat = el.gfxdata.pmatrix? el.gfxdata.pmatrix : el.parentElement.gfxdata.cmatrix;
	if (!el.gfxdata.matrix) el.gfxdata.cmatrix = mat; else { el.gfxdata.cmatrix = new Matrix(el.gfxdata.matrix); el.gfxdata.cmatrix.leftmultiply(mat); }
	checkData(el);
	for (var i=0; i<el.children.length; i++)
	    gfxInitData(el.children[i]);
    }
}

// auto-rotation button
function gfxToggleRotation(event) {
    //    this.blur();
    //    var svgel=document.getElementById(svgid);
    //    if (!svgel) return;
    var svgel = this.parentElement; // weak but works
    if (!svgel.gfxdata) gfxInitData(svgel);
    if (!this.ondblclick) this.ondblclick= function(event) { event.stopPropagation(); }; // weak
    
    // the perspective matrix should *always* exist

    if (!svgel.gfxdata.cmatrix) svgel.gfxdata.cmatrix = new Matrix(svgel.gfxdata.pmatrix);
    if (this.classList.contains("active")) {
	clearInterval(this.intervalId);
	this.classList.remove("active");
    }
    else
    {
	this.classList.add("active");
	this.intervalId=setInterval(() => {
	    if ((!svgel)||(!document.body.contains(svgel))) {
		svgel=null; // for garbage collecting
		this.classList.remove("active");
		clearInterval(this.intervalId);
	    } else {
		gfxAutoRotate(svgel);
		gfxRecompute(svgel);
	    }
	},50);
    }
    event.stopPropagation();
}

var mouseDown=false;

// mouse handling
function gfxMouseDown(event) {
    if (!this.gfxdata) gfxInitData(this);
    if (!this.onmouseup) gfxInitMouse(this); // weak
    mouseDown=true;
    event.preventDefault();
    event.stopPropagation();
}

function gfxMouseUp(event) {
    mouseDown=false;
    event.preventDefault();
    event.stopPropagation();
}

function gfxMouseLeave(event) {
    mouseDown=false;
    event.preventDefault();
    event.stopPropagation();
}

function gfxMouseMove(event) {
    if (!mouseDown) return;

    var x=event.movementX/this.width.baseVal.value;
    var y=event.movementY/this.height.baseVal.value;

    var d=x*x+y*y;
    x*=1+d/3; y*=1+d/3; // nonlinear correction

    var mat=new Matrix([[1-x*x+y*y,2*x*y,2*x,0],[2*x*y,1+x*x-y*y,-2*y,0],[-2*x,2*y,1-x*x-y*y,0],[0,0,0,1+x*x+y*y]]);
    mat.leftmultiply(1/(1+x*x+y*y));
    gfxRotate(this,mat);

    gfxRecompute(this);

    event.preventDefault();
    event.stopPropagation();
}

function gfxMouseClick(event) {
    event.stopPropagation();
}


function gfxAutoRotate(el) {
    if (el.namespaceURI!="http://www.w3.org/2000/svg"||el.classList.contains("gfxauto")) return;
    gfxAutoRotateInt(el,el.gfxdata.dmatrix);
    for (var i=0; i<el.children.length; i++) gfxAutoRotate(el.children[i]);
}
function gfxAutoRotateInt(el,dmatrix) { // returns true if can move to next in list
    if (!dmatrix) return true;
    else if (dmatrix instanceof Matrix) { gfxRotate(el,dmatrix); return true; } // single
    else if ((dmatrix instanceof Array)&&(dmatrix.length>0)) { // list
	if (!dmatrix.index) dmatrix.index=0;
	if (gfxAutoRotateInt(el,dmatrix[dmatrix.index])) {
	    dmatrix.index++;
	    if (dmatrix.index==dmatrix.length) { dmatrix.index=0; return true; } else return false;
	} else return false;
    } else { // repeated
	if (!dmatrix.index) dmatrix.index=0;
	if (gfxAutoRotateInt(el,dmatrix.content)) {
	    dmatrix.index++;
	    if (dmatrix.index==dmatrix.number) { dmatrix.index=0; return true; } else return false;
	} else return false;
    }
}

function gfxRotate(el,mat) {
    if (el.namespaceURI!="http://www.w3.org/2000/svg"||el.classList.contains("gfxauto")) return;
    if (!el.gfxdata.matrix) el.gfxdata.matrix = new Matrix(mat); else el.gfxdata.matrix.leftmultiply(mat);
}

function gfxRecompute(el) {
    if (el.namespaceURI!="http://www.w3.org/2000/svg"||el.classList.contains("gfxauto")) return; // gadgets and non svg aren't affected by transformations
    var mat;
    if (el.gfxdata.pmatrix) mat = el.gfxdata.pmatrix; else { // if not unmoving, get the ancestors' cmatrix
	var el1=el.parentElement;
	if (!el1.gfxdata.cmatrix) return; // shouldn't happen
	mat = el1.gfxdata.cmatrix;
    }
    // cmatrix is the compound rotation matrix (just an optimization to avoid repeated multiplications)
    // at the end of the day "cmatrix" is the *ordered* product over ancestors of matrices "matrix" (plus the leftmost perspective matrix "pmatrix"
    if (!el.gfxdata.matrix) el.gfxdata.cmatrix = mat; else { el.gfxdata.cmatrix = new Matrix(el.gfxdata.matrix); el.gfxdata.cmatrix.leftmultiply(mat); }

    if ((el.tagName=="polyline")||(el.tagName=="polygon")||(el.tagName=="path")) {
	var pth,s,coords,distance;
	// parse path
	s = ""; coords=[]; distance=0; var flag=false;
	for (var j=0; j<el.gfxdata.coords.length; j++) {
	    if (el.gfxdata.coords[j] instanceof Float32Array) {
		var u=el.gfxdata.cmatrix.vectmultiply(el.gfxdata.coords[j]);
		if (u[2]/u[3]<=0) flag=true; else {
		    var v=[u[0]*u[3]/u[2],-u[1]*u[3]/u[2]];
		    coords.push(u);
		    s+=v[0]+" "+v[1]+" ";
		    distance+=u[2]/u[3];
		}
	    }
	}
	if (flag) el.style.display="none"; else {
	    el.style.display="";
	    // rewrite "d" or "points"
	    if (el.tagName=="path") el.setAttribute("d",s); else el.setAttribute("points",s);
	    // recompute distance as average of distances of vertices
	    el.gfxdata.distance=distance/coords.length;
	    if (coords.length>2) {
		var u=[],v=[];
		for (var i=0; i<3; i++) {
		    u.push(coords[1][i]/coords[1][3]-coords[0][i]/coords[0][3]);
		    v.push(coords[2][i]/coords[2][3]-coords[0][i]/coords[0][3]);
		}
		var w=[u[1]*v[2]-v[1]*u[2],u[2]*v[0]-v[2]*u[0],u[0]*v[1]-v[0]*u[1]];
		// visibility
		if (w[2]<0) {
		    if (el.gfxdata.onesided)
			el.style.visibility="hidden"; return;
		}
		else
		    w=[-w[0],-w[1],-w[2]];
		el.style.visibility="visible";
		// lighting
		var lightname = el.getAttribute("filter");
		if (lightname) {
		    lightname=lightname.substring(5,lightname.length-1); // eww. what is correct way??
		    var lightel=document.getElementById(lightname);
		    var w2=w[0]*w[0]+w[1]*w[1]+w[2]*w[2];
		    for (var j=0; j<lightel.children.length; j++)
			if (lightel.children[j].tagName == "feSpecularLighting") {
			    var lightel2=lightel.children[j].firstElementChild; // eww
			    // move the center of the light to its mirror image in the plane of the polygon
			    //var origin=document.getElementById(lightel2.gfxdata.origin);
			    var origin=lightel2.gfxdata.origin; // eval acts as getElementById
			    if (!origin.gfxdata.pcenter) gfxRecompute(origin); // hopefully won't create infinite loops
			    var light0 = new Float32Array(origin.gfxdata.pcenter); // phew
			    var light=[];
			    for (var i=0; i<3; i++)
				light.push(light0[i]/light0[3]);
			    var sp = w[0]*(light[0]-coords[0][0]/coords[0][3])+w[1]*(light[1]-coords[0][1]/coords[0][3])+w[2]*(light[2]-coords[0][2]/coords[0][3]);
			    var c = 2*sp/w2;
			    for (var i=0; i<3; i++) light[i]-=c*w[i];
			    if (sp<0) lightel.children[j].setAttribute("lighting-color","#000000"); else {
				lightel.children[j].setAttribute("lighting-color",origin.style.fill);
				lightel2.setAttribute("x",light[0]*light0[3]/light[2]);
				lightel2.setAttribute("y",-light[1]*light0[3]/light[2]);
				lightel2.setAttribute("z",4*origin.gfxdata.r/light[2]);
			    }
			}
		}
	    }
	}
    }
    else if (el.tagName=="line") {
	var u1=el.gfxdata.cmatrix.vectmultiply(el.gfxdata.point1);
	var u2=el.gfxdata.cmatrix.vectmultiply(el.gfxdata.point2);
	var sc1 = u1[3]/u1[2];
	var sc2 = u2[3]/u2[2];
	if ((sc1<=0)||(sc2<=0)) el.style.display="none"; else {
	    el.style.display="";
	    var v1=[u1[0]*sc1,-u1[1]*sc1];
	    var v2=[u2[0]*sc2,-u2[1]*sc2];
	    el.gfxdata.distance=0.5*(u1[2]/u1[3]+u2[2]/u2[3]);
	    el.setAttribute("x1",v1[0]);
	    el.setAttribute("y1",v1[1]);
	    el.setAttribute("x2",v2[0]);
	    el.setAttribute("y2",v2[1]);
	}
    }
    else if ((el.tagName=="text")||(el.tagName=="foreignObject")) {
	if (!el.gfxdata.fontsize) el.gfxdata.fontsize=14;
	var u=el.gfxdata.cmatrix.vectmultiply(el.gfxdata.point);
	var sc = u[3]/u[2];
	if (sc<=0) el.style.display="none"; else {
	    el.style.display="";
	    var v=[u[0]*sc,-u[1]*sc];
	    el.gfxdata.distance=u[2]/u[3];
	    el.setAttribute("x",v[0]);
	    el.setAttribute("y",v[1]);
	    // rescale font size
	    el.style.fontSize = el.gfxdata.fontsize*sc+"px"; // chrome doesn't mind absence of units but firefox does
	}
    }
    else if ((el.tagName=="circle")||(el.tagName=="ellipse")) {
	var u=el.gfxdata.cmatrix.vectmultiply(el.gfxdata.center);
	el.gfxdata.pcenter = u; // in case someone needs it ... (light)
	var sc=u[3]/u[2];
	if (sc<=0) el.style.display="none"; else {
	    el.style.display="";
	    var v=[u[0]*sc,-u[1]*sc];
	    el.gfxdata.distance=u[2]/u[3];
	    el.setAttribute("cx",v[0]);
	    el.setAttribute("cy",v[1]);
	    // also, rescale radius
	    if (el.tagName=="circle")
		el.setAttribute("r", el.gfxdata.r*sc);
	    else {
		el.setAttribute("rx", el.gfxdata.rx*sc);
		el.setAttribute("ry", el.gfxdata.ry*sc);
	    }
	}
    }
    else if ((el.tagName=="svg")||(el.tagName=="g")) {
	// must call inductively children's
	for (var i=0; i<el.children.length; i++) gfxRecompute(el.children[i]);
	gfxReorder(el);
	// recompute distance as average of distances of children
	el.gfxdata.distance=0; var cnt = 0;
	for (var i=0; i<el.children.length; i++)
	    if (el.children[i].gfxdata) {
		el.gfxdata.distance+=el.children[i].gfxdata.distance;
		cnt++;
	    }
	if (cnt>0) el.gfxdata.distance/=cnt;
    }
    else el.gfxdata.distance=0; // bit of a hack -- for filters...
}

function gfxReorder(el) {
    if (el.namespaceURI!="http://www.w3.org/2000/svg") return;
    if ((el.tagName=="svg")||(el.tagName=="g")) {
	// order children according to distance
	for (i=1; i<el.children.length; i++) if (el.children[i].gfxdata) {
	    var child = el.children[i];
	    j=i; while ((j>0)&&(child.gfxdata.distance>el.children[j-1].gfxdata.distance)) j--;
	    if (j<i) el.insertBefore(child,el.children[j]);
	}
    }
}

function checkData(el) {
    var mat = el.gfxdata.cmatrix.inverse();
    var sc = el.gfxdata.cmatrix.e(3,3); // not quite right but close enough TODO better
    var z = sc;
    if ((el.tagName=="polyline")||(el.tagName=="polygon")) {
	if (!el.gfxdata.coords) {
	    var pts = el.points;
	    el.gfxdata.coords = [];
	    for (var i=0; i<pts.length; i++)
		el.gfxdata.coords.push(mat.vectmultiply(vector([pts[i].x,-pts[i].y,z,sc])));
	}
    }
    else if (el.tagName=="path") {
	if (!el.gfxdata.coords) {
	    var path = el.getAttribute("d").split(" "); // for lack of better
	    el.gfxdata.coords=[];
	    for (var i=0; i<path.length; i++)
		if ( path[i] >= "A" && path[i] <= "Z" ) el.gfxdata.coords.push(path[i]);
	    else {
		el.gfxdata.coords.push(mat.vectmultiply(vector([+path[i],-path[i+1],z,sc])));
		i++;
	    }
	}
    }
    else if (el.tagName=="line") {
	if (!el.gfxdata.point1 || !el.gfxdata.point2) {
	    el.gfxdata.point1=mat.vectmultiply(vector([el.x1.baseVal.value,-el.y1.baseVal.value,z,sc]));
	    el.gfxdata.point2=mat.vectmultiply(vector([el.x2.baseVal.value,-el.y2.baseVal.value,z,sc]));
	}
    }
    else if (el.tagName=="text") {
	if (!el.gfxdata.point) {
	    el.gfxdata.point=mat.vectmultiply(vector([el.x.baseVal[0].value,-el.y.baseVal[0].value,z,sc])); // weird
	    el.gfxdata.fontsize=el.style.fontSize.substring(0,el.style.fontSize.length-2);
	}
    }
    else if (el.tagName=="foreignObject") {
	if (!el.gfxdata.point) {
	    el.gfxdata.point=mat.vectmultiply(vector([el.x.baseVal.value,-el.y.baseVal.value,z,sc]));
	    el.gfxdata.fontsize=el.style.fontSize.substring(0,el.style.fontSize.length-2);
	}
    }
    else if (el.tagName=="circle") {
	if (!el.gfxdata.center) {
	    el.gfxdata.center=mat.vectmultiply(vector([el.cx.baseVal.value,-el.cy.baseVal.value,z,sc]));
	    el.gfxdata.r=el.r.baseVal.value;
	}
    }
    else if (el.tagName=="ellipse") {
	if (!el.gfxdata.center) {
	    el.gfxdata.center=mat.vectmultiply(vector([el.cx.baseVal.value,-el.cy.baseVal.value,z,sc]));
	    el.gfxdata.rx=el.rx.baseVal.value;
	    el.gfxdata.ry=el.ry.baseVal.value;
	}
    }
}

// a simple square matrix type
var dim=4;
var matrix_identity=new Array(dim); for (var i=0; i<dim; i++) { matrix_identity[i]=new Array(dim); for (var j=0; j<dim; j++) if (i==j) matrix_identity[i][j]=1.; else matrix_identity[i][j]=0.; }

class Vector extends Float32Array {
    constructor(v) {
	if ((v instanceof Array)&&(v.length===dim))
	    super(v);
	else
	    super(dim);
    }
    add(v)
    {
	if (v instanceof Vector)
	{
	    for (var i=0; i<dim; i++)
		this[i]+=v[i];
	}
    }
}

function doubleArrayToFloat32Array(mat,fl) // used internally
{
    var i,j;
    for (i=0; i<dim; i++)
	for (j=0; j<dim; j++)
	    fl[i+dim*j]=mat[i][j]; // note the transposition to conform to openGL's silly convention
    return fl;
}

class Matrix extends Float32Array {
    constructor(mat) // simple constructor
    {
	if (typeof(mat)=='number') // a number means multiple of identity
	{
	    super(dim*dim);
	    doubleArrayToFloat32Array(matrix_identity,this);
	    this.leftmultiply(mat);
	}
	else if (mat instanceof Array && mat[0] instanceof Array)
	{
	    super(dim*dim);
	    doubleArrayToFloat32Array(mat,this);
	}
	else if (mat instanceof Float32Array || mat instanceof Array)
	{
	    super(mat);
	}
	else super(dim*dim);
    }

    // Returns element (i,j) of the matrix
    e(i,j) { return this[i+dim*j]; }

    zero()
    {
	this.fill(0);
    }

    // display
    print()
    { 
	a="{"; 
	for (var i=0; i<dim; i++)
	    {
		a+="{";
		for (var j=0; j<dim; j++)
		    {
			a+=this.e(i,j);
			if (j<dim-1) a+=",";
		    }
		a+="}";
		if (i<dim-1) a+=",";
	    }
	a+="}";
	return a;
    }

    // add another matrix or a scalar (multiple of identity)
    add(mat)
    {
	if (typeof(mat)=='number')
	    {
		for (var i=0; i<dim; i++)
		    this[i*(dim+1)]+=mat;
	    }
	else if (mat instanceof Matrix)
	    {
		for (var i=0; i<dim*dim; i++)
		    this[i]+=mat[i];
	    }
    }

    // left multiply by a matrix or scalar
    leftmultiply(mat)
    {
	if (typeof(mat)=='number')
	    {
		for (var i=0; i<dim*dim; i++)
		    this[i]*=mat;
	    }
	else if (mat instanceof Matrix)
	    {
		var temp=new Matrix(this); // it's assumed mat is *not* this
		this.zero();
		for (var i=0; i<dim; i++)
		    for (var j=0; j<dim; j++)
			for (var k=0; k<dim; k++)
			    this[i+dim*k]+=mat[i+dim*j]*temp[j+dim*k];
	    }
    }

    vectmultiply(vec)
    {
	var u=new Vector();
	for (var k=0; k<dim; k++)
	{
	    for (var l=0; l<dim; l++)
		u[k]+=this[k+dim*l]*vec[l];
	}
	return u;
    }

    inverse() // size 4 only!
    {
	var A2323 = this[10] * this[15] - this[11] * this[14], A1323 = this[9] * this[15] - this[11] * this[13], A1223 = this[9] * this[14] - this[10] * this[13], A0323 = this[8] * this[15] - this[11] * this[12], A0223 = this[8] * this[14] - this[10] * this[12], A0123 = this[8] * this[13] - this[9] * this[12], A2313 = this[6] * this[15] - this[7] * this[14], A1313 = this[5] * this[15] - this[7] * this[13], A1213 = this[5] * this[14] - this[6] * this[13], A2312 = this[6] * this[11] - this[7] * this[10], A1312 = this[5] * this[11] - this[7] * this[9], A1212 = this[5] * this[10] - this[6] * this[9], A0313 = this[4] * this[15] - this[7] * this[12], A0213 = this[4] * this[14] - this[6] * this[12], A0312 = this[4] * this[11] - this[7] * this[8], A0212 = this[4] * this[10] - this[6] * this[8], A0113 = this[4] * this[13] - this[5] * this[12], A0112 = this[4] * this[9] - this[5] * this[8];
	var det = this[0] * ( this[5] * A2323 - this[6] * A1323 + this[7] * A1223 ) - this[1] * ( this[4] * A2323 - this[6] * A0323 + this[7] * A0223 ) + this[2] * ( this[4] * A1323 - this[5] * A0323 + this[7] * A0123 ) - this[3] * ( this[4] * A1223 - this[5] * A0223 + this[6] * A0123 );
	det = 1 / det;
	return new Matrix( [ det *   ( this[5] * A2323 - this[6] * A1323 + this[7] * A1223 ), det * - ( this[1] * A2323 - this[2] * A1323 + this[3] * A1223 ), det *   ( this[1] * A2313 - this[2] * A1313 + this[3] * A1213 ), det * - ( this[1] * A2312 - this[2] * A1312 + this[3] * A1212 ), det * - ( this[4] * A2323 - this[6] * A0323 + this[7] * A0223 ), det *   ( this[0] * A2323 - this[2] * A0323 + this[3] * A0223 ), det * - ( this[0] * A2313 - this[2] * A0313 + this[3] * A0213 ), det *   ( this[0] * A2312 - this[2] * A0312 + this[3] * A0212 ), det *   ( this[4] * A1323 - this[5] * A0323 + this[7] * A0123 ), det * - ( this[0] * A1323 - this[1] * A0323 + this[3] * A0123 ), det *   ( this[0] * A1313 - this[1] * A0313 + this[3] * A0113 ), det * - ( this[0] * A1312 - this[1] * A0312 + this[3] * A0112 ), det * - ( this[4] * A1223 - this[5] * A0223 + this[6] * A0123 ), det *   ( this[0] * A1223 - this[1] * A0223 + this[2] * A0123 ), det * - ( this[0] * A1213 - this[1] * A0213 + this[2] * A0113 ), det *   ( this[0] * A1212 - this[1] * A0212 + this[2] * A0112 ) ] );
    }

    transpose()
    {
	var m=new Matrix();
	for (var i=0; i<dim; i++)
	    for (var j=0; j<dim; j++)
		m[i+dim*j]=this[j+dim*i];
	return m;
    }
};

function vector(v) { return new Vector(v); }
function matrix(m) { return new Matrix(m); }
function times(n,x) { return { number: n, content: x }; }
