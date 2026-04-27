if (this.anim) return;
e=event.target;
while (e.tagName!='use') { e=e.parentElement; if (e===null) return; }
xx=e.x.baseVal.value; yy=e.y.baseVal.value;
k=(xx+yy)%2; i=(yy-xx-k)/2; j=(yy+xx+k)/2;
// just shift so always same set up
if (+e.dataset.typ==0) j=j+1; else if (+e.dataset.typ==1) i=i+1;
n=this.children[1].childElementCount-1;
if (i<1||j<1||i>=n||j>=n) return;
nodes=[
    this.children[0].children[i].children[j-1], // pointing/moving SE
    this.children[1].children[j].children[i-1], // SW
    this.children[0].children[i].children[j], // NW
    this.children[1].children[j].children[i] // NE
];
// check it's a valid click
for (l=0; l<4; l++) if (nodes[l].dataset.typ!=l) return;
this.anim='true';
shifts=[[1,1],[-1,1],[-1,-1],[1,-1]];
// first, reorder them
// we made sure nodes[0] and nodes[2] (resp 1, 3) are neighbors
nodes[0].before(nodes[2]);
nodes[1].before(nodes[3]);
window.setTimeout(() => { // need small pause for transition to work correctly after nodes moved
    // update colors
    c=nodes.map(u=>+u.dataset.col)
    mis=[0,1,2,3].filter(x=>c.indexOf(x)<0);
    if (c[0]==c[2]) nodes[0].dataset.col=nodes[2].dataset.col = c[0]<c[1] ? mis.shift() : mis.pop();
    if (c[1]==c[3]) nodes[1].dataset.col=nodes[3].dataset.col = mis[0];
    // animate squares
    for (l=0; l<4; l++) {
        nodes[l].x.baseVal.value += shifts[l][0];
        nodes[l].y.baseVal.value += shifts[l][1];
    }
    this.anim='';
},0);
