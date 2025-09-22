import 'd3';

    $('#side').BootSideMenu({side:"right", closeOnClick: false, width: "230px"});

    $(document).ready(function(){

    // display convex hull when clicked
    $("#convexToggle").on("click", function(){
        if(curConvex) {
          $(this).html("Convex Hull: on");
          curConvex = !curConvex;
            hull.attr("opacity",1);
        } else {
          $(this).html("Convex Hull: off");
          curConvex = !curConvex;
            hull.attr("opacity",0);
        }
      });

    // show the ideal generators when clicked
    $("#idealGens").on("click", function(){
          $(this).html("Ideal Generators: "  + labels);
      });
    });
     
        dataset.sort(function(a,b) {return a[0]-b[0]});

        const svg = d3.select("body")
                        .append("svg")
                        .attr("height", h)
                        .attr("width", w)
                        .attr("id", "svgMain");

        // find largest x and y exponents
        let xMax = 0;
        let yMax = 0;
        for (let i = 0; i < dataset.length; i++) {
                if (dataset[i][0] > xMax) {
                        xMax = dataset[i][0];
                }
                if (dataset[i][1] > yMax) {
                        yMax = dataset[i][1];
                }
        }

        // make the lattice go one unit beyond max x and y values
        // looks like chart is an array of arrays of 1's
        let chart = [];
        for (let i = 0; i < yMax+1; i++) {
                chart.push([]);
                for (let j = 0; j < xMax+1; j++) {
                        chart[i].push(1);
                }
        }

        // determine the size of each unit square
        const sq = Math.ceil(Math.min(h/(yMax+2), w/(xMax+2)));

        // datum will be a coordinate in the plane and blankout will 
        // put a 0 in every lattice point above and to the right
        var blankOut = function(datum, chart) {
                for (let i = 0; i < chart.length; i++) {
                        if (i >= datum[1]) {
                                for (let j = 0; j < chart[0].length; j++) {
                                        if (j >= datum[0]) {
                                                chart[i][j] = 0;
                                        }
                                }
                        }
                }
                return chart;
        }

        // put 0 in coordinates for elements of ideal
        for (let k = 0; k < dataset.length; k++) {
                chart = blankOut(dataset[k], chart);
                }


        // the lattice points not in the ideal
        var makeDatFromChart = function(chart) {
                let dat = [];
                for (let i = 0; i < chart.length; i++) {
                        for (let j = 0; j < chart[0].length; j++) {
                                if (chart[i][j] === 1) {
                                        dat.push([i, j]);
                                }
                        }
                }
                return dat;
        }

        // the coordinates of the points not in the ideal
        const dat = makeDatFromChart(chart);


        
        // i'm not sure but i think the lines we get when switching
        // between convex hull are coming from the scale
        var xScale = d3.scale.linear();
        xScale.domain([0,xMax+1]);
        xScale.range([sq/2, (xMax+1.5)*sq]);

        var yScale = d3.scale.linear();
        yScale.domain([0, yMax+1]);
        yScale.range([h-1.5*sq, h-(yMax+2.5)*sq]);




        // list of triangles from generator to another generator to
        // the point with (x max, y max)
        let tri = [];

        for (let i = 0; i < dataset.length-1; i++) {
                for(let j = i+1; j < dataset.length; j++) {
                        tri.push(xScale(dataset[i][0]+.01).toString() + "," +
                        yScale(dataset[i][1]-1).toString() + " " +
                        xScale(dataset[j][0]+.01).toString() + "," +
                        yScale(dataset[j][1]-1).toString() + " " +
                        xScale(Math.max(dataset[i][0]+.01,
                        dataset[j][0]+.01)).toString() + "," +
                        yScale(Math.max(dataset[i][1],
                        dataset[j][1])-1).toString());
                }
        }



        console.log(tri);

        var xAxis = d3.svg.axis()
                        .scale(xScale)
                        .ticks(xMax)
                        .orient("bottom");

        var yAxis = d3.svg.axis()
                        .scale(yScale) 
                        .ticks(yMax)
                        .orient("left");

        var latticePoints= [];
        for (let i = 0; i <= yMax; i++) {
                for (let j = 1; j <= xMax+1; j++) {
                        latticePoints.push([i,j]);
                }
        }


        // shades all the squares not in the ideal
        const ideal = svg.selectAll("rect")
                        .data(dat)
                        .enter()
                        .append("rect")
                        .attr("x", function(d) { return Math.ceil(xScale(d[1])); })
                        .attr("y", function(d) { return Math.ceil(yScale(d[0])); })
                        .attr("width", sq)
                        .attr("height", sq) 
                        .attr("fill", "#f5deb3");

        // shades all the triangles. default is transparent
        const hull = svg.selectAll("polygon")
                        .data(tri)
                        .enter()
                        .append("polygon")
                        .attr("points", function(d) { return d; })
                        .attr("fill", "#FFFFFF")
                        .attr("opacity", 0);

        // shades all the lattice points
        const lattice = svg.selectAll("circle.lattice")
                        .data(latticePoints)
                        .enter()
                        .append("circle")
                        .attr("cx", function(d) { return Math.floor(xScale(d[1])); })
                        .attr("cy", function(d) { return Math.floor(yScale(d[0])); })
                        .attr("r", 4) 
                        .attr("fill", "#b3caf5");


        svg.append("g")
                .attr("class", "axis")
                .attr("transform", "translate(0," + (h-sq/2) + ")")
                .call(xAxis);

        svg.append("g")
                .attr("class", "axis")
                .attr("transform", "translate(" + sq/2 + "," + sq + ")")
                .call(yAxis);


        var makeXYstring = function(x,y) {
                let xstr, ystr, xystr;
                if (x === 0) { xstr = ""; }
                else if (x === 1) { xstr = "x"; }
                else { xstr = "x<sup>" + x.toString() + "</sup>"; }
                if (y === 0) { ystr = ""; }
                else if (y === 1) { ystr = "y"; }
                else { ystr = "y<sup>" + y.toString() + "</sup>"; }
                xystr = xstr + ystr;
                return xystr;
        }

        var labels = []
        for (let i = 0; i < dataset.length; i++) {
                labels.push(makeXYstring(dataset[i][0],dataset[i][1]));
        }


        var pointsBelow = function (point1, point2, extX, extY) {
                let first, second;
                if (point1[0] < point2[0]) {
                        first = point1;
                        second = point2;
                }
                else { first = point2; second = point1; }
                const xMin = first[0];
                const xMax = second[0];
                const yMin = 0;
                const yMax = Math.max(point1[1], point2[1]);
                let points = []
                for (let x = xMin; x <= xMax; x++) {
                        for (let y = yMin; y <= yMax; y++) {
                                const t = (x - xMin)/(xMax - xMin);
                                const l = first[1]*(1-t) + second[1]*(t);
                                if (y < l) { points.push([x,y]); }
                        }
                }
                for (let x = 0; x < xMin; x++) {
                        for (let y = 0; y <= extY; y++) {
                                points.push([x,y]);
                        }
                }
                for (let x = xMax+1; x <= extX; x++) {
                        for (let y = 0; y <= extY; y++) {
                                points.push([x,y]);
                        }
                }
                return points;
        }

        var comparePoints = function(point1, point2) {
                if (point1[0] === point2[0] && point1[1] === point2[1]) { return true; }
                else { return false; }
        }

        let extX = 0
        let extY = 0
        for (let i = 0; i < dataset.length; i++) {
                if (dataset[i][0] > extX) { extX = dataset[i][0]; }
                if (dataset[i][1] > extY) { extY = dataset[i][1]; }
        }

        let pointList = []
        for (let i = 0; i <= extX; i++) {
                for (let j = 0; j <= extY; j++) {
                        pointList.push([i,j])
                }
        }

        for (let i = 0; i < dataset.length-1; i++) {
                for (let j = i+1; j < dataset.length; j++) {
                        const pointsUnder = pointsBelow(dataset[i], dataset[j], extX, extY);
                        let newPointList = []
                        for (let a = 0; a < pointsUnder.length; a++) {
                                for (let b = 0; b < pointList.length; b++) {
                                        if (comparePoints(pointsUnder[a], pointList[b])) { newPointList.push(pointsUnder[a]); }
                                }
                        }
                        pointList = newPointList;
                }
        }

        const innerLattice = svg.selectAll("circle.inner")
                        .data(pointList)
                        .enter()
                        .append("circle")
                        .attr("cx", function(d) { return Math.floor(xScale(d[0])); })
                        .attr("cy", function(d) { return Math.floor(yScale(d[1]-1)); })
                        .attr("r", 2) 
                        .attr("fill", "#FFFFFF");
