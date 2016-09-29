function plotSequence() {
// Dimensions of sunburst, initial settings
var width = 750;
var height = 500;
var radius = Math.min(width, height) / 2;
var innerRing = document.getElementById("innerTerm").value;
var outerRing = document.getElementById("outerTerm").value;

//get the name of the file we will read
//get the name of the file to read

var subData = document.getElementById("selectDept").value;
console.log(subData);
var combo   = subData.split(".")
var subject = combo[0].split("/")

document.getElementById("subject").innerHTML = "SUBJECT = " + subject[1];
document.getElementById("major").innerHTML = "MAJOR = " + combo[1];
document.getElementById("order").innerHTML = "ORDER = " + combo[2];


// Breadcrumb dimensions: width, height, spacing, width of tip/tail.
var b = {
  w: 300, h: 20, s: 3, t: 10
};

// Total size of all segments; we set this later, after loading the data.
var totalSize = 0;

d3.select("svg").remove();
var vis = d3.select("#chart").append("svg:svg")
    .attr("width", width)
    .attr("height", height)
    .append("svg:g")
    .attr("id", "container")
    .attr("transform", "translate(" + width / 2 + "," + height / 2 + ")");

    d3.select("#chart").append("svg:text")
        .text("Subject = "+subject[1])
        .attr("y",height-0)
        .attr("x",0);
    d3.select("#chart").append("svg:text")
        .text(" Major = "+combo[1])
        .attr("y",20)
        .attr("x",0);
    d3.select("#chart").append("svg:text")
        .text(combo[2])
        .attr("y",40)
        .attr("x",0);

var partition = d3.layout.partition()
    .size([2 * Math.PI, radius * radius])
    .value(function(d) { return d.size; });

var arc = d3.svg.arc()
    .startAngle(function(d) { return d.x; })
    .endAngle(function(d) { return d.x + d.dx; })
    .innerRadius(function(d) { return Math.sqrt(d.y); })
    .outerRadius(function(d) { return Math.sqrt(d.y + d.dy); });

// Use d3.text and d3.csv.parseRows so that we do not need to have a header
// row, and can receive the csv as an array of arrays.
d3.text(subData, function(text) {
  var csv = d3.csv.parseRows(text);
  var colors = computeColors(csv);
  var json = buildHierarchy(csv,colors);
  createVisualization(json,colors);
});

// Main function to draw and set up the visualization, once we have the data.
function createVisualization(json,colors) {

  // Basic setup of page elements.
  initializeBreadcrumbTrail();
  //drawLegend();
  d3.select("#togglelegend").on("click", toggleLegend);

  // Bounding circle underneath the sunburst, to make it easier to detect
  // when the mouse leaves the parent g.
  vis.append("svg:circle")
      .attr("r", radius)
      .style("opacity", 0);

  // For efficiency, filter nodes to keep only those large enough to see.
  var nodes = partition.nodes(json)
      .filter(function(d) {
      return (d.dx > 0.005); // 0.005 radians = 0.29 degrees
      });
  var path = vis.data([json]).selectAll("path")
      .data(nodes)
      .enter().append("svg:path")
      .attr("display", function(d) { return d.depth ? null : "none"; })
      .attr("d", arc)
      .attr("fill-rule", "evenodd")
      .style("fill", function(d) { return colors[d.name]; })
      .style("opacity", 1)
      .on("mouseover", mouseover);

  // Add the mouseleave handler to the bounding circle.
  d3.select("#container").on("mouseleave", mouseleave);

  // Get total size of the tree = value of root node from partition.
  totalSize = path.node().__data__.value;
 };

// Fade all but the current sequence, and show it in the breadcrumb trail.
function mouseover(d) {

  var percentage = (100 * d.value / totalSize).toPrecision(3);
  var percentageString = percentage + "%";
  if (percentage < 0.1) {
    percentageString = "< 0.1%";
  }

  d3.select("#percentage")
      .text(percentageString);

  d3.select("#explanation")
      .style("visibility", "");

  var sequenceArray = getAncestors(d);
  updateBreadcrumbs(sequenceArray, percentageString);

  // Fade all the segments.
  d3.selectAll("path")
      .style("opacity", 0.3);

  // Then highlight only those that are an ancestor of the current segment.
  vis.selectAll("path")
      .filter(function(node) {
                return (sequenceArray.indexOf(node) >= 0);
              })
      .style("opacity", 1);
}

// Restore everything to full opacity when moving off the visualization.
function mouseleave(d) {

  // Hide the breadcrumb trail
  d3.select("#trail")
      .style("visibility", "hidden");

  // Deactivate all segments during transition.
  d3.selectAll("path").on("mouseover", null);

  // Transition each segment to full opacity and then reactivate it.
  d3.selectAll("path")
      .transition()
      .duration(1000)
      .style("opacity", 1)
      .each("end", function() {
              d3.select(this).on("mouseover", mouseover);
            });

  d3.select("#explanation")
      .style("visibility", "hidden");
}

// Given a node in a partition layout, return an array of all of its ancestor
// nodes, highest first, but excluding the root.
function getAncestors(node) {
  var path = [];
  var current = node;
  while (current.parent) {
    path.unshift(current);
    current = current.parent;
  }
  return path;
}

function initializeBreadcrumbTrail() {
  // Add the svg area.
  var trail = d3.select("#legend").append("svg:svg")
      //.attr("width", width)
      .attr("width",300)
      .attr("height",1000)
      .attr("id", "trail");
  // Add the label at the end, for the percentage.
  trail.append("svg:text")
    .attr("id", "endlabel")
    .style("fill", "#000");
}

// Generate a string that describes the points of a breadcrumb polygon.
function breadcrumbPoints(d, i) {
  var points = [];
  var stTrail = 0; //radius/2;
  points.push("0,"+"0");
  points.push(b.w+","+"0");
  points.push(b.w+","+b.h);
  points.push("0,"+b.h);
  return points.join(" ");
}

// Update the breadcrumb trail to show the current sequence and percentage.
function updateBreadcrumbs(nodeArray, percentageString) {
  // Data join; key function combines name and depth (= position in sequence).
  var g = d3.select("#trail")
      .selectAll("g")
      .data(nodeArray, function(d) { return d.name + d.depth; });

  // Add breadcrumb and label for entering nodes.
  var entering = g.enter().append("svg:g");
  entering.append("svg:polygon")
      .attr("points", breadcrumbPoints)
      .style("fill", function(d) { return colors[d.name]; });

  entering.append("svg:text")
      .attr("x", (b.w + b.t) / 2)
      .attr("y", b.h / 2)
      .attr("dy", "0.35em")
      .attr("text-anchor", "middle")
      .text(function(d) { return d.name; });
  //not sure if i like this because it doesn't update the whole table dynamically
  //entering.append("svg:text")
  //    .attr("x", b.w*2)
  //    .attr("y", b.h / 2)
  //    .attr("dy", "0.35em")
  //    .attr("text-anchor", "middle")
  //    .text(percentageString);

  // Set position for entering and updating nodes.
  g.attr("transform", function(d, i) {
    //return "translate(" + i * (b.w + b.s) + ", 0)";
      return "translate(0 ," + i * (b.h + b.s) + ")";
  });

  // Remove exiting nodes.
  g.exit().remove();

  // Now move and update the percentage at the end.
  d3.select("#trail").select("#endlabel")
      .attr("x", (b.w + b.w))
      .attr("y", (nodeArray.length-0.5)*(b.h+b.s))
      .attr("dy", "0.35em")
      .attr("text-anchor", "middle")
      .text(percentageString);

  // Make the breadcrumb trail visible, if it's hidden.
  d3.select("#trail")
      .style("visibility", "");

}

function drawLegend() {

  // Dimensions of legend item: width, height, spacing, radius of rounded rect.
  var li = {
    w: 75, h: 30, s: 3, r: 3
  };

  var legend = d3.select("#legend").append("svg:svg")
      .attr("width", li.w)
      .attr("height", d3.keys(colors).length * (li.h + li.s));

  var g = legend.selectAll("g")
      .data(d3.entries(colors))
      .enter().append("svg:g")
      .attr("transform", function(d, i) {
              return "translate(0," + i * (li.h + li.s) + ")";
           });

  g.append("svg:rect")
      .attr("rx", li.r)
      .attr("ry", li.r)
      .attr("width", li.w)
      .attr("height", li.h)
      .style("fill", function(d) { return d.value; });

  g.append("svg:text")
      .attr("x", li.w / 2)
      .attr("y", li.h / 2)
      .attr("dy", "0.35em")
      .attr("text-anchor", "middle")
      .text(function(d) { return d.key; });
}

function toggleLegend() {
  var legend = d3.select("#legend");
  if (legend.style("visibility") == "hidden") {
  legend.style("visibility", "");
  } else {
    legend.style("visibility", "hidden");
  }
}

// Take a 2-column CSV and transform it into a hierarchical structure suitable
// for a partition layout. The first column is a sequence of step names, from
// root to leaf, separated by hyphens. The second column is a count of how
// often that sequence occurred.
function buildHierarchy(csv) {
  colors=computeColors(csv);
  var root = {"name": "root", "children": []};
  for (var i = 0; i < csv.length; i++) {
    var sequence = csv[i][0];
    var size = +csv[i][1];
    if (isNaN(size)) { // e.g. if this is a header row
      continue;
    }
    var parts = sequence.split("-");
    parts = parts.slice(innerRing,outerRing);

    var currentNode = root;

    for (var j = 0; j < parts.length; j++) {

      var children = currentNode["children"];
      var nodeName = parts[j];
      var childNode;
      if (j + 1 < parts.length) {
   // Not yet at the end of the sequence; move down the tree.
 	var foundChild = false;
 	for (var k = 0; k < children.length; k++) {

 	  if (children[k]["name"] == nodeName) {
 	    childNode = children[k];
 	    foundChild = true;
 	    break;
 	  }
 	}
  // If we don't already have a child node for this branch, create it.
 	if (!foundChild) {
 	  childNode = {"name": nodeName, "children": []};
 	  children.push(childNode);
 	}
 	currentNode = childNode;
      } else {
 	// Reached the end of the sequence; create a leaf node.
 	childNode = {"name": nodeName, "size": size};
 	children.push(childNode);
      }
    }
  }
  return root;
};

//a function Ben added to get unique courses and compute color levels,
//and, as it turns out, to compute course fractions.
function computeColors(csv){

  var uCourse = [];
  var numStud = 0;

  for (var i = 0; i < csv.length; i++) {
    var sequence = csv[i][0];
    var size     = csv[i][1];
    numStud = numStud+Number(size);
    if (isNaN(size)) { // e.g. if this is a header row
      continue;
    }

    var parts = sequence.split("-");
    parts = parts.slice(innerRing,outerRing);

    for (var j = 0; j < parts.length; j++) {
      if (parts[j] != 'end'){
      uCourse.push(parts[j]);
      }
    };
  };
  var allCourse = uCourse;
  uCourse = uCourse.filter( sort_unique ).sort();
  var courseFrac = [];

  for (var j = 0; j < uCourse.length; j++) {
    var pos = allCourse.indexOf(uCourse[j]);
    var count = 0;
    while (pos !== -1) {
      count++;
      pos = allCourse.indexOf(uCourse[j], pos + 1);
    }
    courseFrac[j] = count/numStud;
  }

  //ok, write out the student count to the page while we're here.
  document.getElementById("numStud").innerHTML = "N students = " + numStud;

  //now make the color structure
  var col = [];
  var colObj = {};

  for (var i = 0; i < uCourse.length;i++)
  {
     match = uCourse[i].match(/\d+/);
     var num = parseInt(match,10);

     //this handles numbered courses
     if (num > 100 && num < 200){col = "#5687d1"}
     if (num > 200 && num < 300){col = "#7b615c"}
     if (num > 300 && num < 400){col = "#6ab975"}
     if (num < 500 && num > 400){col = "#a173d1"}
     if (num >= 500){col = '#0000ff'}
     if (uCourse[i] == 'NONE'){col = '#000000'}

     //this handles by division
     if (uCourse[i] == 'S'){col = "#5687d1"}
     if (uCourse[i] == 'SS'){col = "#7b615c"}
     if (uCourse[i] == 'H'){col = "#6ab975"}
     if (uCourse[i] == 'E'){col = "#a173d1"}
     if (uCourse[i] == 'O'){col = '#0000ff'}

     colObj[uCourse[i]] = col;
     colObj['Frac'] = courseFrac;

  };

  return(colObj);
};

//a function for sorting and finding unique courses
function sort_unique(value,index,self) {
  return self.indexOf(value) === index;
}

//and a function for computing fractions of students taking one of the unique courses.
//There is some redundancy with computeColors, should probably break that out
//into it's own function.
//function computeCourseFractions(csv,colObj)
//{
//  console.log(colObj);
//}
}
