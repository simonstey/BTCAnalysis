HTMLWidgets.widget({

  name: "forceNetwork",

  type: "output",

  initialize: function(el, width, height) {

    d3.select(el).append("svg")
        .attr("width", width)
        .attr("height", height);

    return d3.layout.force();
  },

  resize: function(el, width, height, force) {

    d3.select(el).select("svg")
        .attr("width", width)
        .attr("height", height);

    force.size([width, height]).resume();
  },

  renderValue: function(el, x, force) {

  // Compute the node radius  using the javascript math expression specified
    function nodeSize(d) {
            if(options.nodesize){
                    return eval(options.radiusCalculation);

            }else{
                    return 6}

    }


	
    // alias options
    var options = x.options;

    // convert links and nodes data frames to d3 friendly format
    var links = HTMLWidgets.dataframeToD3(x.links);
    var nodes = HTMLWidgets.dataframeToD3(x.nodes);

    // get the width and height
    var width = el.offsetWidth;
    var height = el.offsetHeight;

    var color = eval(options.colourScale);

    // set this up even if zoom = F
    var zoom = d3.behavior.zoom();

    // create d3 force layout
    force
      .nodes(d3.values(nodes))
      .links(links)
      .size([width, height])
      .linkDistance(options.linkDistance)
      .charge(options.charge)
      .on("tick", tick)
      .start();

	  //Toggle stores whether the highlighting is on
var toggle = 0;
//Create an array logging what is connected to what
var linkedByIndex = {};
for (i = 0; i < nodes.length; i++) {
    linkedByIndex[i + "," + i] = 1;
};
links.forEach(function (d) {
    linkedByIndex[d.source.index + "," + d.target.index] = 1;
});
//This function looks up whether a pair are neighbours
function neighboring(a, b) {
    return linkedByIndex[a.index + "," + b.index];
}
function connectedNodes() {
    if (toggle == 0) {
        //Reduce the opacity of all but the neighbouring nodes
        d = d3.select(this).node().__data__;
        node.style("opacity", function (o) {
            return neighboring(d, o) | neighboring(o, d) ? 1 : 0.1;
        });
        link.style("opacity", function (o) {
            return d.index==o.source.index | d.index==o.target.index ? 1 : 0.1;
        });
        //Reduce the op
        toggle = 1;
    } else {
        //Put them back to opacity=1
        node.style("opacity", 1);
        link.style("opacity", 1);
        toggle = 0;
    }
}
    // thanks http://plnkr.co/edit/cxLlvIlmo1Y6vJyPs6N9?p=preview
    //  http://stackoverflow.com/questions/22924253/adding-pan-zoom-to-d3js-force-directed
      var drag = force.drag()
        .on("dragstart", dragstart)
      // allow force drag to work with pan/zoom drag
      function dragstart(d) {
        d3.event.sourceEvent.preventDefault();
        d3.event.sourceEvent.stopPropagation();
      }

    // select the svg element and remove existing children
    var svg = d3.select(el).select("svg");
    svg.selectAll("*").remove();
    // add two g layers; the first will be zoom target if zoom = T
    //  fine to have two g layers even if zoom = F
    svg = svg
        .append("g").attr("class","zoom-layer")
        .append("g")
	


    // add zooming if requested
    if (options.zoom) {
      function redraw() {
        d3.select(el).select(".zoom-layer").attr("transform",
          "translate(" + d3.event.translate + ")"+
          " scale(" + d3.event.scale + ")");
      }
      zoom.on("zoom", redraw)

      d3.select(el).select("svg")
        .attr("pointer-events", "all")
        .call(zoom);

    } else {
      zoom.on("zoom", null);
    }

		d3.select(el).select(".zoom-layer").append("svg:marker")
     .attr("id", "end")
     .attr("viewBox", "0 -5 10 10")
     .attr("refX", 11)
     .attr("refY", 0)
     .attr("markerWidth", 5)
     .attr("markerHeight", 5)
     .attr("orient", "auto")
   .append("svg:path")
     .attr("d", "M0,-5L10,0L0,5");
	
	
	
	
    // draw links
    var link = svg.selectAll(".link")
      .data(force.links())
      .enter().append("svg:path")
      .attr("class", "link")
	  .attr("marker-end", "url("+window.location.href+"#end)")
      .style("stroke", function(d) { return d.colour ; })
      //.style("stroke", options.linkColour)
      .style("opacity", options.opacity)
      .style("fill","none")
	  //.style("stroke-width", eval("(" + options.linkWidth + ")"))
      .style("stroke-width", eval("(function(d) { return Math.min(5,Math.max(1,Math.sqrt(d.value))); })"))
      .on("mouseover", function(d) {
          d3.select(this)
            .style("opacity", 1);
      })
      .on("mouseout", function(d) {
          d3.select(this)
            .style("opacity", options.opacity);
      });

    // draw nodes
    var node = svg.selectAll(".node")
      .data(force.nodes())
      .enter().append("g")
      .attr("class", "node")
      .style("fill", function(d) { return color(d.group); })
      //.style("opacity", options.opacity)
      .on("mouseover", mouseover)
      .on("mouseout", mouseout)
      .on("click", click)
	  .on("dblclick", connectedNodes)
      .call(force.drag); 

    node.append("circle")
      .attr("r", function(d){return nodeSize(d);})
      .style("stroke", "#fff")
      //.style("opacity", options.opacity)
      .style("stroke-width", "1.5px");

    node.append("svg:text")
      .attr("class", "nodetext")
      .attr("dx", 12)
      .attr("dy", ".35em")
      .text(function(d) { return d.name })
      .style("font", options.fontSize + "px " + options.fontFamily)
      .style("opacity", options.opacityNoHover)
      .style("pointer-events", "none");
	  
        link.append("title")
            .text(function(d) { return d.source.name + " \u2192 " + d.target.name +
                "\n" + d.value + " BTC" });

    function tick() {
      node.attr("transform", function(d) {
        if(options.bounded){ // adds bounding box
            d.x = Math.max(nodeSize(d), Math.min(width - nodeSize(d), d.x));
            d.y = Math.max(nodeSize(d), Math.min(height - nodeSize(d), d.y));
        }
        
        return "translate(" + d.x + "," + d.y + ")"});
        
      link.attr("d", function(d) {
      var x1 = d.source.x,
          y1 = d.source.y;
		
         
		  
		   if ( d.source == d.target ) {
			  var x2 = d.target.x,
				y2 = d.target.y;
			   
          // Defaults for normal edge.
          var  dx = x2 - x1,
          dy = y2 - y1,
          dr = Math.sqrt(dx * dx + dy * dy),
		  drx = dr,
          dry = dr,
          xRotation = 0, // degrees
          largeArc = 0, // 1 or 0
          sweep = 1; // 1 or 0

          // Self edge.
         
            // Fiddle with this angle to get loop oriented.
            xRotation = -45;

            // Needs to be 1.
            largeArc = 1;

            // Change sweep to change orientation of loop. 
            //sweep = 0;

            // Make drx and dry different to get an ellipse
            // instead of a circle.
            drx = 30;
            dry = 20;
            
            // For whatever reason the arc collapses to a point if the beginning
            // and ending points of the arc are the same, so kludge it.
            x2 = x2 + 1;
            y2 = y2 + 1;
         

     return "M" + x1 + "," + y1 + "A" + drx + "," + dry + " " + xRotation + "," + largeArc + "," + sweep + " " + x2 + "," + y2;
	   } else {
		   var length = Math.sqrt(Math.pow(d.target.y - d.source.y, 2) + Math.pow(d.target.x - d.source.x, 2));
		var scale = (length - nodeSize(d.target)) / length;
		var offsetX = (d.target.x - d.source.x) - (d.target.x - d.source.x) * scale;  
		var offsetY = (d.target.y - d.source.y) - (d.target.y - d.source.y) * scale;
       var x2 = d.target.x - offsetX,
          y2 = d.target.y - offsetY;
	   return "M" + x1 + "," + y1 + "A" + 0 + "," + 0 + " 0 0,1 " + x2 + "," + y2;
	   }
	   
    });
    }
  
    function mouseover() {
      d3.select(this).select("circle").transition()
        .duration(750)
        .attr("r", function(d){return nodeSize(d)+5;});
      d3.select(this).select("text").transition()
        .duration(750)
        .attr("x", 13)
        .style("stroke-width", ".5px")
        .style("font", options.clickTextSize + "px ")
        .style("opacity", 1);
    }

    function mouseout() {
      d3.select(this).select("circle").transition()
        .duration(750)
        .attr("r", function(d){return nodeSize(d);});
      d3.select(this).select("text").transition()
        .duration(1250)
        .attr("x", 0)
        .style("font", options.fontSize + "px ") 
        .style("opacity", options.opacityNoHover);
    }
    
    function click(d) {
      return eval(options.clickAction)
    }

    // add legend option
    if(options.legend){
        var legendRectSize = 18;
        var legendSpacing = 4;
        var legend = svg.selectAll('.legend')
          .data(color.domain())
          .enter()
          .append('g')
          .attr('class', 'legend')
          .attr('transform', function(d, i) {
            var height = legendRectSize + legendSpacing;
            var offset =  height * color.domain().length / 2;
            var horz = legendRectSize;
            var vert = i * height+4;
            return 'translate(' + horz + ',' + vert + ')';
          });

        legend.append('rect')
          .attr('width', legendRectSize)
          .attr('height', legendRectSize)
          .style('fill', color)
          .style('stroke', color);

        legend.append('text')
          .attr('x', legendRectSize + legendSpacing)
          .attr('y', legendRectSize - legendSpacing)
          .text(function(d) { return d; });
    }
    
    // make font-family consistent across all elements
    d3.select(el).selectAll('text').style('font-family', options.fontFamily);
  },
});
