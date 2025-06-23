
# Step 1: Rebuild the list of all unique node names
nodes <- data.frame(name = unique(c(links_all$source, links_all$target)))

# Step 2: Count inflows into each node from the links
inflows <- links_all %>%
  group_by(target_id) %>%
  summarise(count = sum(value), .groups = "drop")

# Step 3: Attach counts to node labels
# First, initialize labels as plain names
nodes$label <- nodes$name

# Now update the label only where there is an inflow
nodes$label[inflows$target_id + 1] <- paste0(nodes$name[inflows$target_id + 1],
                                             " (", inflows$count, ")")
library(htmlwidgets)

p <- onRender(p, '
  function(el) {
    // Move all node labels inside by default
    d3.select(el).selectAll(".node text")
      .attr("x", 10)
      .attr("text-anchor", "start");

    // Rotate "Suspected Lymphoma" vertically
    d3.select(el).selectAll(".node text")
      .filter(function(d) { return d.name.startsWith("Suspected Lymphoma"); })
      .attr("transform", function(d) {
        var x = 10;
        var y = d.dy / 2;
        return "rotate(-90," + x + "," + y + ")";
      })
      .attr("x", 10)
      .attr("y", function(d) { return d.dy / 2; })
      .attr("text-anchor", "middle");

    // Adjust "Non-BL diagnosed" label inside the **third column**
    d3.select(el).selectAll(".node text")
      .filter(function(d) { return d.name.startsWith("Non-BL diagnosed") && d.depth === 2; })
      .attr("x", -10)
      .attr("text-anchor", "end");

    // Adjust "No diagnosis" label inside the **third column**
    d3.select(el).selectAll(".node text")
      .filter(function(d) { return d.name.startsWith("No diagnosis") && d.depth === 2; })
      .attr("x", -10)
      .attr("text-anchor", "end");
  }
')

p <- onRender(p, '
  function(el) {
    const svg = d3.select(el).select("svg");

    // Add custom labels for each column
    svg.append("text")
      .attr("x", 0)
      .attr("y", 15)
      .text("Patients")
      .style("font-size", "16px")
      .style("font-weight", "bold");

    svg.append("text")
      .attr("x", 300)
      .attr("y", 15)
      .text("Test availability at 1st MDT")
      .style("font-size", "16px")
      .style("font-weight", "bold");

    svg.append("text")
      .attr("x", 600)
      .attr("y", 15)
      .text("Diagnosis")
      .style("font-size", "16px")
      .style("font-weight", "bold");
  }
')
p

