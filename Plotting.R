# Load libraries and graph data ----
library(igraph)
library(RColorBrewer)
source('Graph Generation.R')

# Plot setup ----

# Visually cluster facilities according to the transfers between them
layout_by_denominator <- function(seed = 85, layout_csv = NA){

  # If there's a file you want to load for the layout, load it
  if (!(is.na(layout_csv))) {
    weighted_layout <- as.matrix(read.csv(layout_csv, header = FALSE))
    dimnames(weighted_layout) <- NULL
    return(weighted_layout)
  }

  # Create a communities object weighted according to the denominator transfers
  # NOTE: Depending on your computing power and number of points, this may take a while.
  # Or forever. It worked for me after a couple minutes
  c <- cluster_optimal(g, weights = abs(E(g)$transfers))

  # Create a layout based on communities
  edge_weights <- ifelse(crossing(c, g), 1, 70)

  # Set the randomness seed to create a reproducible layout
  set.seed(seed)
  weighted_layout <- layout_with_fr(g, weights = edge_weights, coords = layout_with_kk(g, weights = edge_weights))

  # Find outlying points
  x_extremes <- boxplot.stats(weighted_layout[,1])$stats[c(1,5)]
  y_extremes <- boxplot.stats(weighted_layout[,2])$stats[c(1,5)]
  x_outliers <- which(weighted_layout[,1] %in% boxplot.stats(weighted_layout[,1])$out)
  y_outliers <- which(weighted_layout[,2] %in% boxplot.stats(weighted_layout[,2])$out)

  # Bring outlying points closer and give them a random position at their appropriate extreme
  # NOTE: May be misleading, these can look like clustered points even if they're not
  for (x in x_outliers) {
    if (weighted_layout[x,1] < x_extremes[1]) {
      weighted_layout[x,1] <- x_extremes[1] - runif(1, max = 0.5)
    } else {
      weighted_layout[x,1] <- x_extremes[2] + runif(1, max = 0.5)
    }
  }
  for (y in y_outliers) {
    if (weighted_layout[y,2] < y_extremes[1]) {
      weighted_layout[y,2] <- y_extremes[1] - runif(1, max = 0.5)
    } else {
      weighted_layout[y,2] <- y_extremes[2] + runif(1, max = 0.5)
    }
  }

  return(weighted_layout)

}

# Set shapes for vertices according to facility type
generate_node_shapes <- function(){

  node_shapes_opts <- c('circle', 'triangle', 'square', 'diamond')
  names(node_shapes_opts) <- unique(V(g)$type)
  return(node_shapes_opts[V(g)$type])

}

# Plotting ----
plot_network <- function(label_clusters = FALSE, polygon_clusters = FALSE, node_sizes = c('uniform', 'stays'),
                            node_colors = c('uniform', 'cluster', 'cases', 'prevalence'),
                            edges_to_plot = c('suppress', 'ari', 'all'),
                            edge_colors = c('denominator', 'ari', 'percent_ari'),
                            edge_widths = c('uniform', 'transfers', 'ari', 'percent_ari'),
                            highlight_facility = FALSE){

  # Set node sizes according to node_sizes ----
  node_sizes <- match.arg(node_sizes)

  if (node_sizes == 'stays'){

    # Logs of the absolute value capture different orders of magnitude, and also make censored (negative) values
    # disappear
    V(g)$size <- log(abs(V(g)$stays), 5)
  } else {
    V(g)$size <- 2.5
  }

  # Set node coloration according to node_colors ----
  node_colors <- match.arg(node_colors)

  if (node_colors == 'cluster') {

    # Color the nodes according to their cluster

    # https://stackoverflow.com/questions/15282580/how-to-generate-a-number-of-most-distinctive-colors-in-r
    qual_col_pals <- brewer.pal.info[brewer.pal.info$category == 'qual',]
    col_vector <- unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
    V(g)$color <- col_vector[c$membership]

  } else if (node_colors == 'cases') {

    # Color the nodes according to the number of cases they have
    col_vector <- rev(heat.colors(max(V(g)$cases) + 1))
    V(g)$color <- col_vector[V(g)$cases + 1]

  } else if (node_colors == 'prevalence') {

    # The percentage is 100 * prevalence, so take the log of that to smooth it out
    logged <- log10(V(g)$prevalence * 100)

    # Find the coloration thresholds according to boxplot stats, without infinite
    thresholds <- boxplot.stats(logged[-which(is.infinite(logged))])$stats

    # Set a palette and pick colors according to where the node is in the boxplot
    heat_vector <- rev(heat.colors(6))
    V(g)$color <- vapply(logged, function(x){
      if (is.infinite(x)) {
        return('#FFFFFF')  # Prevalence of 0
      } else if (x <= thresholds[1]) {
        return(heat_vector[1])
      } else if (x <= thresholds[2]) {
        return(heat_vector[2])
      } else if (x <= thresholds[3]) {
        return(heat_vector[3])
      } else if (x <= thresholds[4]) {
        return(heat_vector[4])
      } else if (x <= thresholds[5]) {
        return(heat_vector[5])
      } else {
        return(heat_vector[6])
      }
    }, character(1), USE.NAMES = FALSE)
  } else if (node_colors == 'uniform') {
    V(g)$color <- 'white'
  }

  # Highlight bridging facility according to highlight_facility ----
  # Note: Because the bridging facility is identified by it's ID, it's manually added in every time
  if (highlight_facility) {
    V(g)$color[which(V(g)$name == facility_to_highlight)] <- 'blue'
  }

  # Set edge types according to edges_to_plot ----
  edges_to_plot = match.arg(edges_to_plot)

  # If you wanna plot all edges, assign ltys according to number of trasnfers
  if (edges_to_plot == 'all') {
    E(g)$lty <- vapply(E(g)$transfers, function(x){

      # Suppress plotting of edges with less then 10 transfers
      if (x < 35) {
        return(0)
      }
      # Dashed lines for less than 50 transfers
      else if (x < 100) {
        return(5)
      }
      # Solid lines for more than 50 transfers
      else {
        return(1)
      }
    }, double(1), USE.NAMES = FALSE)
  } else if (edges_to_plot == 'ari') {

    # Same as above, with an additional round of checking edges for ari
    E(g)$lty <- vapply(E(g)$transfers, function(x){

      # Suppress plotting of edges with less then 10 transfers
      if (x < 35) {
        return(0)
      }
      # Dashed lines for less than 50 transfers
      else if (x < 100) {
        return(5)
      }
      # Solid lines for more than 50 transfers
      else {
        return(1)
      }
    }, double(1), USE.NAMES = FALSE)

    # Find edges with less than 35 transfers, but which have ari, and make those dotted
    E(g)$lty[which(E(g)$ari != 0 & E(g)$transfers < 35)] <- 3

  } else if (edges_to_plot == 'suppress') {
    # Suppress plotting
    E(g)$lty <- 0
  }

  # Set edge colors according to edge_colors ----
  edge_colors <- match.arg(edge_colors)

  if (edge_colors == 'denominator') {

    E(g)$color <- 'gray'

  } else if (edge_colors == 'ari') {

    # Create a palette
    edge_pal <- c('lightgreen', 'dodgerblue', 'orangered', 'black')

    # Create a heat colors vector with a grey for edges with 0 cases
    E(g)$color <- vapply(E(g)$ari, function(x){

      # Edges with no ARI are greyed out
      if (x == 0) {
        return(adjustcolor('grey', 0.4))
      }

      # Edges with one case get green
      else if (x == 1){
        return(edge_pal[1])
      }

      # Two cases gets blue
      else if (x == 2){
        return(edge_pal[2])
      }

      # Three cases
      else if (x == 3){
        return(edge_pal[3])
      }

      # Four cases
      else if (x==4){
        return(edge_pal[4])
      }

    }, character(1), USE.NAMES = FALSE)

  } else if (edge_colors == 'percent_ari') {

    # Set edge colors according to categorical prevalence rates

    # Create a palette
    edge_pal <- c('lightgreen', 'dodgerblue', 'red4')

    E(g)$color <- vapply(E(g)$percent_ari, function(x){

      # Edges with no ARI are greyed out
      if (x == 0) {
        return(adjustcolor('grey', .4))
      }
      # Edges with <2%
      if (x <= .02) {
        return (edge_pal[1])
      # <5%
      } else if (x <= .05) {
        return(edge_pal[2])
        # >10%
      } else {
        return(edge_pal[3])
      }
    }, character(1), USE.NAMES = FALSE)
  }

  # Set edge_widths according to edge_widths ----
  edge_widths <- match.arg(edge_widths)
  E(g)$curve <- 0
  E(g)$arrows <- 0

  if (edge_widths == 'uniform') {
    E(g)$widths <- 1
  } else if (edge_widths == 'transfers') {
    E(g)$widths <- vapply(E(g)$transfers, function(x){

      if (x <= 0) {
        return(0.5)
      } else {
        return(log10(x))
      }
    }, double(1), USE.NAMES = FALSE)
  } else if (edge_widths == 'ari') {
    E(g)$widths <- vapply(E(g)$ari, function(x){

      if (x == 0) {
        return(1)
      } else {
        return(3)
      }
    }, double(1), USE.NAMES = FALSE)

    E(g)$arrows <- vapply(E(g)$percent_ari, function(x){
      if(x != 0){
        return(2)
      } else {
        return(0)
      }
    }, double(1), USE.NAMES = FALSE)

    E(g)$curve[which(E(g)$ari != 0)] <- .1
  } else if (edge_widths == 'percent_ari') {
    E(g)$widths <- vapply(E(g)$percent_ari, function(x){
      if (x == 0 ){
        return(1)
      } else {
        return(3)
      }
    }, double(1), USE.NAMES = FALSE)

    E(g)$arrows <- vapply(E(g)$percent_ari, function(x){
      if(x != 0){
        return(2)
      } else {
        return(0)
      }
    }, double(1), USE.NAMES = FALSE)

    E(g)$curve[which(E(g)$ari != 0)] <- .1
  }

  # Final plot setup and output ----
  # Set the plot's limits to be slightly bigger than the plot itself
  x_max <- max(l[,1]) + abs(mean(l[,1])) * 0.001
  x_min <- min(l[,1]) - abs(mean(l[,1])) * 0.001
  y_max <- max(l[,2]) + abs(mean(l[,2])) * 0.001
  y_min <- min(l[,2]) - abs(mean(l[,2])) * 0.001

  plot(g, layout = l, xlim = c(x_min, x_max), ylim = c(y_min, y_max), rescale = FALSE,

       # Vertex parameters
       vertex.size = V(g)$size,
       vertex.color = V(g)$color,
       vertex.shape = V(g)$shape,
       # TODO: vertex.frame.width = 7,

       # Vertex label parameters
       vertex.label = NA,

       # Edge parameters
       edge.lty = E(g)$lty,
       edge.col = E(g)$color,
       edge.width = E(g)$widths,
       edge.curved = E(g)$curve,

       # Arrow parameters
       edge.arrow.size = .3,
       edge.arrow.mode = E(g)$arrows)

  # Put some text down in the center of every group with the group label if label_clusters ----
  if(label_clusters){
    for (community in 1:length(c)) {

      # Find the locations of points of community members
      locs <- matrix(l[which(membership(c) == community),], ncol = 2)

      # Find the x and y means
      x_mean <- mean(locs[,1])
      y_mean <- mean(locs[,2])

      # Put some text there
      text(x_mean, y = y_mean, labels = community)
    }
  }

  # Polygon behind clusters if you want ----
  # If you wanna draw polygons behind the clusters, do that
  if (polygon_clusters) {

    # For each cluster
    for (group in unique(c$membership)) {

      if (length(which(c$membership == group)) != 1) {

        # Find which points are in that group
        p <- l[which(c$membership == group),]

        # Find the convex hull of those points
        p <- p[chull(p),]

        # Polygon those
        polygon(p, density = NA, border = NA, col = adjustcolor('grey', alpha.f = .3))

      }

    }
  }
}

# Run this before plotting to ensure shapes actually happen ----

# Ripped from the docs, don't ask me how this works
mytriangle <- function(coords, v=NULL, params) {
  vertex.color <- params("vertex", "color")
  if (length(vertex.color) != 1 && !is.null(v)) {
    vertex.color <- vertex.color[v]
  }
  vertex.size <- 1/125 * params("vertex", "size")
  if (length(vertex.size) != 1 && !is.null(v)) {
    vertex.size <- vertex.size[v]
  }

  symbols(x=coords[,1], y=coords[,2], bg=vertex.color,
          stars=cbind(vertex.size, vertex.size, vertex.size),
          add=TRUE, inches=FALSE)
}
# clips as a circle
add_shape("triangle", clip=shapes("circle")$clip,
          plot=mytriangle)

# generic star vertex shape, with a parameter for number of rays
mystar <- function(coords, v=NULL, params) {
  vertex.color <- params("vertex", "color")
  if (length(vertex.color) != 1 && !is.null(v)) {
    vertex.color <- vertex.color[v]
  }
  vertex.size  <- 1/150 * params("vertex", "size")
  if (length(vertex.size) != 1 && !is.null(v)) {
    vertex.size <- vertex.size[v]
  }
  norays <- params("vertex", "norays")
  if (length(norays) != 1 && !is.null(v)) {
    norays <- norays[v]
  }

  mapply(coords[,1], coords[,2], vertex.color, vertex.size, norays,
         FUN=function(x, y, bg, size, nor) {
           symbols(x=x, y=y, bg=bg,
                   stars=matrix(c(size,size), nrow=1, ncol=nor*2),
                   add=TRUE, inches=FALSE)
         })
}
# no clipping, edges will be below the vertices anyway
add_shape("diamond", clip=shape_noclip,
          plot=mystar, parameters=list(vertex.norays=2))

# Generate globals for layout and clustering ----
l <- layout_by_denominator(layout_csv = 'Layout.csv')
set.seed(1)  # Consistent clustering
c <- cluster_optimal(g, weights = abs(E(g)$transfers))

# Set node shapes, too
V(g)$shape <- generate_node_shapes()


