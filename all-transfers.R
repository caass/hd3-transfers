# Imports and reading in Data --------------------------------
library(RColorBrewer)
library(igraph)

# Read in raw data
options(stringsAsFactors = FALSE)
setwd('~/Projects/hd3-transfers')
denom_transfers <- read.delim('Denominator Data/Raw/denominator_transfers.txt')
denom_facilities <- read.delim('Denominator Data/Raw/denominator_facilities.txt')
facilities_key <- read.csv('ARI Data/Cleaned/ari_facilities.csv')
ari_transfers <- read.csv('ARI Data/Cleaned/ari_transfers.csv')
ari_layout <- read.csv('ARI Data/Cleaned/ari_layout.csv')

# Create edges and nodes ----------------------------------------

# Edges
edges_df <- data.frame('from' = character(), 'to' = character(),
                       'num' = integer())
for (i in 1:length(facilities_key$ccn)) {

  current_ccn <- as.character(facilities_key$ccn[i])

  # current_transfers are transfers FROM current_ccn that have timeinterval 1
  # NOTE: If timeinterval == 365, it's previous year, not direct transfers
  current_transfers <- which(denom_transfers$numid1 == current_ccn &
                               denom_transfers$timeinterval == 1)

  temp_df <- data.frame('from' = denom_transfers[current_transfers, 'numid1'],
                        'to' = denom_transfers[current_transfers, 'numid2'],
                        'count' = denom_transfers[current_transfers, 'transfers'])

  edges_df <- rbind(edges_df, temp_df)

}

# Make tos and froms use IDs instead of CCNs to match layouts
for (i in 1:length(edges_df$from)) {

  from_ccn <- edges_df$from[i]
  to_ccn <- edges_df$to[i]

  from_id <- facilities_key$id[which(facilities_key$ccn == from_ccn)]
  to_id <- facilities_key$id[which(facilities_key$ccn == to_ccn)]

  # When you have multiple IDs for the same CCN, just use the first one
  if (length(to_id) > 1) to_id <- to_id[1]
  if (length(from_id) > 1) from_id <- from_id[1]

  # If there's no ID for the CCN, give it an NA
  if (length(to_id) < 1) to_id <- NA
  if (length(from_id) < 1) from_id <- NA

  edges_df$from[i] <- from_id
  edges_df$to[i] <- to_id

}

# Remove cases in which we didn't get a match on CCN
edges_df <- edges_df[complete.cases(edges_df),]

# Now, add in ARI data
ari_counts <- numeric()
for (i in 1:length(edges_df$from)) {

  # Find rows that match with the current transfer
  ari_rows <- which(ari_transfers$from == edges_df$from[i] & ari_transfers$to == edges_df$to[i])

  # If you found any matches
  if (length(ari_rows) != 0) {

    # Add up all the transmissions and put them in ari_counts
    num_transmissions <- sum(ari_transfers[ari_rows, 'count'])

    # If you're looking at the same facility, they have duplicate admissions and discsharges
    # So just take the larger count to have one number
    if (edges_df$from[i] == edges_df$to[i]) {

      num_transmissions <- max(ari_transfers[ari_rows, 'count'])

    }

    ari_counts[i] <- num_transmissions

  } else ari_counts[i] <- 0

}

edges_df <- cbind(edges_df, ari_counts)

# Nodes

# Pull out only the nodes that you want
unique_facilities <- unique(unlist(edges_df[,c('from', 'to')]))
nodes_row <- which(facilities_key$id %in% unique_facilities)

# Craete nodes dataframe with blank vector for stays
nodes_df <- facilities_key[nodes_row, c('id', 'type', 'ccn')]
stays <- numeric(length(nodes_df$ccn))
nodes_df <- cbind(nodes_df, stays)
for(i in 1:length(nodes_df$ccn)){
  denom_row <- which(denom_facilities$numid == nodes_df$ccn[i])
  nodes_df$stays[i] <- as.numeric(denom_facilities$stays[denom_row])
}

# Create graph and layout --------------------------

denom_graph <- graph_from_data_frame(edges_df, vertices = nodes_df)

# Layout the graph according to the original layout
denom_layout <- matrix(nrow = length(V(denom_graph)), ncol = 2)
for(i in 1:length(V(denom_graph))){
  key_row <- which(ari_layout$V3 == names(V(denom_graph))[i])
  denom_layout[i,] <- as.numeric(ari_layout[key_row, 1:2])
}

# Fix up layout
xmean = mean(denom_layout[,1]) * 0.1
xmax = max(denom_layout[,1] + xmean)
xmin = min(denom_layout[,1] - xmean)

ymean = mean(denom_layout[,2]) * 0.1
ymax = max(denom_layout[,2] + ymean)
ymin = min(denom_layout[,2] - ymean)

# Edge parameters -----------

# Which transfers have ARI?
edge_has_ari <- which(E(denom_graph)$ari_counts != 0)

# Create percentages for each transfer that contains ARI
ari_percentages <- E(denom_graph)$ari_counts[edge_has_ari] / E(denom_graph)$count[edge_has_ari]
ari_percentages <- abs(round(ari_percentages * 100))
ari_percentages[which(ari_percentages > 100)] <- 100  # Some go over b/c of -1 transfers
ari_percentages[which(ari_percentages == 0)] <- 1

# Create a palette for edges that contain ARI, plus a green for edges that don't contain any
ari_cols_func <- colorRampPalette(c('dodgerblue1', 'limegreen', 'yellow', 'orange', 'red4'))
ari_cols_pal <- ari_cols_func(100)

# Create a color vector to use for graphing
E(denom_graph)$col <- adjustcolor('grey', alpha.f = 0.1)
for(i in 1:length(edge_has_ari)){
  E(denom_graph)$col[edge_has_ari[i]] <- ari_cols_pal[ari_percentages[i]]
}

# Edge widths reflect the total volume of transfers (log scale)
E(denom_graph)$width <- log10(E(denom_graph)$count)
E(denom_graph)$width[which(is.nan(E(denom_graph)$width))] <- 1

# Edge types reflect whether or not we have >10 cases
E(denom_graph)$type <- 1
E(denom_graph)$type[which(E(denom_graph)$count < 0)] <- 2

# Set shapes for nodes ----------------------

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
add_shape("star", clip=shape_noclip,
          plot=mystar, parameters=list(vertex.norays=2))

node_shapes_opts <- c('circle', 'star', 'circle', 'triangle', 'square', 'circle')
names(node_shapes_opts) <- unique(nodes_df$type)
node_shapes <- node_shapes_opts[nodes_df$type]

# Plot ---------

# Open PDF Device and plot main graph (adm's and dx's)
pdf(file = 'Visuals/Denominator Network (ARI Overlay).pdf', width = 11, height = 8.5, pointsize = 9)

# Layout so that you can have the color-code key
layout(matrix(1:2,nrow=1),widths=c(0.1,0.9))

# X-Left, Y-Bottom, X-Right, and Y-Top
xl <- 1
yb <- 1
xr <- 2
yt <- 2

# Call plot.new, and then plot rectangles for every color
par(mar = c(5, 0, 4, 2) + 0.1)
plot(NA,type="n",ann=FALSE,xlim=c(1,2),ylim=c(1,2),xaxt="n",yaxt="n",bty="n")
rect(
  xl,
  head(seq(yb,yt,(yt-yb)/100),-1),
  xr,
  tail(seq(yb,yt,(yt-yb)/100),-1),
  col=ari_cols_func(100),
  border = NA
)

# Label
mtext(c('~100% of Transfers', 'Contained ARI', '~0% of Transfers', 'Contained ARI'),
      side = 2, at = c(2.03, 2.01, 0.99, 0.97), las = 2, adj = -0.05)

# Plot the main graph
par(mar = c(0, 0, 4, 2) + 0.1)
plot(denom_graph, layout = denom_layout, rescale = FALSE, ylim = c(ymin, ymax),
     xlim = c(xmin, xmax), asp = 0, main = 'Denominator Direct Transfers (ARI Overlay)',

     # Edge parameters
     edge.color = E(denom_graph)$col,
     edge.arrow.size = .5,
     edge.curved = 0.1,
     edge.width = E(denom_graph)$width,
     edge.lty = E(denom_graph)$type,

     # Vertex parameters
     vertex.shape = node_shapes,
     vertex.color = 'grey',

     # Label parameters
     vertex.label.dist = 3,
     vertex.label.family = 'sans',
     vertex.label.color = 'black',
     vertex.label.font = 3,
     vertex.label.cex = .55
)

legend('topleft', c('Long-Term Acute Care Hospital (LTACH)',
                    'Long-Term Care Facility (LTCF)',
                    'Hospital (HOSP)',
                    '>= 10 Total Transfers',
                    '< 10 Total Transfers',
                    'No ARI Present',
                    'Line Width Indicates # of Transfers (Log Scale)'),

       # Object params
       col = c('black', 'black', 'black', 'black', 'black', 'grey', NA),

       lty = c(0, 0, 0, 1, 2, 1, 0),
       lwd = c(0, 0, 0, 2, 2, 2, 0),
       seg.len = 2,

       pch = c(24, 22, 23, NA, NA, NA, NA),
       pt.bg = c('grey', 'grey', 'grey'),
       pt.cex = c(1.5, 1.5, 1.5),
       pt.lwd = .75,

       # Legend params
       title = 'Legend',
       box.col = 'grey',
       bty = 'n',
       inset = 0
)

dev.off()

# Export cleaned data
write.csv(denom_transfers, 'Denominator Data/Cleaned/all_transfers.csv', row.names = FALSE)
write.csv(denom_facilities, 'Denominator Data/Cleaned/all_facilities.csv', row.names = FALSE)
write.csv(edges_df, 'Denominator Data/Cleaned/overlapping_edges.csv', row.names = FALSE)
