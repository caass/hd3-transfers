# Load packages and read in data & functions ----
library(igraph)
source('Data Merging.R')

# Functions to create graphs ----

# Create base graph from merged facilities and transfers
base_graph = function(){

  # Trim nodes and edges to relevant data only
  nodes <- merge_facilities()[,c('id', 'type', 'stays', 'prevalence')]
  edges <- merge_transfers()[,c('from', 'to', 'ari', 'transfers', 'percent_ari')]

  return(graph_from_data_frame(edges, vertices = nodes))

}

# Create a subgraph where all facilities have a type, all edges have denominator data and all vertices are connected
real_graph <- function(){

  base <- base_graph()

  # Trim vertices which have no type
  g <- induced_subgraph(base, which(!(is.na(V(base)$type))))

  # Trim edges which have no transfers (and therefore percent_ari), and delete unconnected vertices
  g <- subgraph.edges(g, which(!(is.na(E(g)$transfers))), delete.vertices = TRUE)

  return(g)

}

# Create graph object ----
g <- real_graph()

# Export graph information to a spreadsheet ----
write.csv(as_adj(g, sparse = FALSE), 'Cleaned Data/Adjacency Matrices/Adjacency Matrix.csv')
write.csv(as_adj(g, sparse = FALSE, attr = 'percent_ari'), 'Cleaned Data/Adjacency Matrices/Adjacency Matrix (Weighted by ARI Percentage).csv')
write.csv(as_adj(g, sparse = FALSE, attr = 'ari'), 'Cleaned Data/Adjacency Matrices/Adjacency Matrix (Weighted by Number of ARI).csv')
write.csv(as_adj(g, sparse = FALSE, attr = 'transfers'), 'Cleaned Data/Adjacency Matrices/Adjacency Matrix (Weighted by Total Transfers).csv')
