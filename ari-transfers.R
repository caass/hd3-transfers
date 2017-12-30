# ----------------------
#
# Read in dirty data on antibiotic-resistant infection (ARI)
# transfers, clean it up, and plot it to Visuals
#
# ----------------------

# Import data and load libraries -------------

library(igraph)
library(RColorBrewer)

# Set working directory and read in data from CSV
setwd('~/Projects/hd3-transfers')
CASES_RAW <- read.csv('ARI Data/Raw/cases.csv')
facilities_info <- read.csv('ARI Data/Raw/ari_facilities.csv')

# Clean up dataset --------------

# If the bug status is Unkown or Pending, remove the data point because
# medical records are either unavailable or unavailable now
valid_case <- which(CASES_RAW$aristatus != 'U' & CASES_RAW$aristatus != 'P')

clean_data <- data.frame('stateid' = CASES_RAW$stateid[valid_case])

# Build vectors of relevant data
residence <- c()
dxto <- c()
collect <- c()
for (i in valid_case) {

  # Residence data - where the patient was 4 days prior to culture ----
  current_residence <- CASES_RAW$residence[i]

  # Private residences, incarcerateds, and unknowns are easy
  if (current_residence == 'PRRES') {
    residence <- append(residence, 'PRRES')
  } else if (current_residence == 'INCAR') {
    residence <- append(residence, 'INCAR')
  } else if (current_residence == 'UNK') {
    residence <- append(residence, 'UNK')
  } else if (current_residence == 'HOMEL') {
    residence <- append(residence, 'HOMEL')

    # For LTCFs and LTACHs, retrieve the appropriate ID
  } else if (current_residence == 'LTCF') {
    residence <- append(residence, as.character(CASES_RAW$ltcfid[i]))
  } else if (current_residence == 'LTACH') {
    residence <- append(residence, as.character(CASES_RAW$ltachid[i]))

    # For inpatients, their residence depends on whether they were transferred
  } else if (current_residence == 'IP') {
    if (CASES_RAW$transferred[i] != 'Y') {
      residence <- append(residence, as.character(CASES_RAW$provid[i]))
    } else {
      residence <- append(residence, as.character(CASES_RAW$hospq5id[i]))
    }

    # Don't get here
  } else {
    print('Missed case')
    residence <- append(residence, 'ERROR')
  }


  # Collection data - where the patient was when the culture was taken ----
  current_collect <- CASES_RAW$collect[i]

  # If LTACH or LTCF, use the appropriate id, otherwise take the provider id
  if (current_collect == 'LTACH') {
    collect <- append(collect, as.character(CASES_RAW$ltachcollid[i]))
  } else if (current_collect == 'LTCF') {
    collect <- append(collect, as.character(CASES_RAW$ltcfcollid[i]))
  } else if (current_collect == 'UNK') {
    collect <- append(collect, 'UNK')
  } else {
    collect <- append(collect, as.character(CASES_RAW$provid[i]))
  }

  # Discharge data - where the patient went after their stay ----
  current_discharge <- CASES_RAW$dxto[i]
  if (current_discharge == 'LTCF') {
    dxto <- append(dxto, as.character(CASES_RAW$ltcfdxid[i]))
  } else if (current_discharge == 'LTACH') {
    dxto <- append(dxto, as.character(CASES_RAW$ltachdxid[i]))
  } else if (current_discharge == 'PRRES') {
    dxto <- append(dxto, 'PRRES')
  } else if (current_discharge == 'UNK') {
    dxto <- append(dxto, 'UNK')
  } else if (current_discharge == 'OTH') {
    dxto <- append(dxto, 'INCAR')  # Special case, hardcoded
  } else if (CASES_RAW$outcome[i] == 'D') {
    dxto <- append(dxto, 'RIP') # Patient died
  } else {

    # If there's no dxto written in, make some assumptions to fill out data

    # If they stay in the same LTCF for residence & collect, put that in dxto
    # Since they probably remained in the same facility
    if (current_residence == 'LTCF' & current_collect == 'LTCF') {

      current_ltcfid <- as.character(CASES_RAW$ltcfid[i])
      current_ltcfcollid <- as.character(CASES_RAW$ltcfcollid[i])

      if (current_ltcfid == current_ltcfcollid) {
        # Hurray, they were in the same LTCF all along!
        dxto <- append(dxto, current_ltcfid)
      } else {
        # Otherwise, assume they stayed in the LTCF the culture was coll'd in
        dxto <- append(dxto, current_ltcfcollid)
      }
    } else if (current_residence == 'PRRES') {

      # If they came from their home, assume they went back home afterwards
      dxto <- append(dxto, 'PRRES')

    } else {
      dxto <- append(dxto, NA)
    }
  }

}

clean_data <- cbind(clean_data, residence, collect, dxto)

# Further cleaning (Capitalize everything and consolidate unknowns)
for (i in 2:4) {
  clean_data[,i] <- vapply(clean_data[,i], toupper, character(1),
                           USE.NAMES = FALSE)
  clean_data[which(clean_data[,i] == 'UNKNOWN'),i] <- 'UNK'
}

# At this point clean_data has only a few empty cases, and those are where a
# case was recorded with the bug, but there was no info on the patient other
# than that

# Create edges and vertices dataframes ----

# Edges dataframe
edges_df <- data.frame('from' = character(), 'to' = character(),
                       'count' = integer(), 'type' = character(),
                       stringsAsFactors = FALSE)

# Every data point has a 'collect', so take the uniques and go from there
collects <- unique(unlist(clean_data$collect))

for (i in 1:length(collects)) {

  # For every 'collect', find the links that go into and out of it
  # ins are incoming connections, admitted from elsewhere
  # outs are outgoing connections, discharged to elsewhere
  ins <- clean_data$residence[which(clean_data$collect == collects[i])]
  outs <- clean_data$dxto[which(clean_data$collect == collects[i])]

  ins <- sort(table(ins), decreasing = TRUE)
  outs <- sort(table(outs), decreasing = TRUE)

  current_froms <- c(names(ins), rep(collects[i], length(outs)))
  current_tos <- c(rep(collects[i], length(ins)), names(outs))
  current_counts <- as.integer(c(ins, outs))
  current_types <- c(rep('adm', length(ins)), rep('dx', length(outs)))

  current_transfers_df <- data.frame('from' = current_froms, 'to' = current_tos,
                                     'count' = current_counts, 'type' = current_types,
                                     stringsAsFactors = FALSE)

  edges_df <- rbind(edges_df, current_transfers_df)
}

# Nodes dataframe
raw_nodes <- unique(na.omit(unlist(clean_data[,c('residence', 'collect', 'dxto')])))

# Append "PRRES" "UNK"   "INCAR" "HOMEL" "GAMDO" "RIP" to facilities_info
new_rows <- data.frame('id' = c("PRRES", "UNK", "INCAR", "HOMEL", "GAMDO", "RIP"),
                       'name' = c('PRIVATE RESIDENCE', 'UNKNOWN', 'INCARCERATED',
                                     'HOMELESS', 'UNKNOWN MDO', 'DECEASED'),
                       'type' = rep('OTH', 6),
                       'ccn' = rep(NA, 6))

facilities_info <- rbind(facilities_info, new_rows)

nodes_df <- data.frame('id' = as.character(facilities_info$id),
                       'type' = facilities_info$type)

# Trim the edges dataframe to match the nodes dataframe
all_facilities <- unique(c(edges_df$from, edges_df$to))
not_in_nodes <- all_facilities[which(!(all_facilities %in% nodes_df$id))]
edges_df <- edges_df[-which(edges_df$from %in% not_in_nodes | edges_df$to %in% not_in_nodes),]
for (i in not_in_nodes) {
  warning('Failed to find ', i, ' in the nodes dataframe, trimming case')  # Should probably warn them
}

# Create visualization ----------------------------------------------------

# "Max Graph" includes nodes like RIP and UNK
max_graph = graph_from_data_frame(edges_df, directed = TRUE, vertices = nodes_df)
max_layout = layout_nicely(max_graph)

# Real graph eliminates GAMDO, UNK, and RIP (Not PRRES, GLTC, GA, INCAR, HOMEL, GAMD)
real_nodes <- 1:length(nodes_df$id)
real_nodes <- real_nodes[-which(nodes_df$id %in% c('GAMDO', 'UNK', 'RIP'))]
real_graph = induced_subgraph(max_graph, real_nodes, impl = 'auto')
real_layout = layout_nicely(real_graph)

# Fix up layout
xmean = mean(real_layout[,1]) * 0.1
xmax = max(real_layout[,1] + xmean)
xmin = min(real_layout[,1] - xmean)

ymean = mean(real_layout[,2]) * 0.1
ymax = max(real_layout[,2] + ymean)
ymin = min(real_layout[,2] - ymean)

# Set edge palette
edge_types <- E(real_graph)$type
edge_types[which(edge_types == 'adm')] <- 'darkorange'
edge_types[which(edge_types == 'dx')] <- 'dodgerblue1'

# Set shapes for nodes

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
pdf(file = 'Visuals/ARI Network.pdf', width = 11, height = 8.5, pointsize = 9)

plot(real_graph, layout = real_layout, rescale = FALSE, ylim = c(ymin, ymax),
     xlim = c(xmin, xmax), asp = 0, main = '2016 GA HD3 ARI Direct Transfers',

     # Edge parameters
     edge.color = edge_types,
     edge.arrow.size = .5,
     edge.width = E(real_graph)$count / 5,
     edge.curved = 0.1,

     # Vertex parameters
     vertex.shape = node_shapes,
     vertex.color = 'grey',

     # Label parameters
     vertex.label.dist = 2.7,
     vertex.label.family = 'sans',
     vertex.label.color = 'black',
     vertex.label.font = 3,
     vertex.label.cex = .65
)

legend('topleft', c('Long-Term Acute Care Hospital (LTACH)',
                    'Long-Term Care Facility (LTCF)',
                    'Medical Doctor (GAMD)',
                    'Hospital (HOSP)',
                    'Admissions', 'Discharges',
                    'Line Thickness Indicates # of Cases'),

       # Object params
       col = c( 'black', 'black', 'black', 'black', 'darkorange', 'dodgerblue1'),

       lty = c(0, 0, 0, 0, 1, 1, 0),
       lwd = c(0, 0, 0, 0, 2, 2, 0),
       seg.len = 1,

       pch = c(24, 22, 23, 21, NA, NA, NA),
       pt.bg = c('grey', 'grey', 'grey', 'grey', NA, NA, NA),
       pt.cex = c(1.5, 1.5, 1.5, 1.5, NA, NA, NA),
       pt.lwd = .75,

       # Legend params
       title = 'Legend',
       box.col = 'grey',
       bty = 'n',
       inset = -0.05
)

# Plot only admissions
adm_graph <- subgraph.edges(real_graph, which(E(real_graph)$type == 'adm'))

# Match up the layout so points don't move between graphs
adm_layout <- matrix(nrow = length(V(adm_graph)), ncol = 2)
for(i in 1:length(V(adm_graph))){

  current_pt <- names(V(adm_graph))[i]
  old_row <- which(names(V(real_graph)) == current_pt)
  adm_layout[i,] <- real_layout[old_row,]

}

plot(adm_graph, layout = adm_layout, rescale = FALSE, ylim = c(ymin, ymax),
     xlim = c(xmin, xmax), asp = 0,
     main = '2016 GA HD3 ARI Direct Transfers (Admissions Only)',

     # Edge parameters
     edge.color = 'darkorange',
     edge.arrow.size = .5,
     edge.width = E(adm_graph)$count / 5,
     edge.curved = 0.1,

     # Vertex parameters
     vertex.shape = as.character(node_shapes_opts[V(adm_graph)$type]),
     vertex.color = 'grey',

     # Label parameters
     vertex.label.dist = 2.7,
     vertex.label.family = 'sans',
     vertex.label.color = 'black',
     vertex.label.font = 3,
     vertex.label.cex = .65
)

legend('topleft', c('Long-Term Acute Care Hospital (LTACH)',
                    'Long-Term Care Facility (LTCF)',
                    'Medical Doctor (GAMD)',
                    'Hospital (HOSP)',
                    'Admissions',
                    'Line Thickness Indicates # of Cases'),

       # Object params
       col = c( 'black', 'black', 'black', 'black', 'darkorange'),

       lty = c(0, 0, 0, 0, 1, 0),
       lwd = c(0, 0, 0, 0, 2, 0),
       seg.len = 1,

       pch = c(24, 22, 23, 21, NA, NA),
       pt.bg = c('grey', 'grey', 'grey', 'grey', NA, NA),
       pt.cex = c(1.5, 1.5, 1.5, 1.5, NA, NA),
       pt.lwd = .75,

       # Legend params
       title = 'Legend',
       box.col = 'grey',
       bty = 'n',
       inset = -0.05
)


# Plot only discharges
dx_graph <- subgraph.edges(real_graph, which(E(real_graph)$type == 'dx'))

# Match up the layout so points don't move between graphs
dx_layout <- matrix(nrow = length(V(dx_graph)), ncol = 2)
for(i in 1:length(V(dx_graph))){

  current_pt <- names(V(dx_graph))[i]
  old_row <- which(names(V(real_graph)) == current_pt)
  dx_layout[i,] <- real_layout[old_row,]

}

plot(dx_graph, layout = dx_layout, rescale = FALSE, ylim = c(ymin, ymax),
     xlim = c(xmin, xmax), asp = 0,
     main = '2016 GA HD3 ARI Direct Transfers (Discharges Only)',

     # Edge parameters
     edge.color = 'dodgerblue1',
     edge.arrow.size = .5,
     edge.width = E(dx_graph)$count / 5,
     edge.curved = 0.1,

     # Vertex parameters
     vertex.shape = as.character(node_shapes_opts[V(dx_graph)$type]),
     vertex.color = 'grey',

     # Label parameters
     vertex.label.dist = 2.7,
     vertex.label.family = 'sans',
     vertex.label.color = 'black',
     vertex.label.font = 3,
     vertex.label.cex = .65
)

legend('topleft', c('Long-Term Acute Care Hospital (LTACH)',
                    'Long-Term Care Facility (LTCF)',
                    'Medical Doctor (GAMD)',
                    'Hospital (HOSP)',
                    'Discharges',
                    'Line Thickness Indicates # of Cases'),

       # Object params
       col = c( 'black', 'black', 'black', 'black', 'dodgerblue1'),

       lty = c(0, 0, 0, 0, 1, 0),
       lwd = c(0, 0, 0, 0, 2, 0),
       seg.len = 1,

       pch = c(24, 22, 23, 21, NA, NA),
       pt.bg = c('grey', 'grey', 'grey', 'grey', NA, NA),
       pt.cex = c(1.5, 1.5, 1.5, 1.5, NA, NA),
       pt.lwd = .75,

       # Legend params
       title = 'Legend',
       box.col = 'grey',
       bty = 'n',
       inset = -0.05
)


dev.off()

# Write cleaned data to CSVs (nodes and edges)
write.csv(facilities_info, 'ARI Data/Cleaned/ari_facilities.csv', row.names = FALSE)
write.csv(edges_df, 'ARI Data/Cleaned/ari_transfers.csv', row.names = FALSE)

# Export layout to make future visualization easier
export_layout <- cbind(real_layout, names(V(real_graph)))
write.csv(export_layout, 'ARI Data/Cleaned/ari_layout.csv', row.names = FALSE)
