library(igraph)
library(RColorBrewer)
source('Plotting.R')

# Arguments:
# label_clusters = c(FALSE, TRUE)  -- label clusters by their group number
# polygon_clusters = c(FALSE, TRUE) -- draw a polygon behind each cluster
# node_sizes = c('uniform', 'stays') -- size nodes uniformly or according to the log of their stays
# node_colors = c('uniform', 'cluster', 'cases', 'prevalence') -- color nodes according to clustering, cases, or prevalence
# edges_to_plot = c('suppress', 'ari', 'all') -- don't plot edges, plot cms + ari, or just cms (misleading, i know)
# edge_colors = c('suppress', 'denominator', 'ari', 'percent_ari') -- color edges according to edge params
# edge_widths = c('uniform', 'transfers', 'ari', 'percent_ari') -- Edge widths uniform or proportional to log(transfers or ari), or percent_ari
# highlight_facility = c(FALSE, TRUE) -- Highlight a bridging facility?

# Finalize plotting setup
par(mar = c(0, 0, 0, 0))
g_backup <- g
g <- simplify(g, remove.multiple = FALSE)

# Slide 1
plot_network(polygon_clusters = TRUE, node_sizes = 'stays', node_colors = 'uniform', edges_to_plot = 'all',
             edge_colors = 'denominator', edge_widths = 'uniform')

# Slide 2
plot_network(node_sizes = 'stays', node_colors = 'cases', edges_to_plot = 'all',
             edge_colors = 'denominator', edge_widths = 'uniform')

# Slide 3
plot_network(node_sizes = 'stays', node_colors = 'cases', edges_to_plot = 'ari',
             edge_colors = 'ari', edge_widths = 'ari')
