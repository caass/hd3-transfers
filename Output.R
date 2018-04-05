library(igraph)
library(RColorBrewer)
source('Plotting.R')

# Arguments:
# label_clusters = c(FALSE, TRUE)  -- label clusters by their group number
# node_sizes = c('uniform', 'stays') -- size nodes uniformly or according to the log of their stays
# node_colors = c('cluster', 'cases', 'prevalence') -- color nodes according to clustering, cases, or prevalence
# edges_to_plot = c('suppress', 'ari', 'all') -- don't plot edges, only plot ari, or plot all
# edge_colors = c('suppress', 'denominator', 'ari', 'percent_ari') -- color edges according to edge params
# edge_widths = c('uniform', 'transfers', 'ari') -- Edge widths uniform or proportional to log(transfers or ari)
# highlight_facility = c(FALSE, TRUE) -- Highlight a bridging facility?

plot_network(label_clusters = FALSE, node_sizes = 'stays', node_colors = 'cases', edges_to_plot = 'all',
             edge_colors = 'percent_ari', edge_widths = 'transfers', highlight_facility = FALSE)
