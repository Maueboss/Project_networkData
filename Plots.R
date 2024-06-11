# PLOTS

### STARTING POINT ###
# Suggested colors for edge categories and node gradient
edge_colors <- c("1" = "#377eb8", "2" = "#e41a1c", "3-4" = "#4daf4a", ">4" = "#984ea3")
node_gradient <- c("#f7fbff", "#08306b")  # From light blue to dark blue
edge_colors_general <- c("F" = "#377eb8", "S" = "#e41a1c", "T" = "#4daf4a", "O" = "#984ea3")
# Plot the network

ggraph(net_overall, layout = "stress", bbox = 15) +
  geom_edge_link2(aes(edge_colour = sightings), edge_linewidth = 0.5) +
  scale_edge_colour_manual(values = edge_colors) + 
  geom_node_point(aes(fill = sightings_per_dolphin, size = sightings_per_dolphin), shape=21) +
  scale_fill_gradient(low = node_gradient[1], high = node_gradient[2]) + 
  geom_node_text(aes(label = name), vjust = 0.5, hjust = 0.5,
                 family = "serif", size = 3) +  # Adjust font size as needed
  scale_size(range = c(4, 10), guide = "none") +
  theme_graph() +
  theme(legend.position = "bottom")

ggraph(net_dolphins_label, layout = "stress", bbox = 15) +
  geom_edge_link2(aes(edge_colour = type), edge_linewidth = 0.5) +
  scale_edge_colour_manual(values = edge_colors_general) + 
  geom_node_point(aes(fill = sightings_per_dolphin, size = sightings_per_dolphin), shape=21) +
  scale_fill_gradient(low = node_gradient[1], high = node_gradient[2]) + 
  geom_node_text(aes(label = name), vjust = 0.5, hjust = 0.5,
                 family = "serif", size = 3) +  # Adjust font size as needed
  scale_size(range = c(4, 10), guide = "none") +
  theme_graph() +
  theme(legend.position = "bottom")
############################################################################################################################################################################
### LINE GRAPH ###
# Create the line graph
prova= dolphins_label[, c(1,2,4)]
net_prova = graph_from_data_frame(dolphins_label, directed = FALSE)
line_graph <- make_line_graph(net_prova)
str(line_graph)


# Plot the line graph using ggraph
ggraph(line_graph, layout = "stress") +
  geom_edge_link(aes(edge_alpha = 0.8), edge_colour = "blue") +
  geom_node_point(size = 5, color = "red") + 
  theme_void() +
  theme(legend.position = "none")




############################################################################################################################################################################
### Nicer plots ###




############################################################################################################################################################################

######## CLUSTERING ########
#### NODE CLUSTERING ####

comps <- components(net_dolphins_label_F)
net_dolphins_label_F_restricted <- delete_vertices(net_dolphins_label_F, which(comps$membership == which.min(comps$csize)))

comps <- components(net_dolphins_label_F_restricted)
net_dolphins_label_F_restricted <- delete_vertices(net_dolphins_label_F_restricted, which(comps$membership == which.min(comps$csize)))

comps <- components(net_dolphins_label_F_restricted)
net_dolphins_label_F_restricted <- delete_vertices(net_dolphins_label_F_restricted, which(comps$membership == which.min(comps$csize)))


bb <- layout_as_backbone(line_graph, keep = 0.4)
E(net_dolphins_label_F_restricted)$col <- F

E(net_dolphins_label_F_restricted)$col[bb$backbone] <- T

ggraph(net_dolphins_label_F_restricted,
       layout = "manual",
       x = bb$xy[, 1],
       y = bb$xy[, 2]) +
  geom_edge_link0(aes(filter = !col, col = col), width = 0.2) +
  geom_node_voronoi(
    aes(x, y, fill = sightings_per_dolphin),
    max.radius = 0.4,
    expand = unit(-0.5, 'mm'),
    colour = 'black'
  ) +
  scale_color_brewer(palette = "Set1") +
  #scale_fill_brewer(palette = "Set1") +
  scale_edge_color_manual(values = c(rgb(0, 0, 0, 0.3), rgb(0, 0, 0, 1))) +
  theme(
    legend.position = "none",
    panel.grid = element_blank(),
    axis.ticks = element_blank(),
    axis.text = element_blank()
  ) +
  theme_graph() +
  theme(legend.position = "none")

ggraph(net_dolphins_label_O_restricted,
       layout = "manual",
       x = bb$xy[, 1],
       y = bb$xy[, 2]) +
  geom_edge_link0(aes(col = col), width = 0.2) +
  geom_node_point(aes(fill = sightings_per_dolphin), shape = 21, size = 3) +
  geom_mark_hull(
    aes(x, y, group = sightings_per_dolphin, fill = sightings_per_dolphin),
    concavity = 4,
    expand = unit(2, "mm"),
    alpha = 0.25
  ) +
  scale_color_brewer(palette = "Set1") +
  #scale_fill_brewer(palette = "Set1") +
  scale_edge_color_manual(values = c(rgb(0, 0, 0, 0.3), rgb(0, 0, 0, 1))) +
  theme_graph()+
  theme(legend.position = "none")

###### EDGE CLUSTERING ######

num_edges <- ecount(net_dolphins_label_F)
print(paste("Number of edges:", num_edges))


net_dolphins_label_F_cluster <- cluster_edge_betweenness(net_dolphins_label_F)
# Get the membership of each edge
plot(net_dolphins_label_F_cluster, net_dolphins_label_F, layout=layout_with_fr(net_dolphins_label_F))

edge_membership <- membership(net_dolphins_label_F_cluster)
E(net_dolphins_label_F)$cluster <- edge_membership
sum(edge_membership)
ggraph(net_dolphins_label_F, layout = "fr") +
  geom_edge_link(aes(colour = factor(edge_membership)), edge_width = 0.5) +
  geom_node_point(size = 5) +
  scale_edge_colour_manual(values = rainbow(length(unique(edge_membership)))) +
  theme_graph() +
  theme(legend.position = "bottom")



# Plot the graph with ggraph
ggraph(net_dolphins_label_F, layout = "stress") +
  geom_edge_link(aes(colour = factor(cluster)), width = 0.5) +
  geom_node_point(size = 5) +
  scale_edge_colour_manual(values = rainbow(length(unique(edge_membership)))) +
  theme_graph() +
  theme(legend.position = "bottom")


## SHIT METHOD ##
forage_dolphins$type <- "F"
social_dolphins$type <- "S"
travel_dolphins$type <- "T"
dolphins_label_prova <- rbind(forage_dolphins, social_dolphins, travel_dolphins)
dolphins_label_prova$type <- as.factor(dolphins_label_prova$type)
net_prova <- graph_from_data_frame(dolphins_label_prova, directed = FALSE) #from the first 250 rows of the combined data frame with labeled interaction types

edge_clusters <- cluster_spinglass(net_prova, weights = E(net_prova)$type)

# Assign cluster membership as an edge attribute
E(net_prova)$cluster <- membership(edge_clusters)

# Get a layout for the network
layout <- layout_with_fr(net_prova)

# Plot the network clustered by edges
ggraph(net_prova, layout = "manual", x = layout[, 1], y = layout[, 2]) +
  geom_edge_link(aes(colour = factor(cluster)), edge_width = 0.5) +
  geom_node_point(aes(fill = sightings_per_dolphin), shape = 21, size = 3) +
  scale_fill_gradient(low = "blue", high = "red") +
  scale_edge_colour_brewer(palette = "Set1") +
  theme_graph() +
  theme(legend.position = "bottom")

############################################################################################################################################################################

### ORGANIZED ###
ggraph(net_dolphins_label_O, layout = "centrality", cent = graph.strength(net_dolphins_label_O)) +
  geom_edge_link2(aes(edge_colour = type), edge_linewidth = 0.5) +
  geom_node_point(aes(fill= sightings_per_dolphin), shape = 21, size = 3) +
  geom_node_text(aes(label = name, size = igraph::degree(net_dolphins_label_O)),
    family = "serif", repel = TRUE
  ) +
  scale_edge_colour_brewer(palette = "Set1") +
  #scale_fill_manual(values = c("grey66", "#EEB422", "#424242")) +
  scale_size(range = c(4, 10), guide = "none") +
  theme_graph() +
  theme(legend.position = "bottom")

ggraph(net_dolphins_label, layout = "centrality", cent = graph.strength(gotS1)) +
  geom_edge_link0(aes(edge_linewidth = weight), edge_colour = "grey66") +
  geom_node_point(aes(fill = clu, size = size), shape = 21) +
  geom_node_text(aes(size = size, label = name), family = "serif") +
  scale_edge_width_continuous(range = c(0.2, 0.9)) +
  scale_size_continuous(range = c(1, 8)) +
  scale_fill_manual(values = got_palette) +
  coord_fixed() +
  theme_graph() +
  theme(legend.position = "none")

### CONCENTRIC LAYOUT ###

ggraph(gotS1, layout = "focus", focus = 1) +
  geom_edge_link0(aes(edge_linewidth = weight), edge_colour = "grey66") +
  geom_node_point(aes(fill = clu, size = size), shape = 21) +
  geom_node_text(aes(filter = (name == "Ned"), size = size, label = name),
    family = "serif"
  ) +
  scale_edge_width_continuous(range = c(0.2, 1.2)) +
  scale_size_continuous(range = c(1, 5)) +
  scale_fill_manual(values = got_palette) +
  coord_fixed() +
  theme_graph() +
  theme(legend.position = "none")

############################################################################################################################################################################
### CIRCULAR LAYOUT ###


comps <- components(net_overall)
net_O_restricted <- delete_vertices(net_overall, which(comps$membership == which.min(comps$csize)))

comps <- components(net_O_restricted)
net_O_restricted <- delete_vertices(net_O_restricted, which(comps$membership == which.min(comps$csize)))



ggraph(net_O_restricted, layout = "centrality", cent = graph.strength(net_O_restricted)) +
  geom_edge_link2(aes(edge_colour = sightings), edge_linewidth = 0.5) +
  scale_edge_colour_manual(values = edge_colors) + 
  geom_node_point(aes(fill = sightings_per_dolphin, size = sightings_per_dolphin), shape=21) +
  scale_fill_gradient(low = node_gradient[1], high = node_gradient[2]) + 
  geom_node_text(aes(label = name), vjust = 0.5, hjust = 0.5,
                 family = "serif", size = 3) +  # Adjust font size as needed
  scale_size(range = c(4, 10), guide = "none") +
  coord_fixed() +
  theme_graph() +
  theme(legend.position = "bottom")

ggraph(gotS1, layout = "centrality", cent = graph.strength(gotS1)) +
  geom_edge_link0(aes(edge_linewidth = weight), edge_colour = "grey66") +
  geom_node_point(aes(fill = clu, size = size), shape = 21) +
  geom_node_text(aes(size = size, label = name), family = "serif") +
  scale_edge_width_continuous(range = c(0.2, 0.9)) +
  scale_size_continuous(range = c(1, 8)) +
  scale_fill_manual(values = got_palette) +
  coord_fixed() +
  theme_graph() +
  theme(legend.position = "none")
