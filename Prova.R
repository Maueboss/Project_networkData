library(graphlayouts)
library(ggforce)
library(dplyr)
# install.packages("concaveman",repos = "http://cran.us.r-project.org")
library(ggplot2)
library(igraph)
library(tidyverse)
library(ggraph)
library(ggrepel)
library("networkdata")


# Import the data
# Overall dolphin social network
overall_dolphins <- read.table("Data/Overall/mammalia-dolphin-florida-overall.edges")
# Forage dolphins social network
forage_dolphins <- read.table("Data/Forage/mammalia-dolphin-florida-forage.edges")
# Social dolphins social network
social_dolphins <- read.table("Data/Social/mammalia-dolphin-florida-social.edges")
# Travel dolphins social network
travel_dolphins <- read.table("Data/Travel/mammalia-dolphin-floridatravel.edges")

# Create a new dataset where we can identify all of the different relations among delphins
forage_dolphins$type <- "F"
social_dolphins$type <- "S"
travel_dolphins$type <- "T"
overall_dolphins$type <- "O"
dolphins_label_ <- rbind(overall_dolphins, forage_dolphins, social_dolphins, travel_dolphins)
dolphins_label_$type <- as.factor(dolphins_label_$type)
hist(dolphins_label_$V3, breaks = 100)
dolphins_label_ = mutate(dolphins_label_, sightings = case_when(
    V3 == 1 ~ "1",
    V3 == 2 ~ "2",
    V3 > 2 & V3 <= 4 ~ "3-4",
    V3 > 4 ~ ">4",
))
dolphins_label = dolphins_label_[, c(1, 2, 3, 4, 5)]
head(dolphins_label)

################# NETWORKS #################

### EVERYTHING ALL AT ONCE NETWORK ###
# CREATE characteristics of the vertices
sightings_per_dolphin_1 = group_by(dolphins_label, V1) %>% summarise(sightings = sum(V3))
sightings_per_dolphin_2 = group_by(dolphins_label, V2) %>% summarise(sightings = sum(V3))

# Merge the datasets based on the "name" column
merged_data <- merge(sightings_per_dolphin_1, sightings_per_dolphin_2, by.x = "V1", by.y = "V2", all = TRUE)
head(merged_data)
# Replace missing values with 0
merged_data$sightings.x[is.na(merged_data$sightings.x)] <- 0
merged_data$sightings.y[is.na(merged_data$sightings.y)] <- 0

# Create a new column with the sum of occurrences
merged_data$sightings <- merged_data$sightings.x + merged_data$sightings.y
head(merged_data)

# Remove unnecessary columns
merged_data <- select(merged_data, V1, sightings)

# Create our network
net_dolphins_label <- graph_from_data_frame(dolphins_label, directed = FALSE) #from the first 250 rows of the combined data frame with labeled interaction types
# Add the sightings column to the network
V(net_dolphins_label)$sightings_per_dolphin = merged_data$sightings
net_dolphins_label

############################################################################################################

### FORAGE NETWORK ###
# CREATE characteristics of the vertices
dolphins_label_F = dolphins_label[dolphins_label$type == "F",]
sightings_per_dolphin_1 = group_by(dolphins_label_F, V1) %>% summarise(sightings = sum(V3))
sightings_per_dolphin_2 = group_by(dolphins_label_F, V2) %>% summarise(sightings = sum(V3))

# Merge the datasets based on the "name" column
merged_data_F <- merge(sightings_per_dolphin_1, sightings_per_dolphin_2, by.x = "V1", by.y = "V2", all = TRUE)
head(merged_data_F)
# Replace missing values with 0
merged_data_F$sightings.x[is.na(merged_data$sightings.x)] <- 0
merged_data_F$sightings.y[is.na(merged_data$sightings.y)] <- 0

# Create a new column with the sum of occurrences
merged_data_F$sightings <- merged_data$sightings.x + merged_data$sightings.y
head(merged_data)

# Remove unnecessary columns
merged_data_F <- select(merged_data, V1, sightings)

# Create our network
net_dolphins_label_F <- graph_from_data_frame(dolphins_label_F, directed = FALSE) #from the first 250 rows of the combined data frame with labeled interaction types
# Add the sightings column to the network
V(net_dolphins_label_F)$sightings_per_dolphin = merged_data_F$sightings
net_dolphins_label_F

############################################################################################################

### TRAVEL NETWORK ###
# CREATE characteristics of the vertices
dolphins_label_T <- dolphins_label[dolphins_label$type == "T",]
sightings_per_dolphin_1 <- group_by(dolphins_label_T, V1) %>% summarise(sightings = sum(V3))
sightings_per_dolphin_2 <- group_by(dolphins_label_T, V2) %>% summarise(sightings = sum(V3))

# Merge the datasets based on the "name" column
merged_data_T <- merge(sightings_per_dolphin_1, sightings_per_dolphin_2, by.x = "V1", by.y = "V2", all = TRUE)
head(merged_data_T)
# Replace missing values with 0
merged_data_T$sightings.x[is.na(merged_data_T$sightings.x)] <- 0
merged_data_T$sightings.y[is.na(merged_data_T$sightings.y)] <- 0

# Create a new column with the sum of occurrences
merged_data_T$sightings <- merged_data_T$sightings.x + merged_data_T$sightings.y
head(merged_data_T)

# Remove unnecessary columns
merged_data_T <- select(merged_data_T, V1, sightings)

# Create our network
net_dolphins_label_T <- graph_from_data_frame(dolphins_label_T, directed = FALSE) #from the first 250 rows of the combined data frame with labeled interaction types
# Add the sightings column to the network
V(net_dolphins_label_T)$sightings_per_dolphin <- merged_data_T$sightings
net_dolphins_label_T

############################################################################################################

### SOCIAL NETWORK ###
# CREATE characteristics of the vertices
dolphins_label_S <- dolphins_label[dolphins_label$type == "S",]
sightings_per_dolphin_1 <- group_by(dolphins_label_S, V1) %>% summarise(sightings = sum(V3))
sightings_per_dolphin_2 <- group_by(dolphins_label_S, V2) %>% summarise(sightings = sum(V3))

# Merge the datasets based on the "name" column
merged_data_S <- merge(sightings_per_dolphin_1, sightings_per_dolphin_2, by.x = "V1", by.y = "V2", all = TRUE)
head(merged_data_S)
# Replace missing values with 0
merged_data_S$sightings.x[is.na(merged_data_S$sightings.x)] <- 0
merged_data_S$sightings.y[is.na(merged_data_S$sightings.y)] <- 0

# Create a new column with the sum of occurrences
merged_data_S$sightings <- merged_data_S$sightings.x + merged_data_S$sightings.y
head(merged_data_S)

# Remove unnecessary columns
merged_data_S <- select(merged_data_S, V1, sightings)

# Create our network
net_dolphins_label_S <- graph_from_data_frame(dolphins_label_S, directed = FALSE) #from the first 250 rows of the combined data frame with labeled interaction types
# Add the sightings column to the network
V(net_dolphins_label_S)$sightings_per_dolphin <- merged_data_S$sightings
net_dolphins_label_S

############################################################################################################

### OVERALL NETWORK ###
# CREATE characteristics of the vertices
dolphins_label_O <- dolphins_label[dolphins_label$type == "O",]
sightings_per_dolphin_1 <- group_by(dolphins_label_O, V1) %>% summarise(sightings = sum(V3))
sightings_per_dolphin_2 <- group_by(dolphins_label_O, V2) %>% summarise(sightings = sum(V3))

# Merge the datasets based on the "name" column
merged_data_O <- merge(sightings_per_dolphin_1, sightings_per_dolphin_2, by.x = "V1", by.y = "V2", all = TRUE)
head(merged_data_O)
# Replace missing values with 0
merged_data_O$sightings.x[is.na(merged_data_O$sightings.x)] <- 0
merged_data_O$sightings.y[is.na(merged_data_O$sightings.y)] <- 0

# Create a new column with the sum of occurrences
merged_data_O$sightings <- merged_data_O$sightings.x + merged_data_O$sightings.y
head(merged_data_O)

# Remove unnecessary columns
merged_data_O <- select(merged_data_O, V1, sightings)

# Create our network
net_dolphins_label_O <- graph_from_data_frame(dolphins_label_O, directed = FALSE) #from the first 250 rows of the combined data frame with labeled interaction types
# Add the sightings column to the network
V(net_dolphins_label_O)$sightings_per_dolphin <- merged_data_O$sightings
net_dolphins_label_O


# PLOTS
comps <- components(net_dolphins_label_O_restricted)
net_dolphins_label_O_restricted <- delete_vertices(net_dolphins_label_O_restricted, which(comps$membership == which.min(comps$csize)))


### STARTING POINT ###
# Suggested colors for edge categories and node gradient
edge_colors <- c("1" = "#377eb8", "2" = "#e41a1c", "3-4" = "#4daf4a", ">4" = "#984ea3")
node_gradient <- c("#f7fbff", "#08306b")  # From light blue to dark blue

# Plot the network
ggraph(net_dolphins_label_O_restricted, layout = "stress", bbox = 15) +
  geom_edge_link2(aes(edge_colour = sightings), edge_linewidth = 0.5) +
  scale_edge_colour_manual(values = edge_colors) + 
  geom_node_point(aes(fill = sightings_per_dolphin), shape = 21, size = 3) +
  scale_fill_gradient(low = node_gradient[1], high = node_gradient[2]) + 
  geom_node_text(aes(label = name, size = igraph::degree(net_dolphins_label_O_restricted)),
    family = "serif", repel = TRUE
  ) +
  scale_size(range = c(4, 10), guide = "none") +
  theme_graph() +
  theme(legend.position = "bottom")


# Define edge sizes for each category
edge_sizes <- c("1" = 0.5, "2" = 1, "3-4" = 1.5, ">4" = 2)

# Plot the network
ggraph(net_dolphins_label_O, layout = "stress", bbox = 15) +
  geom_edge_link2(aes(edge_width = sightings), edge_colour = "grey50") +
  scale_edge_width_manual(values = edge_sizes) + 
  geom_node_point(aes(fill = sightings_per_dolphin), shape = 21, size = 3) +
  scale_fill_gradient(low = "blue", high = "red") +  # You can still use the gradient for node colors
  geom_node_text(aes(label = name, size = igraph::degree(net_dolphins_label_O)),
    family = "serif", repel = TRUE
  ) +
  scale_size(range = c(4, 10), guide = "none") +
  theme_graph() +
  theme(legend.position = "bottom")


############################################################################################################################################################################

######## CLUSTERING ########
#### NODE CLUSTERING ####


bb <- layout_as_backbone(net_dolphins_label_O_restricted, keep = 0.4)
E(net_dolphins_label_O_restricted)$col <- F
E(net_dolphins_label_O_restricted)$col[bb$backbone] <- T

ggraph(net_dolphins_label_O_restricted,
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


