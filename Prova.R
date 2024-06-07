library(graphlayouts)
library(ggforce)
library(dplyr)
# install.packages("ggraph")
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
overall_dolphins_label_ <- rbind(overall_dolphins, forage_dolphins, social_dolphins, travel_dolphins)
overall_dolphins_label_$type <- as.factor(overall_dolphins_label$type)
overall_dolphins_label_ = mutate(overall_dolphins_label_, sightings = case_when(
    V3 <= 4 ~ "1-4",
    V3 > 4 & V3 <= 8 ~ "4-8",
    V3 > 8 & V3 <= 12 ~ "8-12",
    V3 > 12 ~ "12-16",
))
overall_dolphins_label = overall_dolphins_label_[, c(1, 2, 4, 5)]


### NETWORK

# Most important one
net_overall_dolphins_label <- graph_from_data_frame(overall_dolphins_label, directed = FALSE) #from the first 250 rows of the combined data frame with labeled interaction types

# CREATE characteristics of the vertices
sightings_per_dolphin_1 = group_by(overall_dolphins_label, V1) %>% summarise(sightings = sum(V3))
sightings_per_dolphin_2 = group_by(overall_dolphins_label, V2) %>% summarise(sightings = sum(V3))

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

V(net_overall_dolphins_label)$sightings_per_dolphin = merged_data$sightings
net_overall_dolphins_label


# PLOTS
### STARTING POINT ###
ggraph(net_overall_dolphins_label, "stress", bbox = 15) +
  geom_edge_link2(aes(edge_colour = type), edge_linewidth = 0.5) +
  geom_node_point(aes(fill= sightings_per_dolphin), shape = 21, size = 3) +
  geom_node_text(aes(label = name, size = igraph::degree(net_overall_dolphins_label)),
    family = "serif", repel = TRUE
  ) +
  scale_edge_colour_brewer(palette = "Set1") +
  #scale_fill_manual(values = c("grey66", "#EEB422", "#424242")) +
  scale_size(range = c(4, 10), guide = "none") +
  theme_graph() +
  theme(legend.position = "bottom")

#trying clusters
overall_dolphins_label = overall_dolphins_label_[, c(1, 2, 4)]

### CLUSTERING ###

net_overall_dolphins_label <- graph_from_data_frame(overall_dolphins_label, directed = FALSE) #from the first 250 rows of the combined data frame with labeled interaction types
V(net_overall_dolphins_label)$sightings_per_dolphin = merged_data$sightings

bb <- layout_as_backbone(net_overall_dolphins_label, keep = 0.4)
E(g)$col <- F
E(g)$col[bb$backbone] <- T

ggraph(net_overall_dolphins_label, "stress", bbox = 15) +
  geom_edge_link2(aes(edge_colour = type), edge_linewidth = 0.5) +
  geom_node_point(aes(fill= sightings_per_dolphin), shape = 21, size = 3) +
  geom_node_text(aes(label = name, size = igraph::degree(net_overall_dolphins_label)),
    family = "serif", repel = TRUE
  ) +
  scale_edge_colour_brewer(palette = "Set1") +
  #scale_fill_manual(values = c("grey66", "#EEB422", "#424242")) +
  scale_size(range = c(4, 10), guide = "none") +
  theme_graph() +
  theme(legend.position = "bottom")

  ggraph(g,
       layout = "manual",
       x = bb$xy[, 1],
       y = bb$xy[, 2]) +
  geom_edge_link0(aes(col = col), width = 0.2) +
  geom_node_point(aes(fill = grp), shape = 21, size = 3) +
  geom_mark_hull(
    aes(x, y, group = grp, fill = grp),
    concavity = 4,
    expand = unit(2, "mm"),
    alpha = 0.25
  ) +
  scale_color_brewer(palette = "Set1") +
  scale_fill_brewer(palette = "Set1") +
  scale_edge_color_manual(values = c(rgb(0, 0, 0, 0.3), rgb(0, 0, 0, 1))) +
  theme_graph()+
  theme(legend.position = "none")

### ORGANIZED ###
ggraph(net_overall_dolphins_label, layout = "centrality", cent = graph.strength(net_overall_dolphins_label)) +
  geom_edge_link2(aes(edge_colour = type), edge_linewidth = 0.5) +
  geom_node_point(aes(fill= sightings_per_dolphin), shape = 21, size = 3) +
  geom_node_text(aes(label = name, size = igraph::degree(net_overall_dolphins_label)),
    family = "serif", repel = TRUE
  ) +
  scale_edge_colour_brewer(palette = "Set1") +
  #scale_fill_manual(values = c("grey66", "#EEB422", "#424242")) +
  scale_size(range = c(4, 10), guide = "none") +
  theme_graph() +
  theme(legend.position = "bottom")

ggraph(net_overall_dolphins_label, layout = "centrality", cent = graph.strength(gotS1)) +
  geom_edge_link0(aes(edge_linewidth = weight), edge_colour = "grey66") +
  geom_node_point(aes(fill = clu, size = size), shape = 21) +
  geom_node_text(aes(size = size, label = name), family = "serif") +
  scale_edge_width_continuous(range = c(0.2, 0.9)) +
  scale_size_continuous(range = c(1, 8)) +
  scale_fill_manual(values = got_palette) +
  coord_fixed() +
  theme_graph() +
  theme(legend.position = "none")


