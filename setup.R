
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
dolphins_label = dolphins_label_[, c(1, 2,4)]
# Filter dataset with type=="F"
# Create the line graph
prova= dolphins_label[, c(1,2,4)]
net_prova = graph_from_data_frame(prova, directed = FALSE)
line_graph <- make_line_graph(net_prova)
length(V(net_prova))
length(E(net_prova))
length(V(line_graph)) #  Since each node in L(G) L(G) represents an edge in G, L(G) will have 6902 nodes.
length(E(line_graph)) # Two nodes in L(G) are connected if their corresponding edges in G share a common vertex.
V(line_graph)$type <- E(net_prova)$type
str(line_graph)

levels(as.factor(V(line_graph)$type))
contract_prova = contract(line_graph,as.numeric(as.factor(V(line_graph)$type)))
V(contract_prova)
simplify(contract_prova)
data(fblog)

E(line_graph)$weight <- 1
# Contract graph by `group` attribute of vertices
g1 <- contract(line_graph, factor(V(line_graph)$type),
               vertex.attr.comb = function(x) levels(factor(x)))
# Remove loop edges and compute the sum of edge weight by group
g1 <- simplify(g1, edge.attr.comb = "sum")

plot(g1, edge.width = E(g1)$weight, vertex.label = levels(as.factor(V(line_graph)$type)), vertex.color = V(g1))

