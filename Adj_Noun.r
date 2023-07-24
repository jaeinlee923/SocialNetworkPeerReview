library(readxl)
library(igraph)
library(ggraph)
library(udpipe)

# This is just something I do at the beginning of the code to ensure the directory is set correctly
if (getwd() != "C:/Users/amber/Desktop/R programming/soialNetwork"){
  setwd("C:/Users/amber/Desktop/R programming/soialNetwork")
}

dir <- getwd()

input_file <- read_excel(paste0(dir,'/Complete_Final.xlsx'))

# Get rid of the A - variables replace them with A- in Final Grade
input_file[which(input_file$`Final grade` == "A -"),2] <- "A-"

y <- data.frame(input_file$Nouns, input_file$Adj)
names(y) <- c("Nouns", "Adj")

# Create Network
net <- graph_from_data_frame(d = y, directed = T)

# get adjacency matrix
adj <- get.adjacency(net, sparse = TRUE)

# Graph again
net <- graph.adjacency(adj, mode="directed", weighted=TRUE)


V(net)
E(net)
V(net)$label <- V(net)$name
V(net)$degree <- degree(net)
E(net)$width <- log(E(net)$weight)


# Histogram of node degree
hist(V(net)$degree, 
     col = "tomato2", 
     main = "Histogram of Node Degree",
     ylab = "Frequency",
     xlab = "Degree of Vertices")
# Plot
par(mar = rep(1, 4))
plot(net,
     vertex.color = "tomato",
     edge.color = rgb(0.5, 0.5, 0.5, 0.5),
     vertex.size = (V(net)$degree)/1.5 + 6,
     edge.arrow.size = 0.2,
     vertex.label.cex=1, 
     vertex.label.family= "Arial",
     vertex.label.color="black", 
     layout=layout_with_graphopt, 
     main="Number of Nouns to Adjectives")


ggraph(net) +
  geom_edge_link(aes(color = "salmon", edge_linewidth  = weight, arrow = TRUE), alpha = 0.5) +     # different edge color per group
  geom_node_point(size = log(V(net)$degree)*4 + 6, shape = 21, stroke = .5,
                  fill = 'white', color = 'black') +
  geom_node_text(aes(label = name)) +                   # "name" is automatically generated from the node IDs in the edges
  theme_void()


# Community detection
cnet <- cluster_edge_betweenness(net)

# reduce margins
par(mar = rep(1, 4))

# Plot
plot(cnet,
     net,
     edge.arrow.size = 0.2,
     vertex.label.family= "Arial",
     vertex.label.color="black", 
     main = "Community Detection",
     layout = layout_with_gem)

# Get keywords
rapidrake(
  input_file$`Peer Comment`,
  stop_words = slowraker::smart_words
)

keywords <- keywords_rake(x = input_file$`Peer Comment`, term = "lemma")

