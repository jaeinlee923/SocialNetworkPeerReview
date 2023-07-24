library(readxl)
library(igraph)
library(ggraph)
library(tm)
library(topicmodels)
library(tidytext)
library(ggplot2)
library(dplyr)
library(gridExtra)
library(reshape2)


library(udpipe)
library(textrank)


# This is just something I do at the beginning of the code to ensure the directory is set correctly
if (getwd() != "C:/Users/amber/Desktop/R programming/soialNetwork"){
  setwd("C:/Users/amber/Desktop/R programming/soialNetwork")
}

dir <- getwd()

input_file <- read_excel(paste0(dir,'/Complete_Final.xlsx'))

# Get rid of the A - variables replace them with A- in Final Grade
input_file[which(input_file$`Final grade` == "A -"),2] <- "A-"

# check if the NAs of final grades and peer grades match up perfectly
check <- which(is.na(input_file$`Final grade`)) == which(is.na(input_file$`Overall grade by the peers`))
# check how many doesn't match
which(check == FALSE)


# They all match!
# Now Fill in the NAs with the correct grades
for(i in 1:nrow(input_file)) {
  if (is.na(input_file$`Final grade`[i]) == FALSE) { 
    g <- input_file$`Final grade`[i]
    r <- input_file$`Overall grade by the peers`[i]
  }
  else{
    input_file$`Final grade`[i] <- g
    input_file$`Overall grade by the peers`[i] <- r
  }
}

# Eliminate plus and minus from grades by replace varialbes A- and A+ into A
grades <- input_file
grades$`Final grade`[grades$`Final grade` == "A-" | grades$`Final grade` == "A+"] <- "A"
grades$`Final grade`[grades$`Final grade` == "B-" | grades$`Final grade` == "B+"] <- "B"
grades$`Final grade`[grades$`Final grade` == "C-" | grades$`Final grade` == "C+"] <- "C"
grades$`Final grade`[grades$`Final grade` == "D-" | grades$`Final grade` == "D+"] <- "D"

# Do the same for grade by peers.
grades$`Overall grade by the peers`[grades$`Overall grade by the peers` == "A-" | grades$`Overall grade by the peers` == "A+"] <- "A"
grades$`Overall grade by the peers`[grades$`Overall grade by the peers` == "B-" | grades$`Overall grade by the peers` == "B+"] <- "B"
grades$`Overall grade by the peers`[grades$`Overall grade by the peers` == "C-" | grades$`Overall grade by the peers` == "C+"] <- "C"
grades$`Overall grade by the peers`[grades$`Overall grade by the peers` == "D-" | grades$`Overall grade by the peers` == "D+"] <- "D"


grades$Verb[which(is.na(grades$Verb))] <- 0
grades$Nouns[which(is.na(grades$Nouns))] <- 0
grades$Nouns[which(is.na(grades$Adj))] <- 0


# Summarize so that we can create a pie chart per grade with the percentage of nouns, adj, adv and verb
grade_sum <- grades %>% 
  group_by(`Overall grade by the peers`) %>% 
  summarize(Nouns = sum(Nouns), Verb = sum(Verb), adv = sum(adv), Adj = sum(Adj))

grade_sum <- melt(grade_sum, id = "Overall grade by the peers")

# Create percentage variable
grade_sum$percent <- NA

# Get percentage data
for(i in 1:nrow(grade_sum)) {
  if (grade_sum$`Overall grade by the peers`[i] == "A") {
    sum <- sum(subset(grade_sum, `Overall grade by the peers` %in% "A")$value)
    grade_sum$percent[i] <- round(grade_sum$value[i]/sum, 3)
  } else if (grade_sum$`Overall grade by the peers`[i] == "B") {
    sum <- sum(subset(grade_sum, `Overall grade by the peers` %in% "B")$value)
    grade_sum$percent[i] <- round(grade_sum$value[i]/sum, 3)
  } else if (grade_sum$`Overall grade by the peers`[i] == "C") {
    sum <- sum(subset(grade_sum, `Overall grade by the peers` %in% "C")$value)
    grade_sum$percent[i] <- round(grade_sum$value[i]/sum, 3)
  } else if (grade_sum$`Overall grade by the peers`[i] == "D") {
    sum <- sum(subset(grade_sum, `Overall grade by the peers` %in% "D")$value)
    grade_sum$percent[i] <- round(grade_sum$value[i]/sum, 3)
  } else {
    sum <- sum(subset(grade_sum, `Overall grade by the peers` %in% "F")$value)
    grade_sum$percent[i] <- round(grade_sum$value[i]/sum, 3)
  }
}


# Add label position
grade_sum <- grade_sum %>%
  arrange(desc(`Overall grade by the peers`)) %>%
  mutate(cs = rev(cumsum(rev(percent))), 
         lab = percent/2 + lead(cs, 1, default = 0))

grade_sum

# Plot
ggplot(grade_sum, aes(x = 2, y = percent, fill = variable)) +
  geom_bar(stat = "identity") +
  coord_polar(theta = "y", start = 0)+
  #geom_text(aes(y = lab, label = percent), color = "black")+
  theme_void()+
  facet_wrap(~`Overall grade by the peers`) + 
  labs(title = "Type of Words Appeared in Peer Reviews\n")


ggplot(grade_sum, aes(x = variable, y = percent,  fill = variable)) +
  geom_bar(stat = "identity") +
  facet_wrap(~`Overall grade by the peers`)


# Function call for co-occurences
cooccurr <- function(x, g){
  # Annotate using the english udpipe model
  comments <- subset(x, `Overall grade by the peers` %in% g)
  ud_model <- udpipe_download_model(language = "english")
  ud_model <- udpipe_load_model(ud_model$file_model)
  x <- udpipe_annotate(ud_model, x = comments$`Peer Comment`)
  x <- as.data.frame(x)
  head(x)
  
  # Start collocation process
  stats <- keywords_collocation(x = x, 
                                term = "token", group = c("doc_id", "paragraph_id", "sentence_id"),
                                ngram_max = 4)
  
  ## Co-occurrences: How frequent do words occur in the same sentence, in this case only nouns or adjectives
  stats <- cooccurrence(x = subset(x, upos %in% c("NOUN", "ADJ")), 
                        term = "lemma", group = c("doc_id", "paragraph_id", "sentence_id"))
  
  ## Co-occurrences: How frequent do words follow one another
  stats <- cooccurrence(x = x$lemma, 
                        relevant = x$upos %in% c("NOUN", "ADJ"))
  
  ## Co-occurrences: How frequent do words follow one another even if we would skip 2 words in between
  stats_freq <- cooccurrence(x = x$lemma, 
                        relevant = x$upos %in% c("NOUN", "ADJ"), skipgram = 2)
  stats_freq <- stats_freq[!grepl("response", stats_freq $term1),]
  stats_freq <- stats_freq[!grepl("none", stats_freq $term1),]
  # Build network with the top n Co-occurring words
  wordnetwork <- head(stats_freq, 30)
  gr <- graph_from_data_frame(wordnetwork)
  return(gr)
  
}

coocAll <- cooccurr(grades, c("A", "B", "C", "D", "F"))
coocA <- cooccurr(grades, "A")
coocB <- cooccurr(grades, "B")
coocC <- cooccurr(grades, "C")
coocD <- cooccurr(grades, "D")
coocF <- cooccurr(grades, "F")

# Function to graph coocurrences
graphcooc <- function(c, n) {
  str <- deparse(substitute(c))
  g <- ggraph(c, layout = "circle") +
    geom_edge_link(aes(width = cooc, edge_alpha = cooc), edge_colour = "lightgray", alpha = 0.5) +
    geom_node_text(aes(label = name), col = "grey15", size = 5, repel = T) +
    geom_node_point(aes(color="darkred"), size = 2) +
    theme_graph(base_family = "Arial Narrow") +
    theme(legend.position = "none") +
    coord_fixed() +
    labs(title = ifelse(str == "coocAll","All Grades", paste0(substr(str, nchar(str), nchar(str))))) +
    theme(plot.title = element_text(hjust = 0.5, size = 15))+   
    scale_x_continuous(expand = expand_scale(c(.14, .14))) +
    scale_y_continuous(expand = expand_scale(c(.14, .14)))
  return(g)
}



cAll <- graphcooc(coocAll, 30)
cAll
cA <- graphcooc(coocA, 30)
cB <- graphcooc(coocB, 30)
cC <- graphcooc(coocC, 30)
cD <- graphcooc(coocD, 30)
cF <- graphcooc(coocF, 30)


# Create tiff
tiff("Coocurrences_2.tif",width=6000, height=3500, res = 300)

# Combine 
g <- grid.arrange(grobs = list(cA,cB,cC, cD, cF), nrow = 2, top = textGrob("Cooccurrences within 3 words distance", gp = gpar(fontsize=20)))

dev.off()

# get adjacency matrix
adj <- get.adjacency(coocAll, sparse = TRUE)

# Graph again
net <- graph.adjacency(adj, mode="directed", weighted=TRUE)


V(net)
E(net)
V(net)$label <- V(net)$name
V(net)$degree <- degree(net)
E(net)$width <- E(net)$weight

plot(net, vertex.shape="none", 
     vertex.label.font=2, 
     vertex.label.color="gray40",
     vertex.size = log(V(wAll)$degree)*6 + 10,
     vertex.label.cex=1, edge.color="gray85",
     edge.arrow.size = 0.5,
     vertex.label.family= "Arial",
     main = "Community Detection: All Grades",
     layout = layout.circle)






wordnetwork <- function(d, g, a, b){
  subgrades <- subset(d, `Overall grade by the peers` %in% g)
  y <- data.frame(subgrades$Nouns, subgrades$Verb)
  
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
  E(net)$width <- E(net)$weight/27 + 1
  
  
  # Histogram of node degree
  hist(V(net)$degree, 
       col = "tomato3", 
       main = "Histogram of Node Degree",
       ylab = "Frequency",
       xlab = "Degree of Vertices")
  
  # Check the names of each nodes
  sort(as.numeric(V(net)$name))
  
  # Prepare colors
  V(net)$color[V(net)$name %in% as.character(21:30)] <- "tomato3"
  V(net)$color[V(net)$name %in% as.character(11:20)] <- "coral"
  V(net)$color[V(net)$name %in% as.character(1:10)] <- "gold"
  V(net)$color[V(net)$name %in% as.character(0)] <- "moccasin"
  return(net)
  
}



wAll <- wordnetwork(grades, c("A", "B", "C", "D", "F"))
wA <- wordnetwork(grades, "A")
wB <- wordnetwork(grades, "B")
wC <- wordnetwork(grades, "C")
wD <- wordnetwork(grades, "D")
wF <- wordnetwork(grades, "F")

# Set layout 
layout(matrix(1, 1, 1, byrow = TRUE))
# Plot all
par(mar = rep(1, 4))
plot(wAll,
     edge.color = rgb(0.5, 0.5, 0.5, 0.5),
     vertex.size = log(V(wAll)$degree)*6 + 10,
     vertex.frame.color="white",
     edge.arrow.size = 0.2,
     vertex.label.cex=1, 
     vertex.label.family= "Arial",
     vertex.label.color="black", 
     layout=layout.circle, 
     main="Relationship of Number of Nouns and Verbs",
     sub = "Peer Comment of All Grades")
# Add legend
legend(x=0.99, y= -0.6, legend=c("0", "1 -10", "11-20", "21-30"), 
       pch=21, pt.bg=c("moccasin", "gold", "coral", "tomato3"), pt.lwd = 0, pt.cex=2, bty="n")



#tiff("NounsVerbs.tif",width=4608, height=2382, res = 300)

# Set layout 
m <- matrix(c(1,2,3,4,5,6),nrow = 2, ncol = 3,byrow = TRUE)

layout(mat = m)

# Plot A
par(mar = rep(1, 4))
plot(wA,
     edge.color = rgb(0.5, 0.5, 0.5, 0.5),
     vertex.size = log(V(wA)$degree)*5 + 15,
     vertex.frame.color="white",
     vertex.frame.cex=0,
     edge.arrow.size = 0.5,
     vertex.label.cex=1.3, 
     vertex.label.family= "Arial",
     vertex.label.color="black", 
     layout=layout.circle,
     cex.main = 0.2)

title("A",cex.main=1.5)



# Plot B
par(mar = rep(1, 4))
plot(wB,
     edge.color = rgb(0.5, 0.5, 0.5, 0.5),
     vertex.size = log(V(wB)$degree)*5 + 15,
     vertex.frame.color="white",
     vertex.frame.cex=0,
     edge.arrow.size = 0.5,
     vertex.label.cex=1.3, 
     vertex.label.family= "Arial",
     vertex.label.color="black", 
     layout=layout.circle, 
     cex.main = 0.2)

title("B",cex.main=1.5)


# Plot C
par(mar = rep(1, 4))
plot(wC,
     edge.color = rgb(0.5, 0.5, 0.5, 0.5),
     vertex.size = log(V(wC)$degree)*5 + 15,
     vertex.frame.color="white",
       vertex.frame.cex=0,
     edge.arrow.size = 0.5,
     vertex.label.cex=1.3, 
     vertex.label.family= "Arial",
     vertex.label.color="black", 
     layout=layout.circle,
     cex.main = 0.2)

title("C",cex.main=1.5)


# Plot D
par(mar = rep(1, 4))
plot(wD,
     edge.color = rgb(0.5, 0.5, 0.5, 0.5),
     vertex.size = log(V(wD)$degree)*5 + 15,
     vertex.frame.color="white",
     vertex.frame.cex=0,
     edge.arrow.size = 0.5,
     vertex.label.cex=1.3, 
     vertex.label.family= "Arial",
     vertex.label.color="black", 
     layout=layout.circle, 
     cex.main = 0.2)

title("D",cex.main=1.5)



# Plot F
par(mar = rep(1, 4))
plot(wF,
     edge.color = rgb(0.5, 0.5, 0.5, 0.5),
     vertex.size = log(V(wF)$degree)*5 + 15,
     vertex.frame.color="white",
     edge.arrow.size = 0.5,
     vertex.label.cex=1.3, 
     vertex.label.family= "Arial",
     vertex.label.color="black", 
     layout=layout.circle,
     cex.main = 0.2)
title("F",cex.main=1.5)

# empty plot
plot(1, type = "n", axes=FALSE, xlab="", ylab="")

text(1,0.9,"Connections Between the Number of Nouns & Verbs of\nPeer Comments from Each Letter Grade",cex=1.4,font=3, pos = 3)
# Add legend
legend(x= "bottom", y= "bottom",  legend=c("0", "1 -10", "11-20", "21-30"), 
       pch=21, col = "white", pt.bg=c("wheat1", "gold", "coral", "tomato2"), cex=1.5, pt.cex=2.7, bty="n",  horiz = TRUE)

#dev.off()


# Community detection
cnetAll <- cluster_edge_betweenness(wAll)
cnetA <- cluster_edge_betweenness(wA)
cnetB <- cluster_edge_betweenness(wB)
cnetC <- cluster_edge_betweenness(wC)
cnetD <- cluster_edge_betweenness(wD)
cnetF <- cluster_edge_betweenness(wF)


# Set layout 
layout(matrix(1, 1, 1, byrow = TRUE))

# Plot All
par(mar = rep(1, 4))
plot(cnetAll,
     wAll,
     edge.arrow.size = 0.5,
     vertex.label.family= "Arial",
     vertex.label.color="black", 
     main = "Community Detection: All Grades",
     layout = layout.circle)


# Download tiff
tiff("Community_NounsVerbs.tif",width=5000, height=2500, res = 300)

# Set layout 
layout(matrix(c(1:6), 2, 3, byrow = TRUE))

# Plot A
par(mar = rep(1, 4))
plot(cnetA,
     wA,
     edge.arrow.size = 0.5,
     vertex.label.family= "Arial",
     vertex.label.color="black", 
     cex.main = 3,
     layout = layout.circle)
title("A",cex.main=1.5)

# Plot B
par(mar = rep(1, 4))
plot(cnetB,
     wB,
     edge.arrow.size = 0.5,
     vertex.label.family= "Arial",
     vertex.label.color="black", 
     cex.main = 0.2,
     layout = layout.circle)
title("B",cex.main=1.5)

# Plot C
par(mar = rep(1, 4))
plot(cnetC,
     wC,
     edge.arrow.size = 0.5,
     vertex.label.family= "Arial",
     vertex.label.color="black",
     cex.main = 0.2,
     layout = layout.circle)
title("C",cex.main=1.5)

# Plot D
par(mar = rep(1, 4))
plot(cnetD,
     wD,
     edge.arrow.size = 0.5,
     vertex.label.family= "Arial",
     vertex.label.color="black", 
     cex.main = 0.2,
     layout = layout.circle)
title("D",cex.main=1.5)

# Plot F
par(mar = rep(1, 4))
plot(cnetF,
     wF,
     edge.arrow.size = 0.5,
     vertex.label.family= "Arial",
     vertex.label.color="black",
     cex.main = 6,
     layout = layout.circle)
title("F",cex.main=1.5)

# empty plot
plot(1, type = "n", axes=FALSE, xlab="", ylab="")

text(1,0.9,"Community Detection Between Every Peer Comment\nNouns & Verbs Count by Each Letter Grade",cex=1.4,font=3, pos = 3)

dev.off()


# get corpus
corpus <- Corpus(VectorSource(grades$`Peer Comment`))

# Clean corpus
corpus <- tm_map(corpus, tolower)
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeNumbers)
# Remove stop words
corpus <- tm_map(corpus, removeWords, stopwords("english"))

# Create document term matrix
dtm <- DocumentTermMatrix(corpus)

# Fit LDA model
lda_model <- LDA(dtm, k = 5, method = "Gibbs", control=list(iter = 500, verbose = 25, alpha = 0.2))

# extracting the per-topic-per-word probabilities, called beta
lda_topics <- lda_model %>% 
  tidy(matrix = "beta")

# Get the top 20 terms 
top_terms <- lda_topics %>%
  group_by(topic) %>%
  slice_max(beta, n = 20) %>% 
  ungroup() %>%
  arrange(topic, -beta)

# Visualize it
top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  scale_y_reordered()



