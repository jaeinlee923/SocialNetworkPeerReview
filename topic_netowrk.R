library(readxl)
library(igraph)
library(ggraph)
library(udpipe)
library(textrank)
library(arcdiagram)
library(reshape2)
library(tm)
library(plyr)
library(tidyverse)
library(tidygraph)
library(viridis)
library(patchwork)
library(hrbrthemes)
library(colormap)
library(stringr)

if (getwd() != "C:/Users/amber/Desktop/R programming/soialNetwork"){
  setwd("C:/Users/amber/Desktop/R programming/soialNetwork")
}

dir <- getwd()

topic <- read_excel(paste0(dir,'/Topicanalysis -update.xlsx'))

names(topic)[1] <- "Student Number"
names(topic)[4] <- "Overall grade by the peers"
names(topic)[5] <- "Assignment Grade"
names(topic)[6] <- "Assignment Grade by Peers"

# Get rid of the A - variables replace them with A- in Final Grade
topic[which(topic$`Final grade` == "A -"),2] <- "A-"

# check if the NAs of final grades and peer grades match up perfectly
check <- which(is.na(topic$`Final grade`)) == which(is.na(topic$`Overall grade by the peers`))

# check how many doesn't match
which(check == FALSE)

# They all match!
# Now Fill in the NAs with the correct grades
for(i in 1:nrow(topic)) {
  if (is.na(topic$`Final grade`[i]) == FALSE) { 
    g <- topic$`Final grade`[i]
    r <- topic$`Overall grade by the peers`[i]
  }
  else{
    topic$`Final grade`[i] <- g
    topic$`Overall grade by the peers`[i] <- r
  }
}

# Eliminate plus and minus from grades by replace varialbes A- and A+ into A
grades <- topic
grades$`Final grade`[grades$`Final grade` == "A-" | grades$`Final grade` == "A+"] <- "A"
grades$`Final grade`[grades$`Final grade` == "B-" | grades$`Final grade` == "B+"] <- "B"
grades$`Final grade`[grades$`Final grade` == "C-" | grades$`Final grade` == "C+"] <- "C"
grades$`Final grade`[grades$`Final grade` == "D-" | grades$`Final grade` == "D+"] <- "D"

# Do the same for grade by peers.
grades$`Overall grade by the peers`[grades$`Overall grade by the peers` == "A-" | grades$`Overall grade by the peers` == "A+"] <- "A"
grades$`Overall grade by the peers`[grades$`Overall grade by the peers` == "B-" | grades$`Overall grade by the peers` == "B+"] <- "B"
grades$`Overall grade by the peers`[grades$`Overall grade by the peers` == "C-" | grades$`Overall grade by the peers` == "C+"] <- "C"
grades$`Overall grade by the peers`[grades$`Overall grade by the peers` == "D-" | grades$`Overall grade by the peers` == "D+"] <- "D"

grades$topic1 <- NA
grades$topic2 <- NA
grades$topic3 <- NA

for(r in 1:nrow(grades)){
  list <- unlist(strsplit(grades$`Top terms`[r],","))
  if(length(list) == 1){
    grades$topic1[r] <- list[1]
  }
  else if(length(list) == 2){
    grades$topic1[r] <- list[1]
    grades$topic2[r] <- list[2]  
  }
  else{
    grades$topic1[r] <- list[1]
    grades$topic2[r] <- list[2]
    grades$topic3[r] <- list[3]
  }
}


grades$topic1 <- tolower(grades$topic1)
grades$topic2 <- tolower(grades$topic2)
grades$topic3 <- tolower(grades$topic3)


comment <- grades$`Peer Comment`
# Clean up Text
comment <- gsub("[^[:alnum:][:space:]']", "", comment)
comment <- tolower(comment)
comment <- removeWords(comment, stopwords("en"))
comment[which(comment == "none response")] <- NA
comment <- str_squish(comment)

grades$text <- comment

# Set up unipie model
ud_model <- udpipe_download_model(language = "english")
ud_model <- udpipe_load_model(ud_model$file_model)


# Annotate using the english udpipe model
x <- udpipe_annotate(ud_model, x = grades$text)
x <- as.data.frame(x)


# Generate Key words
stat <- keywords_rake(x = x, term = "lemma", group = c("doc_id", "paragraph_id", "sentence_id"), ngram_max = 2, relevant = x$xpos %in% c("NN", "JJ"))
stat <- stat[order(-stat$rake),]

# Order by frequency as well
stat <- stat[-which(stat$keyword == "fo"),]

stat <- stat[order(-stat$freq),]

# See which keywords are made of two words
two_words <- stat_freq[grepl(" ", stat_freq$keyword),]

rownames(stat) <- NULL
head(stat, 50)

# Create Key word variables
grades$keywords <- ""

# Erase NA for now
grades$text[which(is.na(grades$text))] <- ""

# For loop for the entire keyword dataset
for(i in 1:nrow(grades)){
  n <- 1
  print(paste0("comment number ", i))
  for(a in 1:nrow(stat)){
    if(grepl(stat$keyword[a], grades$text[i])){
      if(n == 1) {
        grades$keywords[i] <- paste0(stat$keyword[a])
        n <- 2
      }
      else {
        grades$keywords[i] <- paste0(grades$keywords[i],",", stat$keyword[a])
      }
    }
    else{
      next
    }
  }
}

# Create two words variables
grades$two_words <- ""


# For loop for the entire keyword dataset
for(i in 1:nrow(grades)){
  n <- 1
  print(paste0("comment number ", i))
  for(a in 1:nrow(two_words)){
    if(grepl(two_words$keyword[a], grades$text[i])){
      if(n == 1) {
        grades$two_words[i] <- paste0(two_words$keyword[a])
        n <- 2
      }
      else if(n > 1) {
        grades$two_words[i] <- paste0(grades$two_words[i],",", two_words$keyword[a])
      }
    }
    else{
      next
    }
  }
}


# Create two words variables
grades$keyterm1 <- ""
grades$keyterm2 <- ""
grades$keyterm3 <- ""
grades$keyterm4 <- ""

# For loop to separate keyterms
for(i in 1:nrow(grades)){
  n <- 1
  print(paste0("comment number ", i))
  for(a in 1:nrow(two_words)){
    if(grepl(two_words$keyword[a], grades$text[i])){
      if(n == 1) {
        grades$keyterm1[i] <- paste0(two_words$keyword[a])
        n <- 2
      }
      else if(n == 2) {
        grades$keyterm2[i] <- paste0(two_words$keyword[a])
        n <- 3
      }
      else if(n == 3) {
        grades$keyterm3[i] <- paste0(two_words$keyword[a])
        n <- 4
      }
      else if(n == 4) {
        grades$keyterm3[i] <- paste0(two_words$keyword[a])
        n <- 5
      }
    }
    else{
      next
    }
  }
}





# Add another variable count
grades$count <- 1:nrow(grades)

# Add another variable to input which comment has at least three of the same keyword

grades$connected4 <- ""

grades$connected5 <- ""

grades$connected6 <- ""

grades$connected4_keys <- ""
grades$connected5_keys <- ""
grades$connected5_keys_less <- ""
grades$connected6_keys <- ""
grades$connected6_keys_less <- ""

grades$keywords[which(grades$keywords == "")] <- NA

# Now make connections if we see three same keywords in two comments
for (i in 1:nrow(grades)){
  keywords1 <- unlist(strsplit(grades$keywords[i],","))
  keywordsRest <- as.data.frame(grades$keywords[-i])
  names(keywordsRest) <- "keywords"
  restNum <- grades$count[-i]
  for (k in 1:nrow(keywordsRest)){
    keywords2 <- unlist(strsplit(keywordsRest$keywords[k],",")) 
    # Pull out common words
    same_keys <- intersect(keywords1, keywords2)
    overlap <- c()
    if (length(same_keys) > 1) {
      for(a in 1:length(same_keys)){
        restkey <- same_keys[-a]
        for(b in 1:length(restkey)){
          if(grepl(same_keys[a], restkey[b], fixed = TRUE)){
            overlap <- append(overlap, a)
          }
          else {
            next
          }
        }
      }
    }
    if(!is.null(overlap)){
      same_keys <- same_keys[-overlap] 
    }
    if (length(same_keys) == 4) {
      grades$connected4[i] <- paste0(grades$connected4[i], restNum[k], " ")
      grades$connected4_keys[i] <- paste(same_keys,collapse=', ')
    }
    else if (length(same_keys) > 4) {
      grades$connected5[i] <- paste0(grades$connected5[i], restNum[k], " ")
      grades$connected5_keys[i] <- paste(same_keys,collapse=', ')
      grades$connected5_keys_less[i] <- paste(str_to_title(same_keys[1:3]),collapse=', ')
    }
    else if (length(same_keys) > 5) {
      grades$connected6[i] <- paste0(grades$connected6[i], restNum[k], " ")
      grades$connected6_keys[i] <- paste(same_keys,collapse=', ')
      grades$connected6_keys_less[i] <- paste(str_to_title(same_keys[1:3]),collapse=', ')
    }
    else{
      next
    }
  }
}

# Get the 
grades$connected_term1 <- ""

grades$connected_term2 <- ""

for (i in 1:nrow(grades)){
  if (grades$two_words[i] != ""){
    keywords1 <- unlist(strsplit(grades$two_words[i],","))
    keywordsRest <- as.data.frame(grades$two_words[-i])
    names(keywordsRest) <- "two_words"
    restNum <- grades$count[-i]
    for (k in 1:nrow(keywordsRest)){
      keywords2 <- unlist(strsplit(keywordsRest$two_words[k],",")) 
      # Pull out common words
      same_keys <- intersect(keywords1, keywords2)
      if (length(same_keys) == 1) {
        grades$connected_term1[i] <- paste0(grades$connected_term1[i], restNum[k], " ")
      }
      else if (length(same_keys) == 2) {
        grades$connected_term2[i] <- paste0(grades$connected_term2[i], restNum[k], " ")
      }
      else{
        next
      }
    }
  }
  else{
    next
  }
  
}


write_csv(grades,paste0(dir,"/grades_final.csv"))

write_csv(stat,paste0(dir,"/keywords.csv"))

# Create Connection per every key term
grades_melt <- grades[c("Peer Comment", "Student Number", "keyterm1", "keyterm2", "keyterm3", "keyterm4", "count")]

# melt the key terms
grades_melted <- melt(grades_melt, c("Peer Comment", "Student Number", "count"))

grades_melted$value[which(grades_melted$value == "")] <- NA

# Add another variable to input which comment has at least one of the same key terms
grades_melted$connected_term <- ""
grades_melted$connected_term_num <- ""
grades_melted$count_melt <- 1:nrow(grades_melted)

term <- grades_melted$value

for (i in 1:nrow(grades_melted)){
  keyterm1 <- term[i]
  keywordsRest <- as.data.frame(term[-i])
  names(keywordsRest) <- "value"
  restNum <- grades_melted$count[-i]
  n <- 1
  for (k in 1:nrow(keywordsRest)){
    keyterm2 <- keywordsRest$value[k]
    if(!is.na(keyterm1) && !is.na(keyterm2)){
      if (keyterm1 == keyterm2 && n == 1) {
        grades_melted$connected_term[i] <- keyterm2
        grades_melted$connected_term_num[i] <- restNum[k]
        term[restNum[k]] <- NA
        n <- n + 1
      }
      else if (keyterm1 == keyterm2 && n >= 2) {
        grades_melted$connected_term[i] <- keyterm2
        grades_melted$connected_term_num[i] <- paste0(grades_melted$connected_term_num[i], ",", restNum[k])
      }
      else{
        next
      }
    }
    else{
      next
    }

  }
}


peer_comment1 <- c()
peer_comment2 <- c()
terms <- c()


for(i in 1:nrow(grades_melted)){
  if(grades_melted$connected_term_num[i] != ""){
    connected_term <- unlist(strsplit(grades_melted$connected_term_num[i],","))
    connected_term <- as.integer(connected_term)
    for(a in 1:length(connected_term)){
      terms <- append(terms, grades_melted$connected_term[i])
      peer_comment1 <- append(peer_comment1, grades_melted$count[i])
      peer_comment2 <- append(peer_comment2, connected_term[a])
    }
  }
  else{
    next
  }
}

peer_comment_network <- data.frame(peer_comment1, peer_comment2, terms)



# Delete the connections that is repeated in the earlier process
for(i in 1:nrow(peer_comment_network)){
  if(!is.na(peer_comment_network$peer_comment1[i])){
    comb1 <- peer_comment_network$peer_comment1[i]
    comb2 <- peer_comment_network$peer_comment2[i]
    for(a in 1:nrow(peer_comment_network[-i,])){
      if(!is.na(peer_comment_network$peer_comment1[-i][a])){
        if (peer_comment_network$peer_comment1[-i][a] == comb2 && peer_comment_network$peer_comment2[-i][a] == comb1){
          peer_comment_network$peer_comment1[a+1] <- NA
          peer_comment_network$peer_comment2[a+1] <- NA
        }
        else{
          next
        } 
      }
      else{
        next
      }
    }
  }
  else{
    next
  }
}



two_terms <- grades[which(grades$connected_term2 != ""),]

peer_comment1_tt <- c()
peer_comment2_tt <- c()
two_term_connect <- c()


for(i in 1:nrow(two_terms)){
  connected_term <- unlist(strsplit(two_terms$connected_term2[i]," "))
  connected_term <- as.integer(connected_term)
  for(a in 1:length(connected_term)){
    two_term_connect <- append(two_term_connect, two_terms$two_words[i])
    peer_comment1_tt <- append(peer_comment1_tt, two_terms$count[i])
    peer_comment2_tt <- append(peer_comment2_tt, connected_term[a])
  }
}

strong_peer_comment_network <- data.frame(peer_comment1_tt, peer_comment2_tt)


for(i in 1:nrow(strong_peer_comment_network)){
  if(!is.na(strong_peer_comment_network$peer_comment1_tt[i])){
    comb1 <- strong_peer_comment_network$peer_comment1_tt[i]
    comb2 <- strong_peer_comment_network$peer_comment2_tt[i]
    for(a in 1:nrow(strong_peer_comment_network[-i,])){
      if(!is.na(strong_peer_comment_network$peer_comment1_tt[-i][a])){
        if (strong_peer_comment_network$peer_comment1_tt[-i][a] == comb2 && strong_peer_comment_network$peer_comment2_tt[-i][a] == comb1){
          strong_peer_comment_network$peer_comment1_tt[a+1] <- NA
          strong_peer_comment_network$peer_comment2_tt[a+1] <- NA
        }
        else{
          next
        } 
      }
      else{
        next
      }
    }
  }
  else{
    next
  }
}


strong_peer_comment_network <- strong_peer_comment_network[which(!is.na(strong_peer_comment_network$peer_comment1_tt)),]






# Distinguish which nodes represent instructor comments
names(peer_comment_network) <- c("Source","Target", "terms")

peer_comment_network <- peer_comment_network[which(!is.na(peer_comment_network$Source)),]

# Distinguish which nodes represent instructor comments
inst <- comp$count[which(comp$`Student Number` == "instructor")]

A <- comp$count[which(comp$`Overall grade by the peers` == "A")]

B <- comp$count[which(comp$`Overall grade by the peers` == "B")]

C <- comp$count[which(comp$`Overall grade by the peers` == "C")]

D <- comp$count[which(comp$`Overall grade by the peers` == "D")]

Ff <- comp$count[which(comp$`Overall grade by the peers` == "F")]

# Work on ordering our graph
order <- c(peer_comment_network$Source, peer_comment_network$Target)
order1 <- sort(unique(order[which(order %in% inst)]), decreasing = FALSE)

order1a <- sort(unique(order1[which(order1 %in% A)]), decreasing = FALSE)
order1b <- sort(unique(order1[which(order1 %in% B)]), decreasing = FALSE)
order1c <- sort(unique(order1[which(order1 %in% C)]), decreasing = FALSE)
order1d <- sort(unique(order1[which(order1 %in% D)]), decreasing = FALSE)
order1f <- sort(unique(order1[which(order1 %in% Ff)]), decreasing = FALSE)

order1 <- c(order1a,order1b,order1c,order1d,order1f)


order2 <- sort(unique(order[which(!order %in% inst)]), decreasing = FALSE)

order2a <- sort(unique(order2[which(order2 %in% A)]), decreasing = FALSE)
order2b <- sort(unique(order2[which(order2 %in% B)]), decreasing = FALSE)
order2c <- sort(unique(order2[which(order2 %in% C)]), decreasing = FALSE)
order2d <- sort(unique(order2[which(order2 %in% D)]), decreasing = FALSE)
order2f <- sort(unique(order2[which(order2 %in% Ff)]), decreasing = FALSE)

order2 <- c(order2a,order2b,order2c,order2d,order2f)

new_ord <- c(order1, order2)

# Create Vertices Dataframe
student <- as.data.frame(new_ord)

student$status <- 0

names(student) <- c("comment", "status")

student$status[which(student$comment %in% inst)] <- 1

# Create Network
pc_net <- graph_from_data_frame(peer_comment_network, vertices=student, directed=FALSE)






# Set attributes

# Create labels
label <- peer_comment_network$terms[which(new_ord %in% order)]

# Get the degree
deg <- degree(pc_net, mode="all")

# Get the nodes with greater degrees
d <- which(deg > 10)

# Create labels only for those with higher degrees
label[-d] <- ""

V(pc_net)$label <- label




# Set color attribute
V(pc_net)$color <- c(rep("Instructor, A", length(order1a)), rep("Instructor, B", length(order1b)), rep("Instructor, C", length(order1c)), rep("Instructor, D", length(order1d)), rep("Instructor, F", length(order1f)), rep("Student, A", length(order2a)), rep("Student, B", length(order2b)), rep("Student, C", length(order2c)),  rep("Student, D", length(order2d)), rep("Student, F", length(order2f)))



V(pc_net)$group <- ifelse(V(pc_net)$status == 1, "instructor", "student")

#V(pc_net)$label <- c(rep("A", 57), rep("B", 10), rep("C", 6), rep("D", 2), rep("F", 0), rep("A", 139), rep("B", 20), rep("C", 17),  rep("D", 5), rep("F", 0))


ggraph(pc_net, layout = 'linear') + 
  geom_edge_arc(edge_colour="black", edge_alpha=0.3, edge_width=0.4) +
  geom_node_point(aes(size= deg/2, color=V(pc_net)$color), alpha=0.5) +
  scale_color_manual(name = "Commented by,\nGrade by Peers",
                     values = c("navy", "royalblue4", "royalblue","lightblue", "olivedrab", "olivedrab3", "gold", "goldenrod") ) +
  geom_node_text(aes(label=V(pc_net)$label), angle=90, hjust=0.95,vjust=0.2, repel = F, size=2.2, color="black", nudge_y=-1.4) +
  theme_void() +
  labs(title = "Connections of peer review comments by 6 Common Keywords", subtitle = "Showing only three most frequently appearing keywords for nodes with degrees greater than 7", size = "Degrees") + 
  theme(plot.margin = margin(50, 50, 50, 50))  + 
  expand_limits(y = -11) + 
  guides(colour = guide_legend(override.aes = list(size=4)))









# Distinguish which nodes represent instructor comments
names(strong_peer_comment_network) <- c("Source","Target")

strong_peer_comment_network <- strong_peer_comment_network[which(!is.na(strong_peer_comment_network$Source)),]

# Distinguish which nodes represent instructor comments
inst <- comp$count[which(comp$`Student Number` == "instructor")]

A <- comp$count[which(comp$`Overall grade by the peers` == "A")]

B <- comp$count[which(comp$`Overall grade by the peers` == "B")]

C <- comp$count[which(comp$`Overall grade by the peers` == "C")]

D <- comp$count[which(comp$`Overall grade by the peers` == "D")]

Ff <- comp$count[which(comp$`Overall grade by the peers` == "F")]

# Work on ordering our graph
order <- c(strong_peer_comment_network$Source, strong_peer_comment_network$Target)
order1 <- sort(unique(order[which(order %in% inst)]), decreasing = FALSE)

order1a <- sort(unique(order1[which(order1 %in% A)]), decreasing = FALSE)
order1b <- sort(unique(order1[which(order1 %in% B)]), decreasing = FALSE)
order1c <- sort(unique(order1[which(order1 %in% C)]), decreasing = FALSE)
order1d <- sort(unique(order1[which(order1 %in% D)]), decreasing = FALSE)
order1f <- sort(unique(order1[which(order1 %in% Ff)]), decreasing = FALSE)

order1 <- c(order1a,order1b,order1c,order1d,order1f)


order2 <- sort(unique(order[which(!order %in% inst)]), decreasing = FALSE)

order2a <- sort(unique(order2[which(order2 %in% A)]), decreasing = FALSE)
order2b <- sort(unique(order2[which(order2 %in% B)]), decreasing = FALSE)
order2c <- sort(unique(order2[which(order2 %in% C)]), decreasing = FALSE)
order2d <- sort(unique(order2[which(order2 %in% D)]), decreasing = FALSE)
order2f <- sort(unique(order2[which(order2 %in% Ff)]), decreasing = FALSE)

order2 <- c(order2a,order2b,order2c,order2d,order2f)

new_ord <- c(order1, order2)

# Create Vertices Dataframe
student <- as.data.frame(new_ord)

student$status <- 0

names(student) <- c("comment", "status")

student$status[which(student$comment %in% inst)] <- 1

spc_net <- graph_from_data_frame(strong_peer_comment_network, vertices=student, directed=FALSE)



# Set attributes


V(spc_net)$color <- c(rep("darkblue", 21), rep("royalblue", 3), rep("skyblue4", 2), rep("lightblue4", 0), rep("lightblue", 0), rep("olivedrab", 27), rep("olivedrab3", 5), rep("goldenrod", 8),  rep("gold", 2), rep("moccasin", 0))

V(spc_net)$label <- c(rep("A", 21), rep("B", 3), rep("C", 2), rep("D", 0), rep("F", 0), rep("A", 27), rep("B", 5), rep("C", 8),  rep("D", 2), rep("F", 0))

V(spc_net)$group <- ifelse(V(spc_net)$status == 1, "instructor", "student")

deg <- degree(spc_net, mode="all")

ggraph(spc_net, layout = 'linear') + 
  geom_edge_arc(edge_colour="black", edge_alpha=0.3, edge_width=0.2) +
  geom_node_point(color=V(spc_net)$color, size= deg, alpha=0.5) +
  geom_node_text(aes(label=V(spc_net)$label), repel = FALSE, size=2, color="black", nudge_y=-1) +
  theme_void() +
  theme(plot.margin=unit(rep(2,4), "cm")) + 
  labs(title = "Connection by Two Common Keyphrases", subtitle = "Peer review comments that contain two of the same key phrases.")








spc_net <- graph_from_data_frame(strong_peer_comment_network, vertices = student, directed=FALSE)



# Set edge label
spc_net <- set_edge_attr(spc_net, name = "label", value = factor(strong_peer_comment_network$two_term_connect))


# Set vertext colors
V(spc_net)$color <- ifelse(V(spc_net)$status == 1, "lightblue", "orange")

V(spc_net)$group <- ifelse(V(spc_net)$status == 1, "instructor", "student")

V(spc_net)$label <-  V(spc_net)$name


elabels = get.edge.attribute(spc_net, "label")
vcolor = get.vertex.attribute(spc_net, "color")
vgroup = get.vertex.attribute(spc_net, "group")







pc_net.edge <- get.edgelist(pc_net)
spc_net.edge <- get.edgelist(spc_net)

# Match colors
order <- as.character(order)

new_ord <- as.character(new_ord)

col <- rep(0, 160)

col[which(new_ord %in% inst)] <- "lightblue"
  
col[which(!new_ord %in% inst)] <- "orange"
  
# lets try this
myCols <- setNames(col,as.character(order))
  
longer_order_match  <- order(match(unique(matrix(spc_net.edge)), new_ord))
color.names_match <- myCols[match(unique(matrix(spc_net.edge)), new_ord)]





arcplot(spc_net.edge, show.nodes = TRUE,  ordering = new_ord, col.nodes = color.names_match, show.labels = TRUE,
         main = "Peer Review Connections: Two Phrases",
        col.main = "navy")


ggraph(spc_net, layout = 'linear') + 
  geom_edge_arc(edge_colour="black", edge_alpha=0.3, edge_width=0.2) +
  geom_node_point(color=V(spc_net)$color, size=2) +
  geom_node_text(aes(label=name), repel = FALSE, size=2, color="black", nudge_y=-0.5) +
  theme_void() +
  theme(plot.margin=unit(rep(2,4), "cm")) + 
  labs(title = "Connections with Two Terms")




    
#hsv(runif(9,0.5,0.9),alpha=0.4)
#lwd.arcs=4*runif(10,.5,2)

