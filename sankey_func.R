library(readr)
library(readxl)
library(networkD3)
library(plyr)
library(tidyverse)
library(tidygraph)
library(udpipe)
library(htmlwidgets)
library(webshot)
library(ijtiff)
library(magick)



if (getwd() != "C:/Users/amber/Desktop/R programming/soialNetwork"){
  setwd("C:/Users/amber/Desktop/R programming/soialNetwork")
}

dir <- getwd()

# Load Datasets
doc <- read_excel(paste0(dir,'/Topicanalysis -update.xlsx'))

top <- read_excel(paste0(dir,'/Complete_Final.xlsx'))



# Create Links using Grades
Grade_links <- data.frame(
  Source = top$`Overall grade by the peers`, 
  Target = top$`Final grade`
)


Grade_links <- Grade_links[which(!is.na(Grade_links$Source)),]

unique(Grade_links$Source)




# Clean data accordingly 
Grade_links$Target[which(Grade_links$Target == "A -")] <- "A-"


# Alter node names to create connection correctly
Grade_links$Source <- paste0("Peer Grade: ", Grade_links$Source)

Grade_links$Target <- paste0("Instructor Grade: ", Grade_links$Target)


Grade_links <- Grade_links %>% count(Source, Target)


intoSankey <- function(Grade_links){
  # From these flows we need to create a node data frame: it lists every entities involved in the flow
  nodes <- data.frame(
    name=c(as.character(Grade_links$Source), 
           as.character(Grade_links$Target)) %>% unique()
  )
  
  # With networkD3, connection must be provided using id, not using real name like in the links dataframe.. So we need to reformat it.
  Grade_links$IDsource <- match(Grade_links$Source, nodes$name)-1 
  Grade_links$IDtarget <- match(Grade_links$Target, nodes$name)-1
  
  
  links <- data.frame(source = match(Grade_links$Source, nodes$name) - 1,
                      target = match(Grade_links$Target, nodes$name) - 1,
                      value = Grade_links$n)
  
  
  
  # group by source and calculate the percentage of each node
  g <- Grade_links %>%
    group_by(Source) %>%
    summarize(cnt = sum(n)) %>%
    mutate(freq = round(cnt / sum(cnt) * 100, 2)) %>%
    arrange(desc(freq))
  
  g$order <- match(g$Source, Grade_links$Source)
  
  g <- g[order(g$order),]
  
  g<- g[1:3]
  
  g1 <- Grade_links %>%
    group_by(Target) %>%
    summarize(cnt = sum(n)) %>%
    mutate(freq = round(cnt / sum(cnt) * 100, 2)) %>%
    arrange(desc(freq))
  
  names(g1)[1] <- "Source"
  
  g1$order <- match(g1$Source, Grade_links$Source)
  
  g1 <- g1[order(g1$order),]
  
  g1 <- g1[1:3]
  
  cg <- rbind(g, g1)
  
  
  nodes$name <- sub('(.*)_\\d+', '\\1', nodes$name)
  nodes$group <- sub(".*: ", "", nodes$name)
  links$linkgroup <- sub(".*: ", "", Grade_links$Source)
  colourScale <- 
    'd3.scaleOrdinal()
     .domain(["linkgrp"])
     .range(["gainsboro"].concat(d3.schemeCategory20))'
  
  
  
  # Make the Network
  
  p <- sankeyNetwork(Links = links, Nodes = nodes, Source = "source",
                     Target = "target", Value = "value", NodeID = "name",
                     fontSize = 12,
                     fontFamily = "sans-serif", nodePadding=20,
                     margin = list(t=100),
                     sinksRight = FALSE, iterations = 0,
                     LinkGroup = "linkgroup", 
                     colourScale = colourScale,
                     NodeGroup="group",
                     width = 1200,
                     height = 1200)
  
  
  
  p$x$nodes <- cg %>% 
    mutate(name = sub("_[0-9]", "", Source)) %>% 
    select(name, freq, cnt) %>% 
    right_join(p$x$nodes, by = "name") %>% 
    mutate(freq = ifelse(is.na(freq), "", paste0(freq, "%")), cnt = ifelse(is.na(cnt), "", paste0(cnt)))
  
  
  showLabel_string <- 
    'function(el, x){
    d3.select(el).selectAll(".node text")
      .text(d => d.name + " (" + d.cnt + ") " + d.freq);}'
  
  
  
  
  p <- htmlwidgets::onRender(x = p, jsCode = showLabel_string)
  
  
  
#   p <- htmlwidgets::onRender(p, '
#   function(el) { 
#     var cols_x = this.sankey.nodes().map(d => d.x).filter((v, i, a) => a.indexOf(v) === i).sort(function(a, b){return a - b});
#     var labels = ["Grade Recieved From Peers", "Grade Recieved From the Instructor"];
#     cols_x.forEach((d, i) => {
#       d3.select(el).select("svg")
#         .append("text")
#         .attr("x", d-1)
#         .attr("y", 10)
#         .style("font-size", "14px")
#         .style("font-family", "sans-serif")
#         .text(labels[i]);
#     })
#   }
# ')
  
 return(p) 
}

p <- intoSankey(Grade_links)

p

saveWidget(p, file=paste0(getwd(), "/Grades_sankey.html"))


webshot::webshot(file = "Grades_sankey.html", "Grades_sankey.png", vwidth = 800, vheight = 800, zoom = 3)

f <- image_read(paste0(getwd(), "/Grades_sankey.png"))

image_write(f, path = "Grades_sankey.tif", format = "tif", quality = 300)


#--------------------------------------------------------------------------




Assig_links <- data.frame(
  Source = doc$`Reviewers grade in this assignment`, 
  Target = doc$`Final grade be the peers who review this work`
)


Assig_links <- Assig_links[which(!is.na(Assig_links$Source)),]

unique(Assig_links)


# Alter node names to create connection correctly
Assig_links$Source <- paste0("Peer Grade: ", Assig_links$Source)

Assig_links$Target <- paste0("Instructor Grade: ", Assig_links$Target)


Assig_links <- Assig_links %>% count(Source, Target)

p <- intoSankey(Assig_links)


p



saveWidget(p, file=paste0( getwd(), "/Assign_sankey.html"))



webshot::webshot(paste0(getwd(), "/Assign_sankey.html"),"Assign_sankey.png", vwidth = 800, vheight = 800, zoom = 3)


f <- image_read(paste0(getwd(), "/Assign_sankey.png"))

image_write(f, path = "Assign_sankey.tif", format = "tif", quality = 300)




#---------------------------------------------------------------------------------


# Create Links using Grades
links <- data.frame(
  Source = top$`Overall grade by the peers`, 
  Target = top$`Final grade`,
  Year = top$Date,
  Level = top$`Grade Level`
)


links <- links[which(!is.na(links$Source)),]

unique(links)

# Clean data accordingly 
links$Target[which(links$Target == "A -")] <- "A-"

for(i in 1:nrow(links)){
  if(grepl("2017", links$Year[i])){
    links$Year[i] <- "2017"
  }
  if(grepl("2018", links$Year[i])){
    links$Year[i] <- "2018"
  }
  if(grepl("2019", links$Year[i])){
    links$Year[i] <- "2019"
  }
  if(grepl("2020", links$Year[i])){
    links$Year[i] <- "2020"
  }
  if(grepl("2021", links$Year[i])){
    links$Year[i] <- "2021"
  }
  if(grepl("2022", links$Year[i])){
    links$Year[i] <- "2022"
  }
  if(grepl("A", links$Source[i])){
    links$Source[i] <- "A"
  }
  if(grepl("B", links$Source[i])){
    links$Source[i] <- "B"
  }
  if(grepl("C", links$Source[i])){
    links$Source[i] <- "C"
  }
  if(grepl("D", links$Source[i])){
    links$Source[i] <- "D"
  }
  if(grepl("F", links$Source[i])){
    links$Source[i] <- "F"
  }
  if(grepl("A", links$Target[i])){
    links$Target[i] <- "A"
  }
  if(grepl("B", links$Target[i])){
    links$Target[i] <- "B"
  }
  if(grepl("C", links$Target[i])){
    links$Target[i] <- "C"
  }
  if(grepl("D", links$Target[i])){
    links$Target[i] <- "D"
  }
  if(grepl("F", links$Target[i])){
    links$Target[i] <- "F"
  }
}


# Alter node names to create connection correctly
links$Source <- paste0(links$Source, " (", links$Year, ")  ")

links$Target <- paste0(links$Target, " (", links$Year, ") ")

Grade_links <- links %>% count(Source, Target, Year)

Grade_links <- arrange(Grade_links, Year, Source)



# From these flows we need to create a node data frame: it lists every entities involved in the flow
nodes <- data.frame(
  name=c(as.character(Grade_links$Source), 
         as.character(Grade_links$Target)) %>% unique()
)

# With networkD3, connection must be provided using id, not using real name like in the links dataframe.. So we need to reformat it.
Grade_links$IDsource <- match(Grade_links$Source, nodes$name)-1 
Grade_links$IDtarget <- match(Grade_links$Target, nodes$name)-1


links_final <- data.frame(source = match(Grade_links$Source, nodes$name) - 1,
                    target = match(Grade_links$Target, nodes$name) - 1,
                    value = Grade_links$n)



# group by source and calculate the percentage of each node
g <- Grade_links %>%
  group_by(Source) %>%
  summarize(cnt = sum(n)) %>%
  mutate(freq = round(cnt / sum(cnt) * 100, 2)) %>%
  arrange(desc(freq))


g1 <- Grade_links %>%
  group_by(Target) %>%
  summarize(cnt = sum(n)) %>%
  mutate(freq = round(cnt / sum(cnt) * 100, 2)) %>%
  arrange(desc(freq))

names(g1)[1] <- "Source"

cg <- rbind(g, g1)


nodes$name <- sub('(.*)_\\d+', '\\1', nodes$name)
nodes$group <- sub(".*: ", "", nodes$name)
links_final$linkgroup <- sub(".*: ", "", Grade_links$Source)
colourScale <- 
  'd3.scaleOrdinal()
     .domain(["linkgrp"])
     .range(["gainsboro"].concat(d3.schemeCategory20))'



# Make the Network

p <- sankeyNetwork(Links = links_final, Nodes = nodes, Source = "source",
                   Target = "target", Value = "value", NodeID = "name",
                   fontSize = 12,
                   fontFamily = "sans-serif", nodePadding= 20,
                   margin = list(t=100),
                   sinksRight = FALSE, iterations = 0,
                   LinkGroup = "linkgroup", 
                   colourScale = colourScale,
                   NodeGroup="group",
                   width = 800,
                   height = 800)

p



#p$x$nodes <- cg %>% 
#  mutate(name = sub("_[0-9]", "", Source)) %>% 
#  select(name, freq) %>% 
#  right_join(p$x$nodes, by = "name") %>% 
#  mutate(freq = ifelse(is.na(freq), "", paste0(freq, "%")))


#showLabel_string <- 
#  'function(el, x){
#    d3.select(el).selectAll(".node text")
#      .text(d => d.name + " (" + d.value + ") " + d.freq);}'



#p <- htmlwidgets::onRender(x = p, jsCode = showLabel_string)

#p <- htmlwidgets::onRender(x = p, jsCode = addTitle_string)

#p <- htmlwidgets::prependContent(p, htmltools::tags$h2("Student Final Grade Recieved by Peers Versus the Instructor by Year"))

p <- htmlwidgets::onRender(p, '
  function(el) { 
    var cols_x = this.sankey.nodes().map(d => d.x).filter((v, i, a) => a.indexOf(v) === i).sort(function(a, b){return a - b});
    var labels = ["Grade Recieved From Peers", "Grade Recieved From the Instructor"];
    cols_x.forEach((d, i) => {
      d3.select(el).select("svg")
        .append("text")
        .attr("x", d-1)
        .attr("y", 10)
        .style("font-size", "14px")
        .style("font-family", "sans-serif")
        .text(labels[i]);
    })
  }
')

p

saveWidget(p, file=paste0( getwd(), "/Grades_sankey_year.html"))



#---------------------------------------------------------------------------------


# Create Links using Grades
links <- data.frame(
  Source = top$`Overall grade by the peers`, 
  Target = top$`Final grade`,
  Year = top$Date,
  Level = top$`Grade Level`
)


links <- links[which(!is.na(links$Source)),]

unique(links)

# Clean data accordingly 
links$Target[which(links$Target == "A -")] <- "A-"

for(i in 1:nrow(links)){
  if(grepl("A", links$Source[i])){
    links$Source[i] <- "A"
  }
  if(grepl("B", links$Source[i])){
    links$Source[i] <- "B"
  }
  if(grepl("C", links$Source[i])){
    links$Source[i] <- "C"
  }
  if(grepl("D", links$Source[i])){
    links$Source[i] <- "D"
  }
  if(grepl("F", links$Source[i])){
    links$Source[i] <- "F"
  }
  if(grepl("A", links$Target[i])){
    links$Target[i] <- "A"
  }
  if(grepl("B", links$Target[i])){
    links$Target[i] <- "B"
  }
  if(grepl("C", links$Target[i])){
    links$Target[i] <- "C"
  }
  if(grepl("D", links$Target[i])){
    links$Target[i] <- "D"
  }
  if(grepl("F", links$Target[i])){
    links$Target[i] <- "F"
  }
}


# Alter node names to create connection correctly
links$Source <- paste0("Peer Grade: ", links$Source, " (Grade Level ", links$Level, ")  ")

links$Target <- paste0("Instructor Grade: ", links$Target, " (Grade Level ", links$Level, ") ")

Grade_links <- links %>% count(Source, Target, Level)

Grade_links <- arrange(Grade_links, Level, Source)



# From these flows we need to create a node data frame: it lists every entities involved in the flow
nodes <- data.frame(
  name=c(as.character(Grade_links$Source), 
         as.character(Grade_links$Target)) %>% unique()
)

# With networkD3, connection must be provided using id, not using real name like in the links dataframe.. So we need to reformat it.
Grade_links$IDsource <- match(Grade_links$Source, nodes$name)-1 
Grade_links$IDtarget <- match(Grade_links$Target, nodes$name)-1


links_final <- data.frame(source = match(Grade_links$Source, nodes$name) - 1,
                          target = match(Grade_links$Target, nodes$name) - 1,
                          value = Grade_links$n)



# group by source and calculate the percentage of each node
g <- Grade_links %>%
  group_by(Source) %>%
  summarize(cnt = sum(n)) %>%
  mutate(freq = round(cnt / sum(cnt) * 100, 2)) %>%
  arrange(desc(freq))


g1 <- Grade_links %>%
  group_by(Target) %>%
  summarize(cnt = sum(n)) %>%
  mutate(freq = round(cnt / sum(cnt) * 100, 2)) %>%
  arrange(desc(freq))

names(g1)[1] <- "Source"

cg <- rbind(g, g1)


nodes$name <- sub('(.*)_\\d+', '\\1', nodes$name)
nodes$group <- sub(".*: ", "", nodes$name)
links_final$linkgroup <- sub(".*: ", "", Grade_links$Source)
colourScale <- 
  'd3.scaleOrdinal()
     .domain(["linkgrp"])
     .range(["gainsboro"].concat(d3.schemeCategory20))'



# Make the Network

p <- sankeyNetwork(Links = links_final, Nodes = nodes, Source = "source",
                   Target = "target", Value = "value", NodeID = "name",
                   fontSize = 12,
                   fontFamily = "sans-serif", nodePadding= 20,
                   margin = list(t=100),
                   sinksRight = FALSE, iterations = 0,
                   LinkGroup = "linkgroup", 
                   colourScale = colourScale,
                   NodeGroup="group",
                   width = 800,
                   height = 800)

p



#p$x$nodes <- cg %>% 
#  mutate(name = sub("_[0-9]", "", Source)) %>% 
#  select(name, freq) %>% 
#  right_join(p$x$nodes, by = "name") %>% 
#  mutate(freq = ifelse(is.na(freq), "", paste0(freq, "%")))


#showLabel_string <- 
#  'function(el, x){
#    d3.select(el).selectAll(".node text")
#      .text(d => d.name + " (" + d.value + ") " + d.freq);}'




#p <- htmlwidgets::onRender(x = p, jsCode = showLabel_string)

#p <- htmlwidgets::onRender(x = p, jsCode = addTitle_string)

#p <- htmlwidgets::prependContent(p, htmltools::tags$h2("Student Final Grade Recieved by Peers Versus the Instructor by Year"))

p <- htmlwidgets::onRender(p, '
  function(el) { 
    var cols_x = this.sankey.nodes().map(d => d.x).filter((v, i, a) => a.indexOf(v) === i).sort(function(a, b){return a - b});
    var labels = ["Grade Recieved From Peers", "Grade Recieved From the Instructor"];
    cols_x.forEach((d, i) => {
      d3.select(el).select("svg")
        .append("text")
        .attr("x", d-1)
        .attr("y", 10)
        .style("font-size", "14px")
        .style("font-family", "sans-serif")
        .text(labels[i]);
    })
  }
')

p
saveWidget(p, file=paste0( getwd(), "/Grades_sankey_level.html"))


