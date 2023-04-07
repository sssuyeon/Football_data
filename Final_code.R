setwd('/Users/suyeon/Desktop/Yonsei/First/NetworkAnalysis')
rm(list = ls())
library(tidyverse)
library(igraph)
library(intergraph)
library(statnet.common)
library(networkD3)
library("knitr")
library(sna)
library(RColorBrewer)
library(ggplot2)
library(gridExtra)
library(grid)
library(latentnet)
library(plotMCMC)
library(mcmcplots)

######################## Data load ########################
football = read.graph('football.gml', format = 'gml')
summary(football) # undirected
class(football)
str(football)

vertex_attr(football) # id, label, value
vcount(football) # the number of nodes : 115
ecount(football) # the number of edges : 613
V(football) # nodes
E(football) # edges connection

######################## Network Data ########################
football_net = asNetwork(football)
class(football_net)
get.vertex.attribute(football_net, "label")

####################### Visualization #######################
par(mfrow = c(1,1))
# Edge list
edgelist = as_edgelist(football)
edgelist
EG = graph.edgelist(edgelist, directed = FALSE)
plot.igraph(EG, vertex.size = 10, vertex.label.cex = 0.5, margin = c(-0.3, -0.3),
            vertex.label.color = 'black', vertex.color = 'lightsteelblue')

# Adjacency matrix
ADJ = as_adjacency_matrix(football, sparse = FALSE)
ADJ
AG = graph.adjacency(ADJ, mode = 'undirected', weighted = NULL, diag = FALSE,)
plot.igraph(AG, vertex.size = 10, vertex.label.cex = 0.5, margin = c(-0.3, -0.3),
            vertex.label.color = 'black', vertex.color = 'lightsteelblue')

# Print a sample of 10 nodes
E(football)[sample(1:ecount(football), 10)]

# conference 별로 node color 다르게
V(football)$label # team(college) name
V(football)$value # conference name

colors = c('#FFADAD', '#FFD6A5', '#FDFFB6', '#CAFFBF',
           '#BDE0FE', '#A0C4FF', '#CDDAFD', '#BDB2FF',
           '#3D5A80', '#ADB5BD', '#565264')
conferences = c('Atlantic Coast', 'Big East', 'Big Ten', 'Big Twelve',
                'ConferenceUSA', 'Independents', 'Mid-American', 
                'Mountain West', 'Pacific Ten', 'Southeastern',
                'Sun Belt', 'Western Athletic')

V(football)$conference = conferences[V(football)$value + 1]
conf_names <- sort(unique(V(football)$conference))
conf_names

set.seed(42)
l = layout.kamada.kawai(football)
conf_num = as.factor(V(football)$conference)
conf_num = as.numeric(conf_num)
plot(football, layout = l, vertex.size = 10, vertex.label = NA,
     vertex.color = colors, margin = c(-0.3, -0.3),)

set.seed(1234)
LO_LGL = layout_with_lgl(football)
plot(as.undirected(football), layout = LO_LGL, margin = c(-0.3, -0.3),
     vertex.size = 10, vertex.label = NA, vertex.color = colors)
# 색을 통해 conference별로 구분했지만 단순하게 노드가 엣지로 연결된 형태밖에 보이지 못 함
# 어떤 노드가 중요한지 나타낼 수 없음

# VxOrd 기반의 DrL방법
set.seed(42)
ld = layout.drl(football)
plot(football, layout = ld, vertex.size = 10, vertex.label = NA,
     vertex.color = colors, margin = c(-0.3, -0.3))
# 색이 비슷한 노드들끼리 잘 모이는 것 같지는 않음

# conference별 그룹의 크기 => node의 크기
# 그룹간의 edge의 수 => edge의 두께
football_c = contract.vertices(football, conf_num)
E(football_c)$weight = 1
football_c = simplify(football_c)

conf_size = as.vector(table(V(football)$conference))
plot(football_c, vertex.size = 10*sqrt(conf_size), vertex.label = conf_names,
     vertex.color = colors, vertex.label.color = 'black',
     vertex.label.dist = 1.5, vertex.col = colors,
     edge.arrow.size = 0, edge.width = sqrt(E(football_c)$weight^(1.5)),
     margin = c(-0.3, -0.3))

football_nodes = get.data.frame(football, what = 'vertices') # id, label, value
football_edges = get.data.frame(football, what = 'edges') # from, to
str(football_nodes)
str(football_edges)

# start from 0
football_edges$from = football_edges$from - 1
football_edges$to = football_edges$to - 1

forceNetwork(Links = football_edges, Nodes = football_nodes,
             Source = 'from', Target = 'to', NodeID = 'label',
             linkDistance = 80, Group = 'label', opacity = 0.8)

####################### Node Degree ########################
degree = igraph::degree(football, mode = "out")
summary(degree)
hist(degree, col = 'lightsteelblue', xlab = 'Node Degree', main = '')
plot(igraph::degree(football), main = 'Node Degree plot',
     col = 'lightsteelblue', pch = 20, ylab = 'Node Degree',
     xlab = 'Team college')

# Degree 값이 높을수록 노드의 크기가 크도록 수정
V(football)$size = degree*1.5
plot(football, vertex.label.cex = 1, vertex.label.color = 'black',
     vertex.color = colors, margin = c(-0.3, -0.3))
# 링크가 많이 연결된 노드가 눈에 띄게 보이지 않음
# team 대부분이 비슷한 링크 수를 가진듯 보임

################### Closeness Centrality ####################
close = igraph::closeness(football)
close.score = round((close - min(close))*length(close)/max(close)) + 1

# Degree 값이 높을수록 노드의 크기가 큰 것 유지
# Closeness 값이 작을수록 노드의 색상이 진해지도록 수정
close.colors = rev(heat.colors(max(close.score)))
V(football)$color = close.colors[close.score]
plot(football, vertex.label.cex = 1, vertex.label.color = 'black',
     margin = c(-0.3, -0.3))

################## Betweenness Centrality ###################
between = igraph::betweenness(football)
between.score = round(between) + 1

# Degree 값이 높을수록 노드의 크기가 큰 것 유지
# Betweenness 값이 클수록 노드의 색상이 진해지도록 수정
between.colors = heat.colors(max(between.score))
V(football)$color = between.colors[between.score]
plot(football, vertex.label.cex = 1, vertex.label.color = 'black',
     margin = c(-0.3, -0.3))

##################### Clique Cluster ########################
cliques(football)
sapply(cliques(football), length) # clique num
largest_cliques(football) # largest clique

vcolor = rep('#999999', vcount(football))
vcolor[unlist(largest_cliques(football))] = '#FFFF00'
plot(as.undirected(football), vertex.color = vcolor,
     vertex.label.cex = 0.5, margin = c(-0.3, -0.3))

####################### Visualization #######################
ADJ
G = as.network.matrix(ADJ)

par(mfrow = c(1,2))
gplot.target(G, degree(G), main = 'Degree', circ.lab = FALSE,
             circ.col = 'lightsteelblue', usearrows = FALSE,
             vertex.col = c('blue', rep('red', 32), '#FFFF00'),
             edge.col = 'slategray')
gplot.target(G, closeness(G), main = 'Closeness', circ.lab = FALSE,
             circ.col = 'lightsteelblue', usearrows = FALSE,
             vertex.col = c('blue', rep('red', 32), '#FFFF00'),
             edge.col = 'slategray')

par(mfrow = c(1,2))
gplot.target(G, betweenness(G), main = 'Betweenness', circ.lab = FALSE,
             circ.col = 'lightsteelblue', usearrows = FALSE,
             vertex.col = c('blue', rep('red', 32), '#FFFF00'),
             edge.col = 'slategray')
gplot.target(G, evcent(G), main = 'EigenValue', circ.lab = FALSE,
             circ.col = 'lightsteelblue', usearrows = FALSE,
             vertex.col = c('blue', rep('red', 32), '#FFFF00'),
             edge.col = 'slategray')

####################### Visualization #######################
plot(football_net)
set.seed(0)
plot(football_net,
     vertex.cex = degree/10,
     vertex.col = "#3F4788", vertex.border = "white",
     edge.lwd = 0.01, edge.col = "gray75")
title("Degree", line = 1, cex.main = 0.9)

set.seed(0)
plot(football_net,
     vertex.cex = close*350,
     vertex.col = "#32648E", vertex.border = "white", 
     edge.lwd = 0.01, edge.col = "gray75")
title("Closeness", line = 1, cex.main = 0.9)

set.seed(0)
plot(football_net,
     vertex.cex = between/100,
     vertex.col = "#287D8E", vertex.border = "white", 
     edge.lwd = 0.01, edge.col = "gray75")
title("Betweenness", line = - 1, cex.main = 0.9)

set.seed(0)
plot(football_net, 
     vertex.cex = evcent(G)*10,
     vertex.col = "#1F968B", vertex.border = "white",
     edge.lwd = 0.01, edge.col = "gray75")
title("Eigenvector", line = - 1, cex.main = 0.9)

#################### Link prediction #######################
# Hierarchical Random Graph Model
predict = predict_edges(football)
pred_edge = predict$edges
pred_prob = predict$prob
result = cbind(pred_edge, pred_prob)
colnames(result) = c('from', 'to', 'prob')
result

#################### LSM #######################
(dol_fit <- ergmm(football_net ~ euclidean(d=2)))
summary(dol_fit)
mcmc_list_dol = as.mcmc(dol_fit)
mcmc = as.mcmc(mcmc_list_dol) # mcmc

# Autocorrelation plot
plotAuto(mcmc, main = 'Autocorrelation', col = colors[9],
        lag.max = 50, ann=FALSE)

# Traceplot
traplot(mcmc, col = colors[6], plot.title = 'Traceplot', 
        style = 'plain', greek = TRUE)
