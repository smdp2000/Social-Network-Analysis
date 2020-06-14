library(igraph)
# READ GRAPH
G = read_graph("/home/samroadie/Desktop/expert/Dp/Friendship-network_data_2013.csv", format = "edgelist", directed = T)
V(G)
E(G)

##################################################################

# READ METADATA

meta_df = read.csv ("/home/samroadie/Desktop/expert/Dp/metadata_2013.txt", header = F, sep = "\t")

#####################################################################

# Cleaning dataset

for (i in meta_df [,1]) {
  if (!(i %in% V(G))) {
    remove <- which(meta_df$V1==i)
    print(remove)
    meta_df = meta_df [-remove,]
  }
}

UG =  as.undirected (G, mode="mutual")
tkplot (UG, vertex.size = 2, vertex.label = NA, edge.color = "blue", vertex.color = "grey", edge.width = 0.5, layout = layout_with_kk)
##########################################################################

#plotting gender graph

V(UG)[meta_df [,1]]$gender = meta_df [,3]
# Delete vertices which do not have gender specified
GG = delete.vertices (UG, V(UG)[is.na(V(UG)$gender)])
#GG = delete.vertices (GG, V(GG)$gender == "3")

V(GG)$type = ifelse(V(GG)$gender == "1", T , F)
# plot the undirected graph based on gender: Male (blue), Female (red)
V(GG)$color = ifelse (V(GG)$type, "blue", "red")
plot (GG, vertex.size = 5, vertex.label = NA, edge.color = "black", vertex.color = V(GG)$color, edge.width = 0.5, layout = layout_with_kk)

###########################################################################################################################################
#Properties
reciprocity(G)

transitivity(G)

diameter(G)



V(UG)$degree = igraph::degree (UG)

V(UG)$closeness = igraph::closeness(UG)
V(UG)$closeness


V(UG)$betweenness = igraph::betweenness (UG)
V(UG)$betweenness

Topdeg = order(V(UG)$degree,decreasing=T)[1:5]
Topclose = order(V(UG)$closeness,decreasing=T)[1:5]
Topbw = order(V(UG)$betweenness,decreasing=T)[1:5]

topnodes = intersect(intersect(Topdeg, Topclose), Topbw)

plot (UG, mark.groups=topnodes, mark.col="yellow", vertex.size = 2, vertex.label = NA, edge.color = "green", vertex.color = "black", edge.width = 0.5, layout = layout_with_kk)

##degree destribution
t = table (V(UG)$degree)
plot (t, xlab = "degree", ylab = "frequency")

###################################################################################################################################
#component analysis
comp = components(UG)
comp = decompose(UG)
cg = comp [[2]]

reciprocity(cg)

transitivity(cg)

diameter(cg)


tkplot (cg, vertex.size = 2, vertex.label = NA, edge.color = "blue", vertex.color = "grey", edge.width = 0.5, layout = layout_with_kk)


V(cg)$degree = igraph::degree (cg)
V(cg)$degree

# Plot degree distribution
tt = table (degree(cg))
tt
plot (tt, xlab = "degree", ylab = "frequency")

# Finding and printing the closeness of the graph
V(cg)$closeness = igraph::closeness(cg)
V(cg)$closeness

V(cg)$betweenness = igraph::betweenness (cg)
V(cg)$betweenness

# Finding top nodes using all the three attributes
Topdeg = order(V(cg)$degree,decreasing=T)[1:5]
Topclose = order(V(cg)$closeness,decreasing=T)[1:5]
Topbw = order(V(cg)$betweenness,decreasing=T)[1:5]

topnodes = intersect(intersect(Topdeg, Topclose), Topbw)

# plot marking those nodes
plot (cg, mark.groups=topnodes, mark.col="yellow", vertex.size = 2, vertex.label = NA, edge.color = "blue", vertex.color = "grey", edge.width = 0.5, layout = layout_with_kk)

