
library (igraph)


community <- function (G) {
  g = G
  com = components (g)$no
  
  while (com == 1) {
    
    cat ("Number of components: ", com, "\n")
    ebt = edge_betweenness (g)  
    g = g -  E(g) [which.max (ebt)]
    com = components (g)$no
  }
  
  cat ("Number of components: ", com, "\n")
  
  com1 = groups (components (g)) [1]
  com2 = groups (components (g)) [2]
  comps = c(c(com1), c(com2))
  
  return (comps)
}

communities <- function (G) {
  
  comps = community (G)
  
  c1 = as.integer (comps$`1`)
  c2 = as.integer (comps$`2`)
  print (c1)
  print (c2)
  
  V(G)$color = ifelse (V(G)%in% c2, "green", "blue")
  
  plot (G, color = V(G)$color, vertex.size = 8)  
}

G = read_graph ("/home/samroadie/Desktop/expert/Dp/karate/karate.gml", format = "gml")
communities (G)