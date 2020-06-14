library(igraph)
node <- c()
p <- c()

for(i in 1 : 100 )
{
  G1 = make_empty_graph(i)
  for(j in 1:200)
  {
    prob = j/200
    for(k in 1:(i-1))
    {                       
      for(l in (k+1):i)
      {
        cp = runif(1)
        if(cp <= prob)
        {
          G1 = G1 + edge(k,l)
        }
      }
    }
    if(is.connected(G1))
    {
      node <- c(node,i)
      p <- c(p,prob)
      break
    }
  }
}
plot(node, p , xlim =c(0,100) ,ylim=c(0,0.15),type="l")








