i=1
j=1
k=1
a <- c()
b <- c()
c <- c()
while(i<100){
  
  if(i>=1 && i<=31){
       a <- c(a,i)}

  else if(i>=32 && i<=62 ){
       b<- c(b,i)
       j=j+1
       }
  else{
  c<- c(c,i);
  k=k+1}
  i=i+1
}
    


bb = make_empty_graph(n=100)
i=1

while(2*i+1 <= 31)
{
  bb = bb+ edge(a[i],a[2*i])
  bb = bb+ edge(a[i],a[2*i+1])
  
  i=i+1
  
  
}
i=1
while(2*i+1 <= 31)
{
  bb = bb+ edge(b[i],b[2*i])
  bb = bb+ edge(b[i],b[2*i+1])
  
  i=i+1
  
  
}
i=1
while(2*i+1 <= 37)
{
  bb = bb+ edge(c[i],c[2*i])
  bb = bb+ edge(c[i],c[2*i+1])
  
  i=i+1
  
  
}

bb = bb + edge(100,a[1])
bb = bb + edge(100,b[1])
bb = bb + edge(100,c[1])
plot (bb, vertex.color = "orange", edge.color = "blue", edge.arrow.size = 0.2, layout = layout_with_kk, rescale=FALSE, xlim=c(-5,5), ylim=c(-4,5), vertex.size=60)
