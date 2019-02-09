Chi.ind <- function(x,y){
  cat("Chi-sqared independence test \n \n")
q=c(0.2,0.4,0.6,0.8)
r.x=c(-Inf,quantile(x,q), Inf)
r.y=c(-Inf,quantile(y,q), Inf)
m.x=length(x)[1]  
v=matrix(rep(0, ((length(r.x)-1)*(length(r.y)-1))), nrow=(length(r.x)-1), ncol=(length(r.y)-1))

for(i in 1:(length(r.x)-1)){
  for(j in 1:(length(r.y)-1)){
    v[i,j]=sum( ((r.x[i]<x)&(x<=r.x[i+1]))*((r.y[j]<y)&(y<=r.y[j+1])) )
  } 
} 

v.x=rowSums(v)
v.y=colSums(v)

cat(v.x)
cat("\n")
cat(v.y)
cat("\n\n")

S=0;
for(i in 1:(length(r.x)-1)){
  for(j in 1:(length(r.y)-1)){
    S=S+(v[i,j]^2)/(v.x[i]*v.y[j]);
  } 
} 

S <- length(x)*(S-1)
p=pchisq(S,(length(r.x)-2)*(length(r.y)-2))

return(data.frame(Statistic=S,pvalue=p))
}
