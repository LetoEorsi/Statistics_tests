SimpleTest <- function(x){
  l <- length(x)
  s <- sign(x-median(x))
 # cat("\n",s,"\n")
  k=1
  p=0;
  n=0;
  a=0;
  for(i in 1:(l-1)){
    a=a+1;
    if(s[i]*s[i+1]<0){
      k=k+1
          
      if((s[i+1]<=0)&(p<a)){
        p=a  
      }
      if((s[i+1]>0)&(n<a)){
        n=a
      }
      
      
        a=0
      }
    
  }
  if((p==0)&(s[1]==1)){
    p=l;
  }
  if((n==0)&(s[1]==-1)){
    n=l;
  }
  S=(k-(l+1)/2)/(sqrt(l-1)/2)
  
  cat("\n",p," ",n,"\n")
  n=p
  P=rep(0, n+1)
  for(i in 1:(n)){
    P[i]=1/2^(n+1-i)
  }
  P[n+1]=-1
  lam=polyroot(P)
  L=matrix(1:(n^2),nrow = n,ncol=n)
  u_0=1-2^n+(0:(n-1))/2
  for(i in 1:(n)){
    for(j in 0:(n-1)){
      L[j,i]=lam[i]^(n+j)
    }
  }
  c=solve(L,u_0)
  #cat("\n",abs(c))
  #cat("\n",abs(lam),"\n")
  ans=0
  for(i in 1:(n)){
    ans=ans+c[i]*lam[i]^(l)
   # cat("  ",abs(c[i]*lam[i]^(l)))
    
  }
  #cat("\n",c[n]*lam[n]^(l)/2^n+1)
  ans=ans/(2^n)+1
  #cat("\n",Re(ans))
  
  return(data.frame(Statistic1=S,pvalue1=pnorm(S,0,1),Statistic2=n,pvalue2=Re(ans)))
}

