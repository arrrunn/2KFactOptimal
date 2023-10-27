optimal.nl <- function(Si,N,K,method="A") {
  I <- c(1:2^K)
  nl<- rep(2,2^K)
  while(sum(nl)<N & length(I)>0){
    delta1 <- sapply(I,function(i){
      Si[i]^2*(1/(nl[i]+1) - 1/nl[i])
      })
    delta2 <- sapply(I,function(i){
      log(Si[i]^2/(nl[i]+1)) - log(Si[i]^2/nl[i])
    })
    if(method=="A"){
        h <- which(delta1==min(delta1))
      } else if (method=="D"){
        h <- which(delta2==min(delta2))
      } else {
        h <- which(Si^2/nl==max(Si^2/nl))
      }
    if(length(h)>1) {h <- h[1]}
    
    if ((nl[h] + 1) <= (N-2*(2^K-1))){
        nl[h] <- nl[h]+1
      } else {
        I <- I[c(-h)]
      }
  }
  return(nl)
}
