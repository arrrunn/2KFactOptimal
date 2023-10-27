#Complete Randomization Optimal Allocation Code
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

#Block Randomization Optimal Allocation Code
optimal.nl.blk <- function(Shi,Mh,K,method="A") {
  N <- sum(Mh)
  I <- matrix(rep(c(1:2^K),length(Mh)),nrow=length(Mh),byrow=T)
  I1 <- c(I[!is.na(I)])
  nl<- matrix(rep(2,length(Mh)*2^K),nrow=length(Mh),byrow=T)
  nl.max <- sapply(Mh, function(x){x-2*2^K})

  while(sum(nl)<N & length(I1)>0){
    obj.curr <- Mh^2/N^2*Shi^2*1/nl
    delta1 <- t(sapply(c(1:length(Mh)),function(h){
                  sapply(I[h,],function(i){
                    Mh[h]^2/N^2*Shi[h,i]^2*(1/(nl[h,i]+1) - 1/nl[h,i])
                })}))
    delta2 <- t(sapply(c(1:length(Mh)),function(h){
                sapply(I[h,],function(i){
                  obj.new <- obj.curr
                  obj.new[h,i] <- obj.curr[h,i]+delta1[h,i]
                  sum(log(apply(obj.new,2,sum))) - sum(log(apply(obj.curr,2,sum)))
                })}))
    
    if(method=="A"){
      j <- sapply(c(1:length(Mh)),function(k){which(delta1[k,]==min(delta1[k,],na.rm=T))[1]})
      #adding the nl's here for A - 1 for each h at a time (faster than E-opt below)
      for (h in c(1:length(Mh))){
        if(sum(nl[h,])+1<=Mh[h]){
          if ((nl[h,j[h]] + 1) <= (nl.max[h]+2)){
            nl[h,j[h]] <- nl[h,j[h]]+1
          } else {
            I[h,j[h]] <- NA
          }
        } else {
          I[h,] <- NA
        }
      }
    } else if (method=="D"){
      j <- which(delta2==min(delta2,na.rm=T),arr.ind=T)[1,2]
      h <- which(delta2==min(delta2,na.rm=T),arr.ind=T)[1,1]
      #adding the nl's here for A - 1 for overall nl (slower than A-opt)
      if(sum(nl[h,])+1<=Mh[h]){
        if (!is.na(I[h,j]) & (nl[h,j] + 1) <= (nl.max[h]+2)){
          nl[h,j] <- nl[h,j]+1
        } else {
          I[h,j] <- NA
        }
      } else {
        I[h,] <- NA
      }
    } else {
      S2.blk <- apply(Mh^2/N^2*Shi^2/nl,2,sum)
      j <- which(S2.blk==max(S2.blk))[1]
      h <- which(delta1[,j]==min(delta1[,j],na.rm=T))[1]
      #adding the nl's here for A - 1 for overall nl (slower than A-opt)
      if(sum(nl[h,])+1<=Mh[h]){
        if (!is.na(I[h,j]) & (nl[h,j] + 1) <= (nl.max[h]+2)){
          nl[h,j] <- nl[h,j]+1
        } else {
          I[h,j] <- NA
        }
      } else {
        I[h,] <- NA
      }
    }
    I1 <- c(I[!is.na(I)])
  }
  return(nl)
}
