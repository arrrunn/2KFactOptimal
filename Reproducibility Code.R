################################################################
#     Source codes with custom functions
################################################################
source('OptimalAlloc.R')

################################################################
#     Reproducing manuscript results
################################################################
#Section 6.1: Angrist experiment
N.Ang = 1656
Sj2.Ang <- c(1,1,1,1)
#Shi is the standard deviation of each PO
optimal.nl(Sj2.Ang,N=N.Ang,K=2,method="A")
optimal.nl(Sj2.Ang,N=N.Ang,K=2,method="D")
optimal.nl(Sj2.Ang,N=N.Ang,K=2,method="E")

Sj2.Ang.blk <- matrix(c(1,1,1,1,1,1,1,1),nrow=2)
Mh.Ang = c(948, 708)
N = N.Ang

#Shi is the standard deviation of each PO
optimal.nl.blk(Shi=Sj2.Ang.blk,Mh=c(Mh.Ang),K=2,method="A")
optimal.nl.blk(Shi=Sj2.Ang.blk,Mh=c(Mh.Ang),K=2,method="D")
optimal.nl.blk(Shi=Sj2.Ang.blk,Mh=c(Mh.Ang),K=2,method="E")

#Section 6.2: Libgober experiment
#overall
Sj2.Law <- sqrt(c(0.21,0.2,0.18,0.2,0.23,0.21,0.27,0.21))
K=3
N.Law=192
#Shi is the standard deviation of each PO
optimal.nl(Sj2.Law,N=N.Law,K=3,method="A")
optimal.nl(Sj2.Law,N=N.Law,K=3,method="D")
optimal.nl(Sj2.Law,N=N.Law,K=3,method="E")

#blk
Sj2.Law.blk <- sqrt(matrix(c(0.15,0.15,0.15,0.2,0.27,0.15,0.27,0.27,
                             0.27,0.24,0.2,0.2,0.2,0.27,0.27,0.15),nrow = 2,byrow=T))
K=3
Mh.Law <- c(192/2,192/2)
#Shi is the standard deviation of each PO
optimal.nl.blk(Shi=Sj2.Law.blk,Mh=c(Mh.Law),K=3,method="A")
optimal.nl.blk(Shi=Sj2.Law.blk,Mh=c(Mh.Law),K=3,method="D")
optimal.nl.blk(Shi=Sj2.Law.blk,Mh=c(Mh.Law),K=3,method="E")

################################################################