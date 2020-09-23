c<-c(134,128,124,116,108,104,122,118,116,106,96,92,126,116,114,108,96,92,114,104,102,98,86,82)
Y<-array(0,dim = c(2,2,3,2))
for (i in 1:2) {
  for (j in 1:2) {
    for (l in 1:3) {
      for (k in 1:2) {
        Y[i,j,l,k]<-c[1]
        c<-c[-1]
      }
    }
  }
}
SSA<-0
SSB<-0
SSC<-0
SSAB<-0
SSWPE<-0
SSAC<-0
SSBC<-0
SSABC<-0
SSSPE<-0
SST<-0
for (i in 1:2) {
  for (j in 1:2) {
    for (l in 1:3) {
      for (k in 1:2) {
        SSA<-SSA+(apply(Y, 1, mean)[i]-mean(Y))^2
        SSB<-SSB+(apply(Y, 2, mean)[j]-mean(Y))^2
        SSC<-SSC+(apply(Y, 3, mean)[l]-mean(Y))^2
        SSAB<-SSAB+(mean(Y[i,j, , ])-mean(Y[i, , , ])-mean(Y[ ,j, , ])+mean(Y))^2
        SSWPE<-SSWPE+(mean(Y[i,j, ,k])-mean(Y[i,j, , ]))^2
        SSAC<-SSAC+(mean(Y[i, ,l, ])-mean(Y[i, , , ])-mean(Y[ , ,l, ])+mean(Y))^2
        SSBC<-SSBC+(mean(Y[ ,j,l, ])-mean(Y[ ,j, , ])-mean(Y[ , ,l, ])+mean(Y))^2
        SSABC<-SSABC+(mean(Y[i,j,l, ])-mean(Y[i,j, , ])-mean(Y[i, ,l, ])-mean(Y[ ,j,l, ])+mean(Y[i, , , ])+mean(Y[ ,j, , ])+mean(Y[ , ,l, ])-mean(Y))^2
        SSSPE<-SSSPE+(Y[i,j,l,k]-mean(Y[i,j,l, ]))^2-(mean(Y[i,j, ,k])-mean(Y[i,j, , ]))^2
        SST<-SST+(Y[i,j,l,k]-mean(Y))^2
      }
    }
  }
}
SSA
SSB
SSC
SSAB
SSWPE
SSAC
SSBC
SSABC
SSSPE
SST
SSA+SSB+SSC+SSAB+SSWPE+SSAC+SSBC+SSABC+SSSPE-SST