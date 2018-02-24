daihiko_t<-function(x,y,paired=TRUE) {
  if (paired) {
    d<-data.frame(x,y)
    d<-subset(d,complete.cases(d))
    rt<-t.test(d$x,d$y,paired=TRUE)
    rsd<-c(sd(d$x,na.rm=T),sd(d$y,na.rm=T))
    cohend<-abs(mean(d$x-d$y)/(sd(d$x-d$y)/sqrt(2*(1-cor(d$x,d$y)))))
    return(list(meanx=mean(d$x),meany=mean(d$y),SD=rsd,t=rt,cohend=cohend))
  }
  else {  
    rt<-t.test(x~y)
    rsd<-tapply(x,y,sd,na.rm=TRUE)
    rt1<-table(y,x)
    rt2<-apply(rt1,1,sum)
    cohend<-(rt$estimate[1]-rt$estimate[2])/sqrt(((rt2[1]-1)*rsd[1]^2+(rt2[2]-1)*rsd[2]^2)/(rt2[1]+rt2[2]-2))
    cohend<-unname(cohend)
    return(list(N=rt2,SD=rsd,t=rt,cohend=cohend))
  } 
}