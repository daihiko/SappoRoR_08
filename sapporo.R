d<-read.csv("data.csv", head=TRUE)

summary (d)

by (d, d$SEX, function(d) mean(d$AGE, na.rm=TRUE))
tapply(d$AGE, d$SEX, mean, na.rm=TRUE)

str(d)
names(d)

d$ID<-factor(d$ID)
d$SEX<-factor(d$SEX, labels=list("male", "female"))
d$MARRIED<-factor(d$MARRIED,labels=c("dontmarried","married"))
tapply(d$AGE, d$SEX, mean, na.rm=TRUE)
tapply(d$AGE, d$SEX, sd, na.rm=TRUE)

d$FoodLiteracy_all<-d$FoodLiteracy01+d$FoodLiteracy02+
  d$FoodLiteracy03+d$FoodLiteracy04+d$FoodLiteracy05+
  d$FoodLiteracy06+d$FoodLiteracy07+d$FoodLiteracy08+
  d$FoodLiteracy09

hist(d$FoodLiteracy_all)

d$Quiz01A<-ifelse(d$Quiz01==3,1,0)
d$Quiz02A<-ifelse(d$Quiz02==4,1,0)
d$Quiz03A<-ifelse(d$Quiz03==3,1,0)
d$Quiz04A<-ifelse(d$Quiz04==1,1,0)
d$Quiz05A<-ifelse(d$Quiz05==3,1,0)
d$Quiz06A<-ifelse(d$Quiz06==2,1,0)
d$Quiz<-d$Quiz01A+d$Quiz02A+d$Quiz03A+d$Quiz04A+
  d$Quiz05A+d$Quiz06A
hist(d$Quiz)

plot(d$Quiz,d$FoodLiteracy_all)

library("ggplot2")
ggplot(d,aes(x=Quiz, y=FoodLiteracy_all, colour=SEX))
+ geom_jitter(size=2)

cor.test(d$Quiz,d$FoodLiteracy_all)
cor.test(d$Quiz,d$FoodLiteracy_all,method="spearman")

library(psych)
corrvar<-data.frame(d$AGE,d$AREAdis,d$ConservatibeBuying,
                    d$Undesirable,d$FoodLiteracy_all,d$Quiz)
corr.test(corrvar)

library(corrplot)
corplot1<-cor(corrvar)
corrplot(corplot1)

result_t<-t.test(d$Quiz~d$SEX)
result_t
result_sd<-tapply(d$Quiz, d$SEX, sd, na.rm=TRUE)
result_sd
result_table<-table(d$SEX)
library(compute.es)
mes(result_t$estimate[1],result_t$estimate[2],result_sd[1],
    result_sd[2],result_table[1],result_table[2])


#Self defined function (22/2/2018, There may be some bugs.)

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
    cohend<-(rt$estimate[1]-rt$estimate[2])/sqrt(((rt2[1]-1)*rsd[1]^2+(rt2[2]-1)*rsd[2]^2)
                                                 /(rt2[1]+rt2[2]-2))
    cohend<-unname(cohend)
    return(list(N=rt2,SD=rsd,t=rt,cohend=cohend))
  } 
}

daihiko_t(d$Quiz,d$SEX,paired=FALSE)
daihiko_t(d$FoodLiteracy01,d$FoodLiteracy02,paired=TRUE)


