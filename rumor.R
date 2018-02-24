library (dplyr)
library (psych)
library (car)
library (pequod)

d<-read.csv("data.csv")

#変数SEXは1が男性、2が女性なので、1にmale、2にfemaleのラベルを付ける
d$SEX<-factor(d$SEX,labels=c("male","female"))
d$CHILD<-factor(d$CHILD,labels=c("donthave","have"))
d$MARRIED<-factor(d$MARRIED,labels=c("dontmarried","married"))


#逆転項目処理
d$ConservativeBuying<-7-d$ConservativeBuying01r

summary(d)

d$Knowledge01A<-ifelse(d$QUIZ01==3,1,0)
d$Knowledge02A<-ifelse(d$QUIZ02==4,1,0)
d$Knowledge03A<-ifelse(d$QUIZ03==3,1,0)
d$Knowledge04A<-ifelse(d$QUIZ04==1,1,0)
d$Knowledge05A<-ifelse(d$QUIZ05==3,1,0)
d$Knowledge06A<-ifelse(d$QUIZ06==2,1,0)
d$Knowledge<-d$Knowledge01A+d$Knowledge02A+d$Knowledge03A+d$Knowledge04A+d$Knowledge05A+d$Knowledge06A
table(d$Knowledge)
hist(d$Knowledge, col="red", main="N. of correct answers", xlab="", breaks=c(0,1,2,3,4,5,6))
mean(d$Knowledge01A)
mean(d$Knowledge02A)
mean(d$Knowledge03A)
mean(d$Knowledge04A)
mean(d$Knowledge05A)
mean(d$Knowledge06A)

#14/10/2017実施 (社会心理学会用) 全部ぶっこんだ因子分析
F01<-dplyr::select(d,starts_with("Q"))
F01<-F01[c(1:34,40,41)]
VSS.scree(F01)
efa <- factanal(F01,factors=3,rotation="promax", scores = "Bartlett")  # 因子数と回転の指定
common <- 1-efa$uniqueness 
common   #共通性（因子抽出後）
print(efa, cutoff=0)

F02<-F01[c(1,3:9,13:21,23,24,26:33,35,36)]
VSS.scree(F02)
efa <- factanal(F02,factors=3,rotation="promax", scores = "Bartlett")  # 因子数と回転の指定
common <- 1-efa$uniqueness 
common   #共通性（因子抽出後）
print(efa, cutoff=0)

corx <- cor(F02)
eigen(corx)$values

source("http://aoki2.si.gunma-u.ac.jp/R/src/sort.loadings.R", encoding="euc-jp")
print(sort.loadings(efa),cutoff=0)

d$Food_Literacy<-efa$scores[,1]
d$Media_Literacy<-efa$scores[,2]
d$Health<-efa$scores[,3]

#望ましくない得点
d$Undesirable<-d$Undesirable_R+d$Undesirable_D
cor.test(d$Undesirable_R,d$Undesirable_D)
table(d$Undesirable)
hist(d$Undesirable)
t.test(d$Undesirable_R, mu=3.5)
t.test(d$Undesirable_D, mu=3.5)

#買い控え傾向得点

ConservativeBuying<-data.frame(d$ConservativeBuying01,d$ConservativeBuying02,d$ConservativeBuying03)
psych::alpha(ConservativeBuying)
d$ConservativeBuying<-d$ConservativeBuying01+d$ConservativeBuying02+d$ConservativeBuying03
hist(d$ConservativeBuying)
table(d$ConservativeBuying)

t.test(d$ConservativeBuying01r, mu=3.5)
t.test(d$ConservativeBuying02r, mu=3.5)
t.test(d$ConservativeBuying03, mu=3.5)

#責任
mean(d$Responsibility01)
sd(d$Responsibility01)
mean(d$Responsibility02)
sd(d$Responsibility02)
mean(d$Responsibility03)
sd(d$Responsibility03)


cor.test(d$Undesirable_R,d$ConservativeBuying01r)
cor.test(d$Undesirable_R,d$ConservativeBuying02r)
cor.test(d$Undesirable_R,d$ConservativeBuying03)

cor.test(d$Undesirable_D,d$ConservativeBuying01r)
cor.test(d$Undesirable_D,d$ConservativeBuying02r)
cor.test(d$Undesirable_D,d$ConservativeBuying03)

#標準化されていない変数を標準化 (factor型は不要。因子得点はすでに標準化されているのでそのまま)
d$ConservativeBuying_s<-scale(d$ConservativeBuying)
d$AGE_s<-scale(d$AGE)
d$Undesirable_s<-scale(d$Undesirable)
d$Knowledge_s<-scale(d$Knowledge)
d$AREAdis_s<-scale(d$AREAdis)

model01<-lmres(ConservativeBuying_s~SEX+AGE_s+AREAdis_s+CHILD+Undesirable_s+
                 Food_Literacy+Media_Literacy+Health+Knowledge_s,
               data=d)
summary(model01)

model02<-lmres(ConservativeBuying_s~SEX+AGE_s+AREAdis_s+CHILD+Undesirable_s+
                Food_Literacy+Media_Literacy+Health+Knowledge_s+
                Undesirable_s:Knowledge_s+Food_Literacy:Knowledge_s+Media_Literacy:Knowledge_s+Health:Knowledge_s,
              centered=c("Undesirable_s","Media_Literacy","Health","Knowledge_s"),
              data=d)
summary(model02)
model03<-simpleSlope(model02, pred="Knowledge_s", mod1 = "Undesirable_s")
summary(model03)
par(family="HiraKakuProN-W3") 
PlotSlope(model03, namex=c("知識得点"), namey=c("買い控え傾向"))
model02$F_change

model02a<-lmres(ConservativeBuying_s~SEX+AGE_s+AREAdis_s+CHILD+Undesirable_s+
                 Food_Literacy+Media_Literacy+Health+Knowledge_s+
                 Undesirable_s:Knowledge_s+Food_Literacy:Knowledge_s+Media_Literacy:Knowledge_s+Health:Knowledge_s,
               centered=c("Undesirable_s","Media_Literacy","Health","Knowledge_s"),
               data=d)
summary(model02a)
confint(model02a,level=0.95)

#態度と行動の矛盾


table (d$Undesirable,d$ConservativeBuying01r)
table (d$Undesirable,d$ConservativeBuying02r)
table (d$Undesirable,d$ConservativeBuying03)

table (d$ConservativeBuying02r)




















#16/5/2017実施


#相関係数
cor.test(d$Q6S1,d$Q6S2)
cor.test(d$Q6S1,d$Q6S2,method="spearman")
hist(d$Q6S1)
hist(d$Q6S2)



#望ましくない得点
d$Undesirable<-d$Q6S1+d$Q6S2
table(d$Undesirable)
hist(d$Undesirable)

#買い控え傾向得点
d$Q6S3<-7-d$Q6S3
d$Q6S4<-7-d$Q6S4
ConservativeBuying<-data.frame(d$Q6S3,d$Q6S4,d$Q6S5)
alpha(ConservativeBuying)
d$ConservativeBuying<-d$Q6S3+d$Q6S4+d$Q6S5
hist(d$ConservativeBuying)
table(d$ConservativeBuying)

cor.test(d$Quiz,d$ConservativeBuying, method="spearman")
cor.test(d$Quiz,d$ConservativeBuying)

cor.test(d$Undesirable,d$ConservativeBuying, method="spearman")


psych::describe(Q6)
psych::describe(Q7)
psych::describe(Q8)
psych::describe(Q9)

#平均を求める
mean (d$AGE)

#男女の数を求める
table (d$SEX)

sd (d$AGE)

hist(d$AGE)
table(d$AGE)

hist(d$AREA)
table(d$AREA)

hist(d$HINCOME)
table(d$HINCOME)
table(d$HINCOME,d$SEX)

plot(d$AGE,d$HINCOME, col="blue")

table(d$CHILD)

table (d$SEX,d$CHILD)
table (d$SEX,d$MARRIED)


#9/5/2017実施分


#dplyrとpsychパッケージを使う
psych::describe(d$Q6S1)

table(d$Quiz)
hist(d$Quiz)

cor.test(d$Quiz,d$Q6S1, method="spearman")
cor.test(d$Quiz,d$Q6S2, method="spearman")
cor.test(d$Quiz,d$Q6S3, method="spearman")
cor.test(d$Quiz,d$Q6S4, method="spearman")
cor.test(d$Quiz,d$Q6S5, method="spearman")
cor.test(d$Quiz,d$Q6S6, method="spearman")
cor.test(d$Quiz,d$Q6S7, method="spearman")
cor.test(d$Quiz,d$Q6S8, method="spearman")

Q6<-dplyr::select(d,starts_with("Q6"))
Q7<-dplyr::select(d,starts_with("Q7"))
Q8<-dplyr::select(d,starts_with("Q8"))
Q9<-dplyr::select(d,starts_with("Q9"))

#因子分析
VSS.scree(Q7)
efa <- factanal(Q7,factors=2,rotation="promax", scores = "regression")  # 因子数と回転の指定
common <- 1-efa$uniqueness 
common   #共通性（因子抽出後）
print(efa, cutoff=0)
source("http://aoki2.si.gunma-u.ac.jp/R/src/sort.loadings.R", encoding="euc-jp")
print(sort.loadings(efa), cutoff=0)

Q7<-Q7[c(1,3,5,6,7,8,9)]
efa <- factanal(Q7,factors=2,rotation="promax", scores = "regression")  # 因子数と回転の指定
common <- 1-efa$uniqueness 
common   #共通性（因子抽出後）
source("http://aoki2.si.gunma-u.ac.jp/R/src/sort.loadings.R", encoding="euc-jp")
print(sort.loadings(efa), cutoff=0)

d$FL_Info<-efa$scores[,1]
d$FL_Foreign<-efa$scores[,2]

Q7_FL_Info<-data.frame(d$Q7S1,d$Q7S3,d$Q7S5,d$Q7S6,d$Q7S7)
alpha(Q7_FL_Info)
Q7_FL_Foreign<-data.frame(d$Q7S8,d$Q7S9)
alpha(Q7_FL_Foreign)


VSS.scree(Q8)
efa <- factanal(Q8,factors=3,rotation="promax", scores = "regression")  # 因子数と回転の指定
common <- 1-efa$uniqueness 
common   #共通性（因子抽出後）
source("http://aoki2.si.gunma-u.ac.jp/R/src/sort.loadings.R", encoding="euc-jp")
print(sort.loadings(efa), cutoff=0)

Q8<-Q8[c(1,2,3,4,5,6,7,8,9,10,13,14,15,16,17,18)]
VSS.scree(Q8)
efa <- factanal(Q8,factors=3,rotation="promax", scores = "regression")  # 因子数と回転の指定
common <- 1-efa$uniqueness 
common   #共通性（因子抽出後）
source("http://aoki2.si.gunma-u.ac.jp/R/src/sort.loadings.R", encoding="euc-jp")
print(sort.loadings(efa), cutoff=0)

Q8<-Q8[c(1:15)]
VSS.scree(Q8)
efa <- factanal(Q8,factors=3,rotation="promax", scores = "regression")  # 因子数と回転の指定
common <- 1-efa$uniqueness 
common   #共通性（因子抽出後）
source("http://aoki2.si.gunma-u.ac.jp/R/src/sort.loadings.R", encoding="euc-jp")
print(sort.loadings(efa), cutoff=0)

Q8<-Q8[c(1:14)]
VSS.scree(Q8)
efa <- factanal(Q8,factors=3,rotation="promax", scores = "regression")  # 因子数と回転の指定
common <- 1-efa$uniqueness 
common   #共通性（因子抽出後）
source("http://aoki2.si.gunma-u.ac.jp/R/src/sort.loadings.R", encoding="euc-jp")
print(sort.loadings(efa), cutoff=0)

Q8<-Q8[c(1:10,12,13)]
VSS.scree(Q8)
efa <- factanal(Q8,factors=3,rotation="promax", scores = "regression")  # 因子数と回転の指定
common <- 1-efa$uniqueness 
common   #共通性（因子抽出後）
source("http://aoki2.si.gunma-u.ac.jp/R/src/sort.loadings.R", encoding="euc-jp")
print(sort.loadings(efa), cutoff=0)

Q8<-Q8[c(1:11)]
VSS.scree(Q8)
efa <- factanal(Q8,factors=3,rotation="promax", scores = "regression")  # 因子数と回転の指定
common <- 1-efa$uniqueness 
common   #共通性（因子抽出後）
source("http://aoki2.si.gunma-u.ac.jp/R/src/sort.loadings.R", encoding="euc-jp")
print(sort.loadings(efa), cutoff=0)

d$ML_Criticism<-efa$scores[,1]
d$ML_Accept<-efa$scores[,2]
d$ML_Influence<-efa$scores[,3]

Q8_ML_Criticism<-data.frame(d$Q8S8,d$Q8S9,d$Q8S10)
alpha(Q8_ML_Criticism)
Q8_ML_Accept<-data.frame(d$Q8S1,d$Q8S2,d$Q8S3,d$Q8S4,d$Q8S14)
alpha(Q8_ML_Accept)
Q8_ML_Influence<-data.frame(d$Q8S5,d$Q8S6,d$Q8S7)
alpha(Q8_ML_Influence)

VSS.scree(Q9)
efa <- factanal(Q9,factors=4,rotation="promax", scores = "regression")  # 因子数と回転の指定
common <- 1-efa$uniqueness 
common   #共通性（因子抽出後）
source("http://aoki2.si.gunma-u.ac.jp/R/src/sort.loadings.R", encoding="euc-jp")
print(sort.loadings(efa), cutoff=0)

#Q9<-Q9[c(1,2,3,4,5,6,8,9,10,11,12,13,14)]
#efa <- factanal(Q9,factors=4,rotation="promax", scores = "regression")  # 因子数と回転の指定
#common <- 1-efa$uniqueness 
#common   #共通性（因子抽出後）
#source("http://aoki2.si.gunma-u.ac.jp/R/src/sort.loadings.R", encoding="euc-jp")
#print(sort.loadings(efa), cutoff=0)

d$FF_Safety<-efa$scores[,1]
d$FF_Regular<-efa$scores[,2]
d$FF_Diet<-efa$scores[,3]
d$FF_Info<-efa$scores[,4]

Q9_FF_Safery<-data.frame(Q9$Q9S1,Q9$Q9S2,Q9$Q9S3,Q9$Q9S4,Q9$Q9S5,Q9$Q9S6,Q9$Q9S7)
alpha(Q9_FF_Safery)
Q9_FF_Regular<-data.frame(Q9$Q9S10,Q9$Q9S11,Q9$Q9S12)
alpha(Q9_FF_Regular)
Q9_FF_Diet<-data.frame(Q9$Q9S8,Q9$Q9S9)
alpha(Q9_FF_Diet)
Q9_FF_Info<-data.frame(Q9$Q9S13,Q9$Q9S14)
alpha(Q9_FF_Info)


hist(d$ML_Message)


mean(d$Q6S1)
sd(d$Q6S1)
mean(d$Q6S2)
sd(d$Q6S2)
mean(d$Q6S3)


#標準化されていない変数を標準化 (factor型は不要。因子得点はすでに標準化されているのでそのまま)
d$ConservativeBuying_a<-scale(d$ConservativeBuying)
d$AGE_a<-scale(d$AGE)
d$Undesirable_a<-scale(d$Undesirable)
d$Quiz_a<-scale(d$Quiz)
d$AREAdis_a<-scale(d$AREAdis)

reg<-lm(d$ConservativeBuying_a~d$SEX+d$AGE_a+d$AREAdis_a+d$CHILD+d$Undesirable_a+d$FL_Info+
          d$FL_Foreign+d$ML_Criticism+d$ML_Accept+d$ML_Influence+d$FF_Safety+d$FF_Info+d$Quiz_a)
summary(reg)

confint(reg,level=0.95)

library(car)
vif<-data.frame(vif(reg))
vif






