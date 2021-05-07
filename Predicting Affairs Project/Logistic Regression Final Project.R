setwd("C:/Users/Sherry/Desktop/Statistics/Stats780")
library(Ecdat)
library(caret)
library(dplyr)
library(e1071)
library(mccr)
data('Fair')
set.seed(93)
#####Data Preprocessing

#Convert variables to factors 
Fair$ym <-as.factor(Fair$ym)
Fair$education <-as.factor(Fair$education)
Fair$occupation <-as.factor(Fair$occupation)
Fair$rate <- as.factor(Fair$rate)
Fair$religious<-factor(Fair$religious)

#Categorize ppl who cheated as Yes and No otherwise 
cheat <- as.factor(ifelse(Fair$nbaffairs == 0,"No","Yes"))
Fair <- data.frame(Fair,cheat)

#Cleaned up data
Fair1 <- Fair[,-9]

#Copy data
Fair2 <- Fair1


str(Fair1)
head(Fair1)

#Stratafied Training and Test Set, ensure same proption of Yes and No in sets
inTraining <- createDataPartition(Fair1$cheat, p=0.75, list=FALSE)
train <-Fair1[inTraining,]
test <-Fair1[-inTraining,]

table(train$cheat) ; table(test$cheat)

####Logistic Regression 
#### Full Model 
full.model <- glm(data=train, cheat ~ ., family=binomial)
summary.full <- summary(full.model)
summary.full

#G= null deviance - residual deviance of full model 
G=508.35 - 412.40 
pchisq(G,30,lower.tail = FALSE) ##reject HO 


####Reduced Model 
reduced.1 <- glm(data=train, cheat ~ age + religious + occupation + rate, family=binomial )
summary(reduced.1)
## By Wald Stats, with the exception of the age, all SS.
  
##Remove Age 
reduced.2 <-glm(data=train, cheat ~ religious + occupation + rate, family=binomial)
summary(reduced.2)

#compare model w/o age and age 
#H0: age = 0 
G2 = 454.87 - 453.50
pchisq(G2,1,lower.tail=FALSE) #p > alpha=0.05, fail to reject H0 so age = 0, take reduced model 2

#Remove occupation 
reduced.3 <-glm(data=train, cheat ~ religious + rate, family=binomial)
summary(reduced.3)

#compare w/o occupation and with 
G3 = 465.01- 454.87 
pchisq(G3,6,lower.tail = FALSE) #reduced model is better 

###### testing with reduced.3 model (cheat ~ religious + rate) 
coef1 <-round(summary(reduced.3)$coef,4)

prob1 <-predict(reduced.3,newdata=test,type="response")
predictions1 <-ifelse(prob <0.50, "0", "1")
tab1 <-table(test$cheat,predictions1)
tab1
classAgreement(tab1)[4]
levels(test$cheat) <-c("0","1")
mccr(test$cheat,predictions)


#Odd Ratios 
or <-cbind(round(exp(reduced.3$coefficients),4))

#Confidence intervals for OR
upper.ci <- round(exp(coef1[,1] + 1.96*coef1[,2]),4)
lower.ci <-round(exp(coef1[,1] - 1.96*coef1[,2]),4)

results1 <- data.frame(coef1, or, lower.ci, upper.ci)

#wide ci- problems with surveys maybe lots of false answers but indicate unreliable 
#unreliability of samples 

########Collpasing Variables 
## Religious 3-levels (not, somewhat and very religious (1,2,3))
## Occupation 3 levels (unskilled, skilled, very skilled (1,2,3))
## Rate of marriage 2 levels (not happy, satisfied, happy (1,2,3))
levels(Fair2$religious) <- list("1" = c("1"),"2"=c("2,3"),"3"=c("4","5"))
levels(Fair2$occupation) <-list("1" = c("1","2"), "2"=c("3","4"), "3"=c("5","6","7"))
levels(Fair2$rate) <-list("1"=c("1","2"),"2"=c("3"),"3"=c("4","5"))

str(Fair2)

#Copy train/test
train1 <-Fair2[inTraining,]
test1 <-Fair2[-inTraining,]


#Remove occupation and collapse not significant
####FINAL MODEL
reduced.4 <-glm(data=train1, cheat ~ religious + rate, family=binomial)
summary(reduced.4)
coef <- round(summary(reduced.4)$coef,4)

#compare model without/with collapsing
G4 <- 472.94 - 465.01
pchisq(G2,2, lower.tail=FALSE) #p > alpha so take reduced.4 as final model 

###Test with reduced.4 model
prob <-predict(reduced.4,newdata=test1,type="response")
predictions <-ifelse(prob <0.50, "0", "1")
tab <-table(test1$cheat,predictions)
tab
levels(test1$cheat) <-c("0","1")

mccr(test1$cheat,predictions)


#Odd Ratios 
or <-cbind(round(exp(reduced.4$coefficients),4))

#Confidence intervals for OR
upper.ci <- round(exp(coef[,1] + 1.96*coef[,2]),4)
lower.ci <-round(exp(coef[,1] - 1.96*coef[,2]),4)

results2 <- data.frame(coef,or,lower.ci,upper.ci)
#Give the estimated log it expression on report 
#Improved results and interpret odd ratios (slightly narrower)


results2 