setwd("C:/Users/Sherry/Desktop/Statistics/Stats780/FinalProject")

library(arules)
source("std_lift.R")
library(Ecdat)
library(tidyverse)
library(Rmisc)
library(e1071)
library(caret)
library(randomForest)
library(gbm)
library(ModelMetrics)
library(mccr)
library(reshape)
library(plyr)
options(max.print = 10000)
data(Fair)
set.seed(93)

#####Data Preprocessing

#Convert variables to factors 
Fair$ym <-as.factor(Fair$ym)
Fair$education <-as.factor(Fair$education)
Fair$occupation <-as.factor(Fair$occupation)
Fair$rate <- as.factor(Fair$rate)
Fair$religious<-as.factor(Fair$religious)

#Categorize ppl who cheated as Yes and No otherwise 
cheat <- as.factor(ifelse(Fair$nbaffairs == 0,"No","Yes"))
Fair <- data.frame(Fair,cheat)

#Recode variables for clarity


#Cleaned up data
Fair1 <- Fair[,-9]
str(Fair1)

#Stratafied Training and Test Set, ensure same proption of Yes and No in sets
inTraining <- createDataPartition(Fair1$cheat, p=0.75, list=FALSE)
train <-Fair1[inTraining,]
test <-Fair1[-inTraining,]

#####Exploratory stuff
c1 <- count(Fair1, vars=c('sex'))
freq <-list()
for (i in c(3:9)){
    freq[[i]] <- count(Fair1, vars=c(colnames(Fair1)[i]))
}


#Frequency
c1
summary(Fair1$age)
freq[[3]]
freq[[4]]
freq[[5]]
freq[[6]]
freq[[7]]
freq[[8]]
freq[[9]]



graph.data <-Fair1

#Recode for clarity
levels(graph.data$religious) <- c("Anti","NotatAll","Slightly","Somewhat","Very")
levels(graph.data$education) <- c('NoHS','HS','SomeUni','Uni', 'SomeGrad', 'Masters','PhD/Adv')
levels(graph.data$ym) <- c('0-3mnths','4-6mnths','6mnths-1yr','1-2yrs','3-5yrs','6-8yrs','9-11yrs','12plus')
levels(graph.data$occupation) <- c('Jobless','Unskilled','Semi','Skilled','Clerical','SemiPro','Pro')
levels(graph.data$rate) <- c('Miserable','Bad','Satisfied','Happy','Very Happy')
levels(graph.data$child) <-c("NoChildren","Children")

#Distribution of affairs
dist <- ggplot(Fair, aes(nbaffairs)) + geom_bar(color="black", fill="#0072B2") + 
  labs(title="Distribution of Affairs", x="Number of Affairs") + 
  theme_bw()

sex1 <- ggplot(graph.data,aes(cheat, fill=sex)) + geom_bar(position="dodge2") +
  theme_bw() + scale_fill_manual(values=c("forestgreen","orange")) + labs(title="Distribution of Genders")

multiplot(dist,sex1)

#Relgious and rate together 
g <- ggplot(graph.data, aes(cheat, fill=religious)) + geom_bar(position = 'dodge2') + 
  facet_grid(.~rate) + theme_bw() + scale_fill_brewer(palette = "RdBu")
g <- g + labs(title='Rate of Marriage and Religion')

#maybe some correlation between occupation and education. more prestiage and higher education more likely to cheat
b <- ggplot(graph.data, aes(cheat, fill=education)) + geom_bar(position="dodge2") + facet_grid(.~ occupation) 
b <- b + labs(title="Occupation vs Education") + theme_bw() + scale_fill_brewer(palette = "RdBu")

multiplot(g,b)

#Age
tt <-ggplot(graph.data, aes(age, fill=cheat)) + geom_bar() + theme_bw() + scale_fill_brewer(palette = "Set1")
tt <-tt + labs(title="Age")

#look at years married + having children
t <-ggplot(graph.data, aes(factor(ym), fill=cheat)) + geom_bar(position="dodge2") + facet_grid(.~child)
t <- t + theme_bw() +  scale_fill_manual(values = c("forestgreen","orange")) + labs(x="Years Married") + theme_bw()
t <- t + labs(title="Years Married Vs Having Children")

multiplot(tt,t)

#suspect that age will not contribute to cheating (similar distribution)



####Association Rules 

#Discretize age variable
Fair3 <- Fair1 #Dataset used for Association rules only
Fair3$age <- discretize(Fair3$age, method="frequency",breaks=3) #equal frequency 

#Cheat = No
params<-list(support=0.03,
             confidence=0.8,minlen=2,maxlen=10)
app<-list(rhs=c("cheat=No"),default="lhs")
fitt1<-apriori(Fair3,parameter=params, appearance = app)
quality(fitt1) <-std_lift(fitt1,Fair3)
inspect(sort(fitt1, by="slift"))

#Cheat = Yes (need smaller support, smaller sample)
params<-list(support=0.005,
             confidence=0.8,minlen=2,maxlen=10)
app<-list(rhs=c("cheat=Yes"),
          default="lhs")
fitt2<-apriori(Fair3,parameter=params,appearance = app)
quality(fitt2) <-std_lift(fitt2,Fair3)
inspect(sort(fitt2, by="slift"))


####Trees

#Custom function to be used in trainControl() in caret 

#######
library(mccr)
myfunction2 <- function(data,lev=NULL,model=NULL)
{
  lvls <-levels(data$obs)
  if (!all(levels(data[, "pred"]) == lvls))
    stop("levels of observed and predicted data do not match")
  crand <- unlist(e1071::classAgreement(table(data[,"obs"],data[,"pred"])))[4]
  
  data1 <-data[complete.cases(data), ]
  data1$obs <-ifelse(data1$obs==data$obs[1],0,1)
  data1$pred <-ifelse(data1$pred==data$obs[1],0,1)
  mcc <- mccr::mccr(data1$obs,data1$pred)

  
  if (is.null(lev)) 
    stop("'lev' cannot be NULL")
  if (!all(lev %in% colnames(data))) 
    stop("'data' should have columns consistent with 'lev'")
  if (!all(sort(lev) %in% sort(levels(data$obs)))) 
    stop("'data$obs' should have levels consistent with 'lev'")
  dataComplete <- data[complete.cases(data), ]
  probs <- as.matrix(dataComplete[, lev, drop = FALSE])
  logLoss <- ModelMetrics::mlogLoss(dataComplete$obs, probs)
  

  out <-c(crand,logLoss,mcc)
  
  names(out) <-c("ARI","logLoss","mcc")
  out
}
######## Random Forest + Bagging
#Tune 'mtry' and 'ntree'
models <- list()
for (ntree in c(500,1000,1500,2000,2500) ){
control <-trainControl(method='cv',number=5,classProbs = TRUE,summaryFunction = myfunction2)

bag.grid = expand.grid(mtry=c(1,2,3,4,5,6,7,8))

rf <- train(cheat ~ ., data=train,
             method="rf",
             trControl=control,
             ntree=ntree,
             metric='logLoss',  #metric can be changed to "ARI", "mcc", "logLoss"
             tuneGrid=bag.grid)
key <- toString(ntree)
models[[ntree]] <- rf
}

rf1 <- models[[500]]$results
models[[500]]$bestTune

rf2<-models[[1000]]$results
models[[1000]]$bestTune

rf3<-models[[1500]]$results
models[[1500]]$bestTune

rf4<-models[[2000]]$results
models[[2000]]$bestTune

rf5 <-models[[2500]]$results
models[[2500]]$bestTune

#logloss
rf.data<-data.frame(rf1$mtry,rf1$logLoss,rf2$logLoss,rf3$logLoss,rf4$logLoss,rf5$logLoss)
colnames(rf.data)<-c('mtry','500','1000','1500','2000','2500')

rf.data.melt <-melt(rf.data,id.vars='mtry')
colnames(rf.data.melt)[c(2,3)] <-c('ntrees','logLoss')


logloss <- ggplot(rf.data.melt, aes(x=mtry,y=logLoss,col=ntrees))+ 
  geom_line() + geom_point() + theme_bw() + labs(title="Tuning RF based on logLoss")


#ARI
rf.data1 <-data.frame(rf1$mtry, rf1$ARI,rf2$ARI,rf3$ARI,rf4$ARI,rf5$ARI)                    
colnames(rf.data1)<-c('mtry','500','1000','1500','2000','2500')
rf.data1.melt <-melt(rf.data1,id.vars='mtry')
colnames(rf.data1.melt)[c(2,3)] <-c('ntrees','ARI')


ari <-ggplot(rf.data1.melt, aes(x=mtry, y=ARI,col=ntrees)) +
  geom_line() + geom_point() + theme_bw() + labs(title="Tuning RF based on ARI")


#mcc
rf.data2<-data.frame(rf1$mtry,rf1$mcc,rf2$mcc,rf3$mcc,rf4$mcc,rf5$mcc)    
colnames(rf.data2)<-c('mtry','500','1000','1500','2000','2500')
rf.data2.melt <-melt(rf.data2,id.vars='mtry')
colnames(rf.data2.melt)[c(2,3)] <-c('ntrees','mcc')


mcc <-ggplot(rf.data2.melt, aes(x=mtry, y=mcc,col=ntrees)) +
  geom_line() + geom_point() + theme_bw() + labs(title="Tuning RF based on MCC")

multiplot(logloss,ari,mcc)


### From plots, test ntree=2500 test mtry=3,6,8
rfm3 <- randomForest(cheat ~.,data=train,mtry=3,ntree=2500)
rfm3.pred <-predict(rfm3,newdata=test,type="response")

rfm3.data <-data.frame(factor(test$cheat),factor(rfm3.pred))
levels(rfm3.data$factor.test.cheat)<-c("0","1")
levels(rfm3.data$factor.rfm3.pred)<-c("0","1")

colnames(rfm3.data) <-c("obs","pred","obs1","pred1")


rfm3.table <-table(rfm3.data$obs,rfm3.data$pred)
classAgreement(rfm3.table)[4]

mccr(rfm3.data$obs1,rfm3.data$pred1)


#### mtry=6, ntree=2500
rfm6 <- randomForest(cheat ~.,data=train,mtry=6,ntree=2500)
rfm6.pred <-predict(rfm6,newdata=test,type="response")

rfm6.data <-data.frame(factor(test$cheat),factor(rfm6.pred))
levels(rfm6.data$factor.test.cheat)<-c("0","1")
levels(rfm6.data$factor.rfm6.pred)<-c("0","1")

colnames(rfm6.data) <-c("obs","pred","obs1","pred1")


rfm6.table <-table(rfm6.data$obs,rfm6.data$pred)
classAgreement(rfm6.table)[4]

mccr(rfm6.data$obs1,rfm6.data$pred1)

####mtry=8, ntree=2500 aka bagging
rfm8 <- randomForest(cheat ~.,data=train,mtry=6,ntree=2500)

rfm8.pred <-predict(rfm8,newdata=test,type="response")

rfm8.data <-data.frame(factor(test$cheat),factor(rfm8.pred))
levels(rfm8.data$factor.test.cheat)<-c("0","1")
levels(rfm8.data$factor.rfm8.pred)<-c("0","1")

colnames(rfm8.data) <-c("obs","pred","obs1","pred1")


rfm8.table <-table(rfm8.data$obs,rfm8.data$pred)
classAgreement(rfm8.table)[4]

mccr(rfm8.data$obs1,rfm8.data$pred1)

##try mtry=8, ntrees=1500 for bagging (all 3 plots reach same conclusion)
bagging <- randomForest(cheat ~.,data=train,mtry=8,ntree=1500)

bagging.pred <-predict(bagging,newdata=test,type="response")

bagging.data <-data.frame(factor(test$cheat),factor(bagging.pred))
levels(bagging.data$factor.test.cheat)<-c("0","1")
levels(bagging.data$factor.bagging.pred)<-c("0","1")

colnames(bagging.data) <-c("obs","pred","obs1","pred1")


bagging.table <-table(bagging.data$obs,bagging.data$pred)
classAgreement(bagging.table)[4]

mccr(bagging.data$obs1,bagging.data$pred1)

###Boosting 
control <-trainControl(method='cv',number=5,classProbs = TRUE,summaryFunction = myfunction2)

gbmGrid <-expand.grid(interaction.depth=c(1,2,3,4),
                      n.trees=c(500,1000,1500,2000,2500),
                      shrinkage=c(0.01,0.1,0.2),
                      n.minobsinnode=10)

gbmCv <-train(cheat ~., data=train,
              method="gbm",
              trControl=control,
              verbose=FALSE,
              metric='logLoss',
              tuneGrid=gbmGrid)

gbmCv$bestTune


trellis.par.set(caretTheme())
gbm.log <- ggplot(gbmCv,metric='logLoss',nameInStrip = TRUE) + theme_bw() + labs(title = 'Boosting based on logLoss')
gbm.ari <- ggplot(gbmCv,metric='ARI',nameInStrip = TRUE) + theme_bw() + labs(title = 'Boosting based on ARI')
gbm.mcc <- ggplot(gbmCv,metric='mcc',nameInStrip = TRUE) + theme_bw() + labs(title = 'Boosting based on mcc')

multiplot(gbm.log,gbm.ari,gbm.mcc)

###Testing
#ntree=500,interaction=2,shrinkage=0.01
model.1 <- gbm(cheat ~., data=test,distribution='multinomial',
               n.tree=500,
               interaction.depth = 2,
               shrinkage = 0.01)



summary(model.1) #relative influence 

boost.pred <- predict(model.1,data=test,n.tree=500,type='response')
df <-data.frame(boost.pred) ; colnames(df)[1] <- '0';colnames(df)[2] <-'1' #put predictions into dataframe
classify <-colnames(df)[max.col(df,ties.method = 'first')]
df <-data.frame(df,classify) ; df$classify <-as.factor(classify) 

boost.1 <-table(test$cheat,df$classify)
mccr(test$cheat,df$classify)  #0.72

#ntree1500,interaction=1,shrinkage=0.1
model.2 <- gbm(cheat ~., data=test,distribution='multinomial',
               n.tree=1500,
               interaction.depth = 1,
               shrinkage = 0.1)



x<- summary(model.2) #relative influence 

boost.pred1 <- predict(model.2,data=test,n.tree=1500,type='response')
df1 <-data.frame(boost.pred1) ; colnames(df1)[1] <- '0';colnames(df1)[2] <-'1' #put predictions into dataframe
classify1 <-colnames(df1)[max.col(df1,ties.method = 'first')]
df1 <-data.frame(df1,classify1) ; df1$classify1 <-as.factor(classify1) 

boost.2 <-table(test$cheat,df1$classify1)
mccr(test$cheat,df$classify1)  #0 


y <- varImpPlot(rfm3)
importance(rfm3)

par(mfrow=c(1,2))
varImpPlot(rfm3)
summary(model.2) #relative influence 


