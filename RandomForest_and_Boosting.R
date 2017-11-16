rm(list=ls(all=TRUE))
setwd("/Users/siliwang/Desktop/职业/MSOR/2016 Fall/Business Analytics/Final project")
mydata<-read.csv("finaldata.csv")

#Drop insignificant explanatory variables
 drops<-c("n_non_stop_words","kw_min_min","kw_max_min","kw_avg_min","kw_min_max","kw_max_max",
         "kw_avg_max","kw_min_avg","kw_max_avg","rate_positive_words","rate_negative_words",
         "min_positive_polarity","max_negative_polarity","abs_title_subjectivity",
         "abs_title_sentiment_polarity","weekday_is_monday","weekday_is_tuesday","weekday_is_wednesday",
         "weekday_is_thursday","weekday_is_friday","weekday_is_saturday","weekday_is_sunday",
         "url","timedelta")
mydata<-mydata[,!(colnames(mydata) %in% drops)]

#Divide data into 50% training set, 25% validation set, and 25% test set
set.seed(4650)
mydata$popularity=ifelse(mydata$shares<=1400,0,1)
trainrows=sample(1:nrow(mydata), 0.50*nrow(mydata))
totalrows=seq(1:38178)
val_and_test=setdiff(totalrows,trainrows)
valrows=sample(val_and_test,0.25*nrow(mydata))
testrows=setdiff(val_and_test,valrows)

traindata=mydata[trainrows,]
valdata=mydata[valrows,]
testdata=mydata[testrows,]

#Random Forest
library(randomForest)
#Parameter Tuning
correctpred=rep(0,31)
for (i in 1:31){
bag.tree=randomForest(as.factor(popularity)~.-shares,data = traindata, mtry=i,importance=TRUE)
pop.bag=predict(bag.tree,newdata=valdata)
correctpred[i]=mean(pop.bag==valdata$popularity)
print(i)
} 
which.max(correctpred)

means=rep(0,5)
for (j in 1:5){
  bag.tree=randomForest(as.factor(popularity)~.-shares,data = rbind(traindata,valdata), mtry=which.max(correctpred),
                        importance=TRUE)
  pop.bag=predict(bag.tree,newdata=testdata)
  means[j]=mean(pop.bag==testdata$popularity)
  print(j)
}
mean(means)
sd(means)

#Boosting
library(gbm)
library(caret)
set.seed(4650)
boost.tree=gbm(popularity~.-shares,data = traindata,distribution = "bernoulli",
               n.trees = 5000,interaction.depth = 4)
pop.boosting=predict(boost.tree,newdata = testdata,n.trees = 5000)
boost.pred=ifelse(pop.boosting>0,1,0)
mean(boost.pred==testdata$popularity)





