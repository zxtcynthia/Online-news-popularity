#lasso logistic
library(glmnet)
set.seed(4650)
#names(traindata)

###########################################################
#### Linear Regression
###########################################################
names(traindata)
x.1=as.matrix(traindata[,c(-37,-42)])
y.1=log(traindata$shares)
grid=10^seq(3,-4,length=50)
cv.out.1=cv.glmnet(x.1,y.1,alpha=1,type.measure="mse",lambda=grid, nfolds=10) 
plot(cv.out.1)
cv.out.1
summary(cv.out.1)
bestlam=cv.out.1$lambda.min
bestlam
lasso.mod.1=glmnet(x.1,y.1,alpha=1,lambda=grid[38])
#lasso.mod=glmnet(x.1,y,alpha=1,lambda=bestlam)
a=coef(lasso.mod.1)
#a
summary(a)[-1,1]-1
traindata_lasso.1=traindata[,c(1,5,7,8,11,12,14,15,16,17,18,19,22,23,24,27,29,30,36,37,38,41)]
testdata_lasso.1=testdata[,c(1,5,7,8,11,12,14,15,16,17,18,19,22,23,24,27,29,30,36,37,38,41)]
#names(traindata_lasso)

pred.1=predict(lasso.mod.1,x.1)
actual.1 = y.1
mean((actua.1-pred.1)^2)
attach(traindata_lasso)
linearreg=lm(log(shares)~.,data=traindata_lasso.1)
summary(linearreg)

pred=predict(linearreg,testdata_lasso.1,type='response')
predict=ifelse(pred<=1400,0,1)
mean(predict==testdata$popularity)


###########################################################
#### Logistic Regression
###########################################################
x.2=as.matrix(traindata[,c(-37,-42)])
names(traindata)
y.2=traindata$popularity
grid=10^seq(3,-4,length=50)
cv.out.2=cv.glmnet(x.2,y.2,alpha=1,type.measure="mse",family="binomial",lambda=grid,nfolds=10) 
plot(cv.out.2)
cv.out.2$cvm
cv.out.2$lambda
bestlam=cv.out.2$lambda.min #value of lambda that gives minimum cvm
bestlam
lasso.mod.2=glmnet(x.2,y.2,alpha=1,family="binomial",lambda=grid[40])
#lasso.mod.2=glmnet(x.2,y.2,alpha=1,family="binomial",lambda=bestlam)
coef(lasso.mod.2)
pred.2=predict(lasso.mod.2,x.2)
actual.2 = y.2
mean((actual.2-pred.2)^2)

library(boot)
attach(traindata)
logreg=glm(popularity~n_tokens_title+n_unique_tokens+num_hrefs+num_videos+data_channel_is_lifestyle+
             data_channel_is_entertainment+data_channel_is_socmed+
             data_channel_is_tech+data_channel_is_world+kw_min_min+kw_max_max+kw_avg_avg+
             is_weekend+global_rate_positive_words+global_rate_negative_words+
             avg_positive_polarity+abs_title_sentiment_polarity+num_self_hrefs_sqrt+title_subjectivity_binary,
             data=traindata,family="binomial")
summary(logreg)

#predprob=predict(logreg,traindata,type='response')
#Threshold = rep(0,100)
#for (s in 1:100){
#  pred=rep(0,nrow(traindata))
#  pred[predprob>s/100]=1
#  Threshold[s]=mean(pred==traindata$popularity)
#}
#which.max(Threshold)
set.seed(4650)
predprob=predict(logreg,testdata,type='response')
pred=rep(0,nrow(testdata))
#pred[predprob>0.56]=1
pred[predprob>0.55]=1
#pred[predprob>0.53]=1
#pred[predprob>0.50]=1
#pred[predprob>0.50]=1
mean(pred==testdata$popularity)