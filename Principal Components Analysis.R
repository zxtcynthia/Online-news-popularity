rm(list=ls(all=TRUE))
mydata = read.csv(file="E:/我的文件夹/哥大MSOR/Business Analytics/Project/FinalData.csv")
#Transfrom data
#Log transformation:
attach(mydata)
mydata$n_tokens_content<-log(n_tokens_content)
mydata$num_hrefs<-ifelse(num_hrefs>0,log(num_hrefs),0)
mydata$num_self_hrefs<-ifelse(num_self_hrefs>0,log(num_self_hrefs),0)
#Sqrt Transformation:
mydata$num_self_hrefs_sqrt<-sqrt(num_self_hrefs)
mydata$global_rate_positive_words<-sqrt(global_rate_positive_words)
mydata$global_rate_negative_words<-sqrt(global_rate_negative_words)
#Binary:
mydata$num_imgs<-ifelse(num_imgs>0,1,0)
mydata$num_videos<-ifelse(num_videos>0,1,0)
mydata$kw_max_max<-ifelse(kw_max_max>=843300,1,0)
#Take out outliers:
mydata<-mydata[!(n_tokens_title>=15 
                 |n_non_stop_unique_tokens<0.3 
                 |average_token_length>6 
                 |global_rate_positive_words>0.11
                 |avg_negative_polarity< -0.8),]
mydata$popularity=ifelse(mydata$shares<=1400,0,1)
drops<-c("timedelta","url")
mydata<-mydata[,!(colnames(mydata) %in% drops)]

colnames(mydata)
x=mydata[,-59]
x=x[,-54]

#scale data,compute principal components
data.scaled=scale(x)
pca.out=prcomp(data.scaled)
p=pca.out$rotation
sort(p[,1])
sort(p[,2])
sort(p[,3])
xmatrix=data.matrix(data.scaled)
newdata=data.frame(xmatrix %*% p)
newdata$popularity=mydata$popularity
#proportion of variance explained
pca.var=pca.out$sdev^2
pve=pca.var/sum(pca.var)
pve
plot(cumsum(pve),type="b")
#plots
par(mfrow=c(1,2))
plot(pve , xlab=" Principal Component ", ylab=" Proportion of Variance Explained ", 
     ylim=c(0,1) ,type="b")
plot(cumsum (pve), xlab=" Principal Component ", ylab ="Cumulative Proportion of Variance Explained ", 
     ylim=c(0,1), type="b")
par(mfrow=c(1,1))

##########################################
# Principal Component Regression (PCR)
#install.packages("pls")
library(pls)
# Make training and test sets
set.seed(4650)
train=sample(1:nrow(mydata),0.75*nrow(mydata))
test=-train
y.test=mydata[test,]$shares

#pcr
pcr.fit = pcr(log(shares)~.-popularity, data= mydata, subset=train, scale=T, 
              validation = "CV", segments=10)
summary(pcr.fit)
# plot
validationplot(pcr.fit,val.type="MSEP",main="PCR")

#obtain predictions using # comps = 30
x = model.matrix(popularity~.-shares, mydata)[,-1]
pcr.pred = predict(pcr.fit, x[test,], ncomp=30)
pcr.pred
correction=rep(0,length(test))
correction=ifelse(exp(pcr.pred)<=1400,0,1)
mean(correction==mydata[test,]$popularity)
#49.58

##########################################
# Partial Least Squares
pls.fit = plsr(log(shares)~.-popularity, data=mydata, subset=train, scale=T, 
               validation ="CV",segments=10)
summary(pls.fit)
#plot
validationplot(pls.fit,val.type="MSEP",main="PLS")

#obtain predictions using # comps = 5
pls.pred = predict(pls.fit,mydata[test,],ncomp=5)
correction=rep(0,length(test))
correction=ifelse(exp(pls.pred)<=1400,0,1)
mean(correction==mydata[test,]$popularity)
#49.60

##########################################
#PCR logistic
#divide data
set.seed(4650)
train0=sample(1:nrow(mydata),0.75*nrow(mydata))
num.train= floor(2/3*length(train0))
train = train0[1:num.train]
valid = train0[(num.train+1):length(train0)]
test=-train0
#choose best number of components using validation
lograte=rep(0,13)
n=0
for (i in c(2,3,5,7,10,15,20,25,30,35,40,45,50)){
  n=n+1
  new=newdata[,1:i]
  new$popularity=newdata$popularity
  logfit=glm(popularity~.,data=new[train,],family="binomial")
  predprob=predict(logfit,newdata[valid,],type='response')
  pred=rep(0,nrow(newdata[valid,]))
  pred[predprob>0.50]=1
  lograte[n]=mean(pred==newdata[valid,]$popularity)
}
lograte
#test on test data
new=newdata[,1:15]
new$popularity=newdata$popularity
logfit=glm(popularity~.,data=new[train0,],family="binomial")
predprob=predict(logfit,newdata[test,],type='response')
pred=rep(0,nrow(newdata[test,]))
pred[predprob>0.50]=1
predictionrate=mean(pred==newdata[test,]$popularity)
predictionrate
#54.5

#plot ROC
#install.packages("pROC")
library(pROC)
pca.roc<-roc(newdata[test,]$popularity,predprob)
par(mfrow = c(1,1))
plot(pca.roc, print.auc=TRUE, auc.polygon=TRUE, grid=c(0.1, 0.2),
     grid.col=c("azure4", "azure4"), max.auc.polygon=TRUE,
     auc.polygon.col= "darkgoldenrod1", print.thres=TRUE)

##########################################
#PLS logistic
install.packages("plsRglm")
library(plsRglm)
names(mydata)
attach(mydata)
x=mydata[,-59]
x=x[,-54]
names(x)

#find best number of comp on validation
row=0
correction=rep(0,10)
for (i in c(2,3,5,7,10,15,20,25,30,35)){
  row=row+1
  model=PLS_glm(dataY = mydata[train,]$popularity,dataX = x[train,],dataPredictY = x[valid,],nt=i,modele="pls-glm-logistic")
  correction[row]=mean(ifelse(model$ValsPredictY>0.5,1,0)==mydata[valid,]$popularity)
}

#find prediction rate with num of components=2
model=PLS_glm(dataY = mydata[train0,]$popularity,dataX = x[train0,],dataPredictY = x[test,],nt=2,modele="pls-glm-logistic")
correctionrate=mean(ifelse(model$ValsPredictY>0.5,1,0)==mydata[test,]$popularity)
#54.94

#plot ROC
pls.roc<-roc(newdata[test,]$popularity,model$ValsPredictY)
par(mfrow = c(1,1))
plot(pls.roc, print.auc=TRUE, auc.polygon=TRUE, grid=c(0.1, 0.2),
     grid.col=c("azure4", "azure4"), max.auc.polygon=TRUE,
     auc.polygon.col= "darkgoldenrod1", print.thres=TRUE)

##########################################
#pCA KNN
#choose best number of components and neighbors on validation
rate=array(NA,dim=c(9,26))
row=0
for (n in c(3,5,7,10,15,20,25,30,35)){
  row=row+1
  xnewdata=newdata[,1:n]
  for (i in 1:n){
    xnewdata[,i]=xnewdata[,i]*pve[i]/sum(pve[1:n])
  }
  outputcol = which(colnames(newdata[train,]) == "popularity")
  yTrain_mani = newdata[train,][,outputcol]
  xTrain_mani = xnewdata[train,]
  yvalid_mani = newdata[valid,][,outputcol]
  xvalid_mani = xnewdata[valid,]
  #install.packages("class")
  library("class")
  for (j in 5:30){
    pred_knn_mani <- knn(xTrain_mani, xvalid_mani, yTrain_mani, k = j)
    rate[row,j-4]=mean(pred_knn_mani==yvalid_mani)
  }
}
#prediction rate on test(15 components,k=20)
xnewdata=newdata[,1:15]
for (i in 1:15){
  xnewdata[,i]=xnewdata[,i]*pve[i]/sum(pve[1:15])
}
outputcol = which(colnames(newdata[train0,]) == "popularity")
yTrainvalid_mani = newdata[train0,][,outputcol]
xTrainvalid_mani = xnewdata[train0,]
ytest_mani = newdata[test,][,outputcol]
xtest_mani = xnewdata[test,]
pred_knn_mani <- knn(xTrainvalid_mani, xtest_mani, yTrainvalid_mani, k = 20)
predictionrate=mean(pred_knn_mani==ytest_mani)
predictionrate
#53.09

##########################################
#conclusion:pls logistic gives the best prediction rate of 54.94%
