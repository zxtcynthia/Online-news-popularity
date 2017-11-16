#code of knn method
library(ISLR)
library(class)

rm(list=ls(all=TRUE))
#setwd("/Users/hellrambler/Documents/BA/BA－final/")
mydata<-read.csv("finaldata.csv")

#Drop insignificant explanatory variables
drops<-c('url', 'timedelta', "n_non_stop_words","kw_max_min","kw_avg_min","kw_min_max",
         "kw_avg_max","kw_min_avg","kw_max_avg","rate_positive_words","rate_negative_words",
         "min_positive_polarity","max_negative_polarity","abs_title_subjectivity",
         "weekday_is_monday","weekday_is_tuesday","weekday_is_wednesday",
         "weekday_is_thursday","weekday_is_friday","weekday_is_saturday","weekday_is_sunday")
mydata<-mydata[,!(colnames(mydata) %in% drops)]

#Variable Transformation 
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
#num_self_hrefs
#mydata$self_hrefs_binary <- ifelse(mydata$num_self_hrefs>0,1,0)
mydata$num_imgs<-ifelse(num_imgs>0,1,0)
mydata$num_videos<-ifelse(num_videos>0,1,0)
mydata$title_subjectivity_binary<-ifelse(title_subjectivity>0,1,0)
#Take out outliers:
mydata<-mydata[!(n_tokens_title>=15 
                 |n_non_stop_unique_tokens<0.3 
                 |average_token_length>6 
                 |global_rate_positive_words>0.11
                 |avg_negative_polarity< -0.8 ),]

set.seed(4650)
mydata$popularity=ifelse(mydata$shares<=1400,0,1)

final_data <- c('popularity',"n_unique_tokens","num_hrefs","num_videos","data_channel_is_bus","data_channel_is_lifestyle",
                "data_channel_is_socmed","data_channel_is_entertainment","data_channel_is_tech","data_channel_is_world",
                "kw_avg_avg","kw_min_min","kw_max_max","is_weekend","global_sentiment_polarity","global_rate_positive_words",
                "global_rate_negative_words","avg_positive_polarity","max_positive_polarity","title_sentiment_polarity","abs_title_sentiment_polarity",
                "num_self_hrefs_sqrt")

news_data <- mydata[,colnames(mydata) %in% final_data]
attach(news_data)

#scale part of the variables
news_data['scale_kw_max'] = scale(news_data['kw_max_max'])
news_data['scale_kw_min'] = scale(news_data['kw_min_min'])
news_data['scale_kw_avg'] = scale(news_data['kw_avg_avg'])
news_data['scale_ref'] = scale(news_data['num_hrefs'])
attach(news_data)

#drop the unscaled columns
scale_drops <- c('kw_max_max',"kw_min_min",'kw_avg_avg','num_hrefs')
news_data_scale <- news_data[,!(colnames(news_data) %in% scale_drops)]
detach(news_data)
attach(news_data_scale)

#variables intend to add
check_set <- c('global_subjectivity','n_tokens_title')
check_data <- mydata[,colnames(mydata) %in% check_set]

#drop all redundant information
remove(mydata,news_data,check_set,drops,final_data,scale_drops)

#set trainrows

manual_knn_data <- news_data_scale
trainrows_w=sample(1:nrow(manual_knn_data), 0.75*nrow(manual_knn_data))
train_validation_w = sample(1:length(trainrows_w), (1/3)*length(trainrows_w))
traindata_w=manual_knn_data[trainrows_w[-train_validation_w],]
validdata_w = manual_knn_data[trainrows_w[train_validation_w],]
testdata_w=manual_knn_data[-trainrows_w,]
remove(news_data_scale)

#weight manipulation
old_weight = array(1/21,21)

unscale_new_weight = array(NA,21)
unscale_new_weight[1] <- 1/21 #num_uni_token
unscale_new_weight[2] <- 1/21 #num_video
unscale_new_weight[3] <- 1/21 #lifestyle
unscale_new_weight[4] <- 1/21 #entertainment
unscale_new_weight[5] <- 1/21 #business
unscale_new_weight[6] <- 2/21 #social media
unscale_new_weight[7] <- 1/21 #technology
unscale_new_weight[8] <- 1/21 #world
unscale_new_weight[9] <- 1/21 #weekend
unscale_new_weight[10] <- 1/21 #g_sentiment_polarity
unscale_new_weight[11] <- 1/21 #rate_pos_word
unscale_new_weight[12] <- 1/21 #rate_neg_word
unscale_new_weight[13] <- 1/21 #avg_pos_polarity
unscale_new_weight[14] <- 1/21 #max_pos_polarity
unscale_new_weight[15] <- 1/21 #title_sentiment_polarity
unscale_new_weight[16] <- 1/21 #abs_title_sentiment_polarity
unscale_new_weight[17] <- 1/21 #num_self_ref_sqrt
unscale_new_weight[18] <- 1/21 #scale_kw_max
unscale_new_weight[19] <- 1/21 #scale_kw_min
unscale_new_weight[20] <- 1/21 #scale_kw_avg
unscale_new_weight[21] <- 1/21 #scale_ref

#rescale our weights
scaled_new_weight = unscale_new_weight/sum(unscale_new_weight)
#generate a manipulate_array
mani_ar = array(NA,21)
for (i in 1:length(scaled_new_weight)){
  mani_ar[i] = scaled_new_weight[i]/old_weight[i]
}

#get xtrain, ytrain, xtest, ytest
outputcol = which(colnames(traindata_w) == "popularity")
yTrain_mani = traindata_w[,outputcol]
xTrain_mani = traindata_w[-outputcol]
yTest_mani = validdata_w[,outputcol]
xTest_mani = validdata_w[-outputcol]

#generate scaled data
gen_xTrain_mani <- xTrain_mani
gen_xTest_mani <- xTest_mani
for (j in 1:21){
  gen_xTrain_mani[j] = xTrain_mani[j]*mani_ar[j]
  gen_xTest_mani[j] = xTest_mani[j]*mani_ar[j]
}

remove(i)
remove(j)

pre_perf = array(NA,21)

for (i in 1:21){
  pred_knn_mani <- knn(gen_xTrain_mani, gen_xTest_mani, yTrain_mani, k = i)
  pre_perf[i] = mean(pred_knn_mani == yTest_mani)
}

pre_perf

#IMPROVEMENT BEGINS HERE

# for loop to check the result if one variable doubles at a time
preference_array = array(NA, 21)

for (m in 1:21){
  unscale_new_weight = array(1/21,21)
  unscale_new_weight[m] = 2/21
  
  #rescale our weights
  scaled_new_weight = unscale_new_weight/sum(unscale_new_weight)
  #generate a manipulate_array
  mani_ar = array(NA,21)
  for (i in 1:length(scaled_new_weight)){
    mani_ar[i] = scaled_new_weight[i]/old_weight[i]
  }
  
  #get xtrain, ytrain, xtest, ytest
  #outputcol = which(colnames(traindata_w) == "popularity")
  #yTrain_mani = traindata_w[,outputcol]
  #xTrain_mani = traindata_w[-outputcol]
  #yTest_mani = testdata_w[,outputcol]
  #xTest_mani = testdata_w[-outputcol]
  
  #generate scaled data
  #gen_xTrain_mani <- xTrain_mani
  #gen_xTest_mani <- xTest_mani
  for (j in 1:21){
    gen_xTrain_mani[j] = xTrain_mani[j]*mani_ar[j]
    gen_xTest_mani[j] = xTest_mani[j]*mani_ar[j]
  }
  
  remove(i)
  remove(j)
  
  pre_perf = array(NA,21)
  
  for (i in 1:21){
    pred_knn_mani <- knn(gen_xTrain_mani, gen_xTest_mani, yTrain_mani, k = i)
    pre_perf[i] = mean(pred_knn_mani == yTest_mani)
  }
  
  pre_perf
  num = which.max(pre_perf)
  print(pre_perf[num])
  preference_array[m] = pre_perf[num]
  
}


preference_array = array(NA, 21)

for (m in 1:21){
  unscale_new_weight = array(19/441,21)
  unscale_new_weight[m] = 38/441
  unscale_new_weight[5] = 2/21
  
  #rescale our weights
  scaled_new_weight = unscale_new_weight/sum(unscale_new_weight)
  #generate a manipulate_array
  mani_ar = array(NA,21)
  for (i in 1:length(scaled_new_weight)){
    mani_ar[i] = scaled_new_weight[i]/old_weight[i]
  }
  
  #get xtrain, ytrain, xtest, ytest
  #outputcol = which(colnames(traindata_w) == "popularity")
  #yTrain_mani = traindata_w[,outputcol]
  #xTrain_mani = traindata_w[-outputcol]
  #yTest_mani = testdata_w[,outputcol]
  #xTest_mani = testdata_w[-outputcol]
  
  #generate scaled data
  #gen_xTrain_mani <- xTrain_mani
  #gen_xTest_mani <- xTest_mani
  for (j in 1:21){
    gen_xTrain_mani[j] = xTrain_mani[j]*mani_ar[j]
    gen_xTest_mani[j] = xTest_mani[j]*mani_ar[j]
  }
  
  remove(i)
  remove(j)
  
  pre_perf = array(NA,21)
  
  for (i in 1:21){
    pred_knn_mani <- knn(gen_xTrain_mani, gen_xTest_mani, yTrain_mani, k = i)
    pre_perf[i] = mean(pred_knn_mani == yTest_mani)
  }
  
  pre_perf
  num = which.max(pre_perf)
  print(pre_perf[num])
  preference_array[m] = pre_perf[num]
  
}

preference_array

# check if it is effecient to add new variables
check_set <- c('global_subjectivity','n_tokens_title')
check_data <- mydata[,colnames(mydata) %in% check_set]

#mani_data[23] = check_data[1] 

preference_check = array(NA, 2)
old_weights_check = array(1/22, 22)

for (num_check in 1:2){
  unscale_new_weight = array(1/22,22)
  
  #rescale our weights
  scaled_new_weight = unscale_new_weight/sum(unscale_new_weight)
  #generate a manipulate_array
  mani_ar = array(NA,21)
  for (i in 1:length(scaled_new_weight)){
    mani_ar[i] = scaled_new_weight[i]/old_weight[i]
  }
  
  #get xtrain, ytrain, xtest, ytest
  manual_check <- manual_knn_data
  manual_check[23] <- check_data[num_check]
  traindata_check=manual_check[trainrows_w[-train_validation_w],]
  validdata_check = manual_check[trainrows_w[train_validation_w],]
  outputcol = which(colnames(traindata_check) == "popularity")
  
  yTrain_check = traindata_check[,outputcol]
  xTrain_check = traindata_check[-outputcol]
  yTest_check = testdata_check[,outputcol]
  xTest_check = testdata_check[-outputcol]
  
  #generate scaled data
  gen_xTrain_check <- xTrain_check
  gen_xTest_check <- xTest_check
  for (j in 1:21){
    gen_xTrain_check[j] = xTrain_check[j]*mani_ar[j]
    gen_xTest_check[j] = xTest_check[j]*mani_ar[j]
  }
  
  remove(i)
  remove(j)
  
  pre_perf = array(NA,21)
  
  for (i in 1:21){
    pred_knn_mani <- knn(gen_xTrain_check, gen_xTest_check, yTrain_check, k = i)
    pre_perf[i] = mean(pred_knn_check == yTest_check)
  }
  
  pre_perf
  num = which.max(pre_perf)
  print(pre_perf[num])
  preference_check[num_check] = pre_perf[num]
  
}

preference_check


