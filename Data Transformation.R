rm(list=ls(all=TRUE))
setwd("~/Desktop/CU/Fall 2016/4650 BA/Project")
mydata = read.csv(file = "finaldata.csv")
attach(mydata)

#Drop insignificant explanatory variables
drops<-c("n_non_stop_words","kw_max_min","kw_avg_min","kw_min_max",
         "kw_avg_max","kw_min_avg","kw_max_avg","rate_positive_words",
         "rate_negative_words","abs_title_subjectivity","weekday_is_monday",
         "weekday_is_tuesday","weekday_is_wednesday","weekday_is_thursday","weekday_is_friday","weekday_is_saturday",
         "weekday_is_sunday","timedelta","url")
mydata<-mydata[,!(colnames(mydata) %in% drops)]


##histogram##
mydata[,37]=as.numeric(mydata[,37])
par(mfrow=c(3,3))
for(i in 1:length(mydata)){
  hist(mydata[,i],xlab=names(mydata)[i])
}
#par(mfrow=c(1,1))

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
mydata$num_imgs<-ifelse(num_imgs>0,1,0)
mydata$num_videos<-ifelse(num_videos>0,1,0)
mydata$kw_max_max<-ifelse(kw_max_max>=843300,1,0)
mydata$range_positive=max_positive_polarity-min_positive_polarity
mydata$range_negative=max_negative_polarity-min_negative_polarity
mydata$title_subjectivity_binary<-ifelse(title_subjectivity>0,1,0)

#Take out outliers:
mydata<-mydata[!(n_tokens_title>=15 
                 |n_non_stop_unique_tokens<0.3 
                 |average_token_length>6 
                 |global_rate_positive_words>0.11
                 |avg_negative_polarity< -0.8),]

#Divide dataset into 75% training set and 25% test set
set.seed(4650)
mydata$popularity=ifelse(mydata$shares<=1400,0,1)
trainrows=sample(1:nrow(mydata), 0.75*nrow(mydata))
traindata=mydata[trainrows,]
testdata=mydata[-trainrows,]
