library(tidyr)
library(dplyr)
library(tm)
library(caret)
library(DMwR)
library(chron)
library(tidytext)
library(InformationValue)
library(e1071)
require(kernlab)
require(car)
library(magrittr)
library(randomForest)
library(kernlab)

############################################### import train csv ####
train <- read.csv("C:/Users/s08x55/C_Drive_Folders/AnalyticsVidya/LordOfTheMachines/train.csv", 
                  #colClasses = c(send_date = "character"), stringsAsFactors = FALSE
                  )

head(train)
str(train)
prop.table(table(train$is_click))
#    0          1 
#0.98750771 0.01249229 


# change data type
train$id <- as.factor(train$id)
train$user_id <- as.factor(train$user_id)
train$campaign_id <- as.factor(train$campaign_id)
train$send_date <- as.factor(train$send_date)
train$is_click <- as.factor(train$is_click)
train$is_open <- as.factor(train$is_open)

# treat imbalanced data
trainSplit <- SMOTE(is_click ~ ., train, perc.over = 100, perc.under = 200, learner = NULL) # adjusting the class balance
prop.table(table(trainSplit$is_click))
dim(trainSplit)
train_new <- as.data.frame(trainSplit)

#change the data types again
train_new$campaign_id <- as.character(train_new$campaign_id)
train_new$send_date <- as.character(train_new$send_date)


#format date & time
train_new$send_time <- substr(train_new$send_date,12,16) #created time time field
train_new$send_date <- substr(train_new$send_date,1,10) #created date time field

train_new$send_date_n <- pbapply::pbapply(train_new['send_date'], 1, FUN = function(x) if(as.integer(substr(x, 1,2))>12) as.character(as.Date(x, format = "%d-%m-%Y")) else  as.character(as.Date(x, format = "%m-%d-%Y")))
train_new$send_date_n <- as.Date(train_new$send_date_n, format = "%Y-%m-%d")

# derive week day 
train_new$day <- as.factor(as.POSIXlt(train_new$send_date_n)$wday)

# derive time of the day
train_new$time_of_day <- pbapply::pbapply(train_new['send_time'],1,FUN = function(x) if(as.integer(substr(as.character(x),1,2)) < 12) "Morning" else if(as.integer(substr(as.character(x),1,2)) > 17) "Evening" else "Afternoon")
train_new$time_of_day <- as.factor(train_new$time_of_day)

# derive time of the month
train_new$time_of_month <- pbapply::pbapply(train_new['send_date_n'], 1, FUN = function(x) if(as.integer(substr(as.character(x),9,10)) > 15) "Second_half" else "First_half")
train_new$time_of_month <- as.factor(train_new$time_of_month)

# calculating hitRate for each campaign
#opened <- read.csv("C:/Users/s08x55/C_Drive_Folders/AnalyticsVidya/LordOfTheMachines/train.csv",colClasses = c(is_open = "numeric")) 
hitRate <- as.data.frame(table(train$campaign_id))
colnames(hitRate) <- c('campaign_id','total_emails')

opened <- train[,c(3,5)]
opened$is_open <- as.numeric(opened$is_open)
opened_sum <- opened %>%
  select(campaign_id, is_open) %>%
  group_by (campaign_id) %>%
  summarise(hitRate = sum(is_open))
  
colnames(opened_sum) <- c('campaign_id','hits')
opened_sum$campaign_id <- as.character(opened_sum$campaign_id)
hitRate <- left_join(hitRate,opened_sum, by = 'campaign_id' )
hitRate$hitRate <- hitRate$hits / hitRate$total_emails
hitRate$hitRate <- as.factor(cut_number(hitRate$hitRate, 3))
hitRate$hitRate1 <- ifelse(hitRate$hitRate == "(0.172,0.373]",1,0 )

##################################################### import test csv ####
test <- read.csv("C:/Users/s08x55/C_Drive_Folders/AnalyticsVidya/LordOfTheMachines/test_BDIfz5B.csv", 
                  #colClasses = c(send_date = "character"), stringsAsFactors = FALSE
                 )
# change data type
test$id <- as.factor(test$id)
test$user_id <- as.factor(test$user_id)
test$campaign_id <- as.character(test$campaign_id)
test$send_date <- as.character(test$send_date)

test_new <- as.data.frame(test)

#format date & time
test_new$send_time <- substr(test_new$send_date,12,16) #created time time field
test_new$send_date <- substr(test_new$send_date,1,10) #created date time field

test_new$send_date_n <- pbapply::pbapply(test_new['send_date'], 1, FUN = function(x) if(as.integer(substr(x, 1,2))>12) as.character(as.Date(x, format = "%d-%m-%Y")) else  as.character(as.Date(x, format = "%m-%d-%Y")))
test_new$send_date_n <- as.Date(test_new$send_date_n, format = "%Y-%m-%d")

# derive week day 
test_new$day <- as.factor(as.POSIXlt(test_new$send_date_n)$wday)

# derive time of the day
test_new$time_of_day <- pbapply::pbapply(test_new['send_time'],1,FUN = function(x) if(as.integer(substr(as.character(x),1,2)) < 12) "Morning" else if(as.integer(substr(as.character(x),1,2)) > 17) "Evening" else "Afternoon")
test_new$time_of_day <- as.factor(test_new$time_of_day)

# derive time of the month
test_new$time_of_month <- pbapply::pbapply(test_new['send_date_n'], 1, FUN = function(x) if(as.integer(substr(as.character(x),9,10)) > 15) "Second_half" else "First_half")
test_new$time_of_month <- as.factor(test_new$time_of_month)


################################################## read the campaign csv ####
campaign <- read.csv("file:///C:/Users/s08x55/C_Drive_Folders/AnalyticsVidya/LordOfTheMachines/campaign_data.csv")
head(campaign)
str(campaign)
colnames(campaign)
campaign$campaign_id <- as.character(campaign$campaign_id)
campaign$no_of_sections <- as.factor(campaign$no_of_sections)
campaign$communication_type <- as.factor(campaign$communication_type)

campaign$total_links_group <- as.factor(cut_number(campaign$total_links, 4))
campaign$no_of_internal_links_group <- as.factor(cut(campaign$no_of_internal_links, 4))
campaign$no_of_images_group <- as.factor(cut(campaign$no_of_images, 3))
campaign$no_of_images_group1 <- as.factor(ifelse(campaign$no_of_images_group == "(7,13]",1,0))# select only the significant category

#joins from campaign file
keep = c("campaign_id","communication_type","total_links_group","no_of_images_group1","hitRate1","across","book","can","no_of_sections","come","hackathon","job","career","scientist","algorithm","skill","speaker","topic","smart","excel","professor","univers")
campaign_sub <- campaign[,(names(campaign) %in% keep)]
campaign_sub <- left_join(campaign_sub, hitRate, by = 'campaign_id')

test_final <- left_join(test_new, campaign_sub, by = 'campaign_id')
#test_final <- left_join(test_final, hitRate, by = "campaign_id")

colnames(campaign)
campaign <- campaign[,-c(3:5)]

# concatenate email subject and email body
campaign$email_text <- paste(campaign$subject,campaign$email_body)

#create corpus from the text feild
email_text <- campaign[,c('email_text')]
#email_text <- campaign[,c('subject')]
email_corpus <- Corpus(VectorSource(email_text))

#clean the corpus
email_corpus_clean <- tm_map(email_corpus, content_transformer(tolower))
email_corpus_clean <- tm_map(email_corpus_clean, removePunctuation)
email_corpus_clean <- tm_map(email_corpus_clean, removeNumbers)
email_corpus_clean <- tm_map(email_corpus_clean, removeWords,stopwords("english"))
email_corpus_clean <- tm_map(email_corpus_clean, stemDocument)
email_corpus_clean <- tm_map(email_corpus_clean, stripWhitespace)

# create dtm
email_dtm <- DocumentTermMatrix(email_corpus_clean)
df <- as.matrix(email_dtm) #convert dtm into a matrix
df <- pbapply::pbapply(df,2,function(x){ifelse(x>0,1,0)}) #flag instead of value
campaign <- as.data.frame(cbind(campaign, df)) # merge dtm with campaign table 

#cols <- dput(colnames(campaign[10:189])) #select the DTM columns to change their data types
#campaign[cols] <- lapply(campaign[cols], factor)

# Create final train and test  set
train_final <- left_join(train_new, campaign, by = 'campaign_id')
#train_final <- left_join(train_final, hitRate, by = "campaign_id")
colnames(train_final)
drops <- c("id","user_id","campaign_id","send_date","is_open","send_time","send_date_n","email_body",
          "subject","email_url","email_text")#drop unwanted columns
train_final <- train_final[ ,!(names(train_final) %in% drops)]#drop unwanted columns

set.seed(100)
training_input_rows <- sample(1:nrow(train_final),0.80*nrow(train_final)) #select random indexes for training sampless
training_set <- train_final[training_input_rows,] # created training set
testing_set <- train_final[-training_input_rows,] # created testing set

prop.table(table(training_set$is_click))
#prop.table(table(testing_set$is_click))
#write.csv(training_set,"file:///C:/Users/s08x55/C_Drive_Folders/AnalyticsVidya/LordOfTheMachines/training_set.csv")

#test_final <- left_join(test_new, campaign, by = 'campaign_id')
#colnames(test_final)

#drop <- c("id","user_id","campaign_id","send_date","is_open","send_time","send_date_n","email_body",
          "subject","email_url","email_text")#drop unwanted columns
#test_final <- test_final[ ,!(names(test_final) %in% drop)]#drop unwanted columns

#colnames(test_final)[2] <- "day.x"


##################################################### Logistic ####


#logitModel <- glm(is_click  ~ ., data = training_set, family = binomial(link="logit"))
logitModel2 <- glm(is_click  ~ day.x+time_of_day+communication_type+total_links_group+no_of_images_group1+across+book+can+come,
                  data = training_set, family = binomial(link="logit")) # with selected variables - which were found significant

predicted <- plogis(predict(logitModel2, training_set))# or
predicted1<- predict(logitModel2, testing_set, type ="response")
predicted2 <- predict(logitModel2, testing_set)

optCutoff <- optimalCutoff(testing_set$is_click, predicted)[1]

summary(logitModel2) #check significant variables
misClassError(testing_set$is_click, predicted2, threshold = .70) # check miss classifcation
plotROC(testing_set$is_click, predicted2)  #plot ROC
confusionMatrix(testing_set$is_click, predicted2
                #, threshold = optCutoff
                ) #confusion matrix
  
##### Stepwise + Logit
install.packages("MASS")
library(MASS)
backward <- stepAIC(logitModel2, direction = "backward", trace = FALSE) # backward elimination

###################################################### SVM ####

trainSVM <- training_set[, c("is_click","day.x", "time_of_day", "communication_type", "no_of_sections", 
                             "total_links_group", "no_of_images_group", "across", "book", 
                             "can", "come") ]
kernel = c('rbfdot','polydot','vanilladot','tanhdot','laplacedot','besseldot','anovadot','splinedot','stringdot')

#choosing which kernel fits best
for (i in 1:length(kernel)){
  svmModel <- ksvm(is_click ~ ., data = trainSVM, kernel = kernel[i], cost = 10, scale = FALSE)
  print(kernel[i])
  print(svmModel)
}

svmModel_final <- ksvm(is_click ~ ., data = trainSVM, kernel = "rbfdot", scale = FALSE)
print(svmModel_final)

svmPredict = predict(svmModel_final, testing_set[-1])
confusionMatrix(svmPredictpredict, testing_set$is_click)


cons(svmModel_final, trainSVM)
compareTable <- table (testing_set$is_click, predict(svmModel_final,testing_set[-1]))  # tabulate
mean(testing_set$is_click != predict(svmModel_final)) # XX.XX% misclassification error

plotROC(as.integer(testing_set$is_click),as.integer(svmPredict))

####################################################### Naive Bayes ####
library (e1071)
keep = c("is_click","day.x","time_of_day","communication_type","total_links_group","no_of_images_group1","hitRate1","across","book","can","no_of_sections","come")
nb_train <- training_set[,(names(training_set) %in% keep)]
nb_train$across <- as.factor(nb_train$across)
nb_train$book <- as.factor(nb_train$book)
nb_train$can <- as.factor(nb_train$can)
nb_train$come <- as.factor(nb_train$come)
#nb_train$hitRate1 <- as.factor(nb_train$hitRate1)

nb_test <- testing_set[,(names(testing_set) %in% keep)]
nb_test$across <- as.factor(nb_test$across)
nb_test$book <- as.factor(nb_test$book)
nb_test$can <- as.factor(nb_test$can)
nb_test$come <- as.factor(nb_test$come)
#nb_test$hitRate1 <- as.factor(nb_test$hitRate1)

nbModel = naiveBayes(is_click ~ ., data = nb_train) # Build Naive Bayes Model
#fitted = bn.fit(nbModel, nb_train, method="bayes", iss=1)

pred = predict(nbModel, nb_test[-1]) # run predict and get probabilities
confusionMatrix(nb_test$is_click, as.numeric(pred))
plotROC(nb_test$is_click, as.numeric(pred))  #plot ROC

# results_prob = data.frame(t(attributes(pred)$prob))
# x <- cbind(pred, results_prob)
# head(x)
# realResults <- nb_test[,c('is_click')]

  
###################################################### Random Forest  ####

library(randomForest)
library(ROCR)

# prepare train and test data
keep = c("is_click","day.x","time_of_day","communication_type","total_links_group","no_of_images_group1","across","book","can","no_of_sections","come","no_of_sections","come","hackathon","job","career","scientist","algorithm","skill","speaker","topic","smart","excel","professor","univers")
nb_train <- training_set[,(names(training_set) %in% keep)]
nb_test <- testing_set[,(names(testing_set) %in% keep)]

#build model
set.seed(222)
rf <- randomForest(is_click  ~ .,
                   data = nb_train,
                   ntree =100,
                   mtry = 3
)
print(rf)
rf$confusion
# p1<- predict(rf, training_set)
# head(p1)
# head(training_set$is_click)
# confusionMatrix(p1, training_set$is_click)
# 
# p2 <- predict(rf, nb_test)
# confusionMatrix(p2, nb_test$is_click)

plot(rf)

t <- tuneRF(nb_train[,-1], nb_train[,1],
            stepFactor = 0.4,
            plot = TRUE,
            ntreeTry = 300,
            trace = TRUE,
            improve = 0.05
)


p1 = predict(rf, nb_test[-1])
confusionMatrix(nb_test$is_click,as.numeric(p1))
plotROC(nb_test$is_click,as.numeric(p1))

# #Calculate AUC and plot ROC
# predictions=as.vector(rf$votes[,2])
# pred=prediction(predictions,nb_train$is_click)
# 
# perf_AUC=performance(pred,"auc") #Calculate the AUC value
# AUC=perf_AUC@y.values[[1]]
# 
# perf_ROC=performance(pred,"tpr","fpr") #plot the actual ROC curve
# plot(perf_ROC, main="ROC plot")
# text(0.5,0.5,paste("AUC = ",format(AUC, digits=5, scientific=FALSE)))

                                                                ##### Final prediction - Using Random Forest ####

keep = c("day","time_of_day","communication_type","total_links_group","no_of_images_group1","across","book","can","no_of_sections","come","no_of_sections","come","hackathon","job","career","scientist","algorithm","skill","speaker","topic","smart","excel","professor","univers")
test_final1 <- test_final[,(names(test_final) %in% keep)]
colnames(test_final1)[1] <- "day.x"

#build model
set.seed(333)

#predict on the final test data
p3 <- predict(rf, test_final1[-1])
#confusionMatrix(p3, test_final1$is_click)


#Calculate AUC and plot ROC
# predictions=as.vector(rf$votes[,2])
# pred=prediction(predictions,nb_train$is_click)
# 
# perf_AUC=performance(pred,"auc") #Calculate the AUC value
# AUC=perf_AUC@y.values[[1]]
# 
# perf_ROC=performance(pred,"tpr","fpr") #plot the actual ROC curve
# plot(perf_ROC, main="ROC plot")
# text(0.5,0.5,paste("AUC = ",format(AUC, digits=5, scientific=FALSE)))

#final output
#p3_m <- as.matrix(p3, nrow = nrow(p3_m))
id <- test_final$id
p3_m <- as.matrix(p3,nrow=nrow(p3))
output1<- as.data.frame(cbind(as.character(id),p3_m[,1]))
colnames(output1) <- c("id","is_click")
dim(output1)
head(output1)
tail(output1)
table(output1$is_click)
write.csv(output1,"C:/Users/s08x55/C_Drive_Folders/AnalyticsVidya/LordOfTheMachines/PredictionsRandomForest2.csv",)

############################################# Plots and graphs


