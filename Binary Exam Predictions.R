library(dplyr)

studentPerformance <- read.csv("StudentsPerformance.csv", header = TRUE)
head(studentPerformance)
#train & test & validate
set.seed(10)
sample_train<- sample(seq_len(nrow(studentPerformance)), size = floor(0.80*nrow(studentPerformance)))
sample_valid<- sample(seq_len(nrow(studentPerformance)), size = floor(0.15*nrow(studentPerformance)))
sample_test <- sample(seq_len(nrow(studentPerformance)), size = floor(0.05*nrow(studentPerformance)))
train     <- studentPerformance[sample_train, ]
validation<- studentPerformance[sample_valid, ]
test      <- studentPerformance[sample_test, ]



#take out scores for test data
test$math.score <- NULL
test$reading.score <- NULL
test$writing.score <- NULL

View(test)

#feature engineering
##combine values for testing results
train$Result <- paste((train$math.score+train$reading.score+train$writing.score)/3)
validation$Result <- paste((validation$math.score+validation$reading.score+validation$writing.score)/3)
head(validation)
##removing reading, writing, and math scores 
train$math.score <- NULL 
train$reading.score <- NULL 
train$writing.score <- NULL 
validation$math.score <- NULL 
validation$reading.score <- NULL 
validation$writing.score <- NULL 

##condense results to pass/fail 
train <- mutate(train, TestResults = ifelse(train$Result > 70,print("Pass"),print("Fail"))) 
validation <- mutate(validation, TestResults = ifelse(validation$Result > 70,print("Pass"),print("Fail")))

head(train)
head(validation)
head(test)

##changing test results to factor and results to int 
train$TestResults <- as.factor(train$TestResults) 
train$Result <- as.numeric(train$Result) 
validation$TestResults <- as.factor(validation$TestResults) 
validation$Result <- as.numeric(validation$Result)

#variable selection
##random forest
library(randomForest)

###without the numeric grades as a variable the best predictors are lunch, test prep and gender 
rf.train <- randomForest(TestResults ~ gender+race.ethnicity+parental.level.of.education+lunch+test.preparation.course, train, importance = TRUE)
importance(rf.train)
varImpPlot(rf.train)

#model engineering and selection
##second model has the lowest AIC so proceed w/ all variables
glm_model.1 <- glm(TestResults ~ lunch+test.preparation.course+gender, data = train, family = "binomial")
summary(glm_model.1) #AIC is 998.71
glm_model.2 <- glm(TestResults ~ gender+race.ethnicity+parental.level.of.education+lunch+test.preparation.course, data = train, family = "binomial")
summary(glm_model.2) #AIC is 980.64

#predict on validation data using selected model 1 and 2
prediction_m1 <- predict(glm_model.1, validation, type="response")
prediction_m2 <- predict(glm_model.2, validation, type="response") ##changed probability to binary factors
prediction_m1 <- ifelse(prediction_m1 > 0.50, print("Pass"), print("Fail"))
prediction_m2 <- ifelse(prediction_m2 > 0.50, print("Pass"), print("Fail")) ##Combine results of test data
View(prediction_m1)
View(validation)

#test model accuracy for validation set

##compute confusion matrix for validation  ###model 1: 107/150 predicted correctly ~ 71% accuracy 
true_observation <- validation$TestResults

table(true_observation, prediction_m2) #model 2 confusion matrix ~ 69% accuracy 
table(true_observation, prediction_m1) #model 1 confusion matrix ~ 71% accuracy 

#apply model 1 to testing data
prediction.test <- predict(glm_model.1, test, type="response") 
prediction.test <- ifelse(prediction.test > 0.50, print("Pass"), print("Fail")) 
prediction.test 
##create an output df to show predictions in test data 
##we can assume that this will be around 70% accurate 
test.output <- cbind(test, prediction.test)

View(test.output)
