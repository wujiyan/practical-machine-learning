#load data
training <- read.csv("C:/R/practical machine learning/week4assignment/pml-training.csv", head = TRUE,na.strings = c("","NA","NULL"))
testing <- read.csv("C:/R/practical machine learning/week4assignment/pml-testing.csv", head = TRUE, na.strings = c("","NA","NULL"))
dim(training)
dim(testing)  # to see the dataset dimension

#drop variables with NA 
training <- training[, apply(training, 2, function(x) !any(is.na(x)))]
testing <- testing[,apply(testing,2,function(x) !any(is.na(x)))]
dim(training)
dim(testing)

#drop irrelevant variable
training <- training[,-c(1:6)]
testing <- testing[,-c(1:6)]

#avoid correlation
corr <- cor(testing[,-54])
highcor <- findCorrelation(corr, 0.85)
training <- training[, -highcor]
testing <- testing[, -highcor]
#avoid multicollinearity
multico <- findLinearCombos(testing[,-39])
training <- training[,-multico$remove]
testing <- testing[,-multico$remove]

library(caret)
#for purpose of cross validation
intrain <- createDataPartition(training$classe, p = 0.7, list = FALSE)
training <- training[intrain,]
training_cro <- training[-intrain,]

#random forest
modfit <- train(classe ~ ., data = training, method = "rf")
pre_rf <- predict(modfit, training_cro[,-21])
result1 <- confusionMatrix(pre_rf, training_cro$classe)
result1
plot(result1$table,  main = "random forest result")

#generalized boosted model
modfit_gbm <- train(classe ~ ., data =training, method = "gbm")
pre_gbm <- predict(modfit_gbm, training_cro[,-21])
result2 <- confusionMatrix(pre_gbm, training_cro$classe)
result2
plot(result2$table,  main = "generalized boosted model result")

#predict testing set
pre_test1 <- predict(modfit,testing)
pre_test1
pre_test2 <- predict(modfit_gbm, testing)
pre_test2
