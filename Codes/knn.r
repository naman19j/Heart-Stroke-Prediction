library(caret)
library(pROC)
library(mlbench)
data <- read.csv(file.choose(),header=T)
start_time <- Sys.time()
str(data)
data$stroke[data$stroke == 0] <- 'No'
data$stroke[data$stroke == 1] <- 'Yes'
data$stroke<-factor(data$stroke)
set.seed(1234)
ind <- sample(2, nrow(data), replace=T, prob=c(0.7,0.3))
training <- data[ind==1,]
test <- data[ind==2,]

#KNN Model
trControl <- trainControl(method="repeatedcv",
                          number=10,
                          repeats=3)
set.seed(222)
fit <- train(stroke ~.,
             data=training,
             method="knn",
             tuneLength=20,
             trControl=trControl,
             preProc=c("center","scale"))
fit
plot(fit)
varImp(fit)
pred <- predict(fit,newdata=test)
confusionMatrix(pred,test$stroke)
end_time <- Sys.time()
end_time-start_time