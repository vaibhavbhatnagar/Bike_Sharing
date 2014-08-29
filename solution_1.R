library(lubridate)
library(randomForest)
library(caret)
library(lattice)
library(Metrics)

train.data <- read.csv("train.csv")
test <- read.csv("test.csv")

# cross validation set
set.seed(101)
intrain <- createDataPartition(train.data$count, p=0.7, list = F, times = 1)
cross.val <- train.data[-intrain,]
train <- train.data[intrain,]

train$hr <- hour(train$datetime)
test$hr <- hour(test$datetime)
cross.val$hr <- hour(cross.val$datetime)

train$day <- wday(train$datetime)
test$day <- wday(test$datetime)
cross.val$day <- wday(cross.val$datetime)

train$month <- month(train$datetime)
cross.val$month <- month(cross.val$datetime)
test$year <- year(test$datetime)

train$year <- year(train$datetime)
cross.val$year <- year(cross.val$datetime)
test$year <- year(test$datetime)

#Choose Model
# fit <- randomForest(as.factor(count) ~ season + holiday + weather + day+ hr + temp + atemp+
#                       humidity+windspeed , data=train.data, ntree = 700, importance=TRUE)
fit_rf <- randomForest(count ~ season + holiday + weather + day+ hr + temp + atemp+
                      humidity+windspeed , data=train, ntree = 700,importance=TRUE)
fit_poisson <- glm(count ~  hr , data=train, family = "poisson" )

varImpPlot(fit)

# Make prediction
predict_cv <- predict(fit_rf,cross.val)
prediction <- predict(fit_rf,test)

# Calculate accuracy
rmsle(cross.val$count, predict_cv)

postResample(cross.val$count, predict_cv)

# Create file for submission
submit <- data.frame(datetime = test$datetime, count = prediction)
write.csv(submit, file = "rf.csv", row.names = F)

