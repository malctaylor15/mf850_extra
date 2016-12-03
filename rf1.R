setwd("C:/Users/board/Desktop/Kaggle/mf850_extra")

data <- read.csv("mf850-loan-data.csv")

# Some data cleaning 
data$DEPENDENTS <- as.factor(data$DEPENDENTS)
data$CRED_HERE <- as.factor(data$CRED_HERE)
data$INSTALLMENTRATE <- as.factor(data$INSTALLMENTRATE)
data$ATADDRESSSINCE <- as.factor(data$ATADDRESSSINCE)
data$AGE <- scale(data$AGE)
data$DURATION <- scale(data$DURATION)

head(data)
summary(data)

# Sample split into test train set 
indexes <- sample(1:nrow(data), size = 0.2*nrow(data))
test <- data[indexes, ]
train <- data[-indexes, ]

library(randomForest)
set.seed(1)
# Fit random forest on training data 
fit_rf1 <- randomForest(CRED_APPROVED~. , data = train)
# Predict outcomes on test data 
predict_rf <- predict(fit_rf1, newdata= test)
misClasificError <- mean(predict_rf != test$CRED_APPROVED)
print(paste('Accuracy',1-misClasificError))

# Confusion matrix 
library(caret)
# http://dni-institute.in/blogs/random-forest-using-r-step-by-step-tutorial/