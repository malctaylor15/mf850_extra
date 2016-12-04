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
#Baseline
table(test$CRED_APPROVED)/nrow(test)

# Try Random forest feature selection by cross validation 

# Prepare data 
y <- train$CRED_APPROVED
x <- model.matrix(CRED_APPROVED~., data= train)
# Fit the model
fit_temp <- rfcv(x, y , ntree = ntree)
# Cross validation error rate by number of features included in random forest 
error <- (fit_temp$error.cv)

# Potential RF guide 
# http://dni-institute.in/blogs/random-forest-using-r-step-by-step-tutorial/

# See the effect of the number of trees on the feature selection by cross validation 

numb_tries <- 6
# Pre allocate space 
ntree_df <- data.frame(matrix(0, nrow= numb_tries, ncol = 7))
names(ntree_df)[1:2] <-c("N Tree", "Accuracy")

# Check the effect of the number of trees on the feature selection technique 
for (ntree_times in 1:numb_tries){
  ntree <- 500+ntree_times*30
  fit_temp <- randomForest(x, y , ntree = ntree)
  error_sum <- fit_temp$error.cv
  ntree_df[ntree_times, 2:7] <- 1-error_sum # accuracy 
  ntree_df[ntree_times,1] <- ntree
  
}

max(ntree_df[,2:6])



