# Random forest cross validate for best number of trees 

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

numb_RFs <- 40
tree_multiple <- 60
# Pre allocate space 
ntree_df <- data.frame(matrix(0,nrow= numb_RFs, ncol= 2))
# Rename columns 
names(ntree_df)[1:2] <-c("N Tree", "Accuracy")

for (RF in 1:numb_RFs){ 
  # Set the number of trees we wish to use 
  ntree <- 500 + RF*tree_multiple
  # Fit random Forest using training data and number of tree specified above 
  fit_temp <- randomForest(CRED_APPROVED~. , data=train,ntree = ntree)
  # Calculate error rate - random forest class pre computers class errors 
  error_rate <- fit_temp$confusion[5] + fit_temp$confusion[6]
  # Store error rate with number of tree 
  ntree_df[RF, c(1,2)] <- c(ntree,error_rate) 
} 

plot(ntree_df)
fit_tree <- lm(Accuracy~., data= ntree_df)
abline(fit_tree, col= "red")
