# R code for a Credit Approval Model

#########################################
#                                       #
#   PART 1: Data loading and cleaning   #
#                                       #
#########################################

#install libraries
install.packages("boot")
install.packages("glmnet")
install.packages("randomForest")
install.packages("caret")

# load up the libraries
library(boot)
library(glmnet)
library(randomForest)
library(caret)

# Set the Seed
set.seed(1)

# Set working directory and load the data
#setwd("C:/Users/Evan/Downloads")
data <- read.csv("mf850-loan-data.csv")

# Data cleaning - change variables into categorical variables 
data$DEPENDENTS <- as.factor(data$DEPENDENTS)
data$CRED_HERE <- as.factor(data$CRED_HERE)
data$INSTALLMENTRATE <- as.factor(data$INSTALLMENTRATE)
data$ATADDRESSSINCE <- as.factor(data$ATADDRESSSINCE)

# Scale continous variables 
data$AGE <- scale(data$AGE)
data$DURATION <- scale(data$DURATION)

# Sample split into test train set 
indexes <- sample(1:nrow(data), size = 0.2*nrow(data))
test <- data[indexes, ]
train <- data[-indexes, ]

# Baseline of only guessing yes 
(sum(data$CRED_APPROVED == 'YES')/ length(data$CRED_APPROVED))
table(data$CRED_APPROVED)/nrow(data)
# Data is lopsided so we need to beat 70% accuracy if guessing YES every time

# Matrix to keep track of model prediction accuracy
acc = rep(0,6)

###################################
#                                 #
#   Part 2: Logistic Regression   #
#                                 #
###################################


# Start with a logistic regression model with all parameters with all data 
log_full_alldata <- glm(CRED_APPROVED~. , family = "binomial", data = data)

# cv.glm computes cross validation error rate 
cv_est <- cv.glm(data, log_full_alldata, K=10)$delta[1]
(log_full_alldata_cv <- 1-cv_est)

# Fit logisitic regression on training set 
log_full_train <- glm(CRED_APPROVED~. , family = "binomial", data=train)

# Predict ratios on test set 
predict_log_full_test <- predict(log_full_train, newdata=test, type = "response")

# Loop through different probability thresholds to find most accurate
accuracy_log_full <- rep(0, 9)
for (i in 1:9){
  res_log_full  <- ifelse(predict_log_full_test > i/10,'YES','NO')
  # Compare with the original results 
  misClasificError <- mean(res_log_full != test$CRED_APPROVED)
  accuracy_log_full[i] = 1-misClasificError
}

# deterimine which threshold was best
best_log_full = which(accuracy_log_full == max(accuracy_log_full))
best = max(best_log_full)

# Confusion matrix for best model
print(paste('Accuracy - ',accuracy_log_full[best]))
table(test$CRED_APPROVED, predict_log_full_test>best/10)
acc[1] = accuracy_log_full[best] # store for later

all_names <- names(log_full_train$coefficients)

#########################################
#                                       #
#   Part 3: Subset Selection Methods    #
#                                       #
#########################################

# 3.A: Forwards Stepwise Selection
#########################################

# Logisitic regression with no variables 
log_null_train <- glm(CRED_APPROVED~1, family = "binomial", data = train)

# Forwards stepwise selection
forward_log <- step(log_null_train, scope = list(lower = log_null_train, upper = log_full_train), direction = "forward", trace = 0)

# Fit to test data - using same threshold as previous model since they are logistic regression
predict_forward_log <- predict(forward_log, newdata=test, type = "response")
result_forward_log  <- ifelse(predict_forward_log > best/10,'YES','NO')

misClasificError <- mean(result_forward_log != test$CRED_APPROVED)
acc[2] = 1-misClasificError

# Confusion matrix 
table(test$CRED_APPROVED, predict_forward_log>best/10)
forward_names<-names(forward_log$coefficients)

# 3.B: Backwards Stepwise Selection
#########################################

# Full model - log_full_train
backward_log <- step(log_full_train, direction = "backward", trace = 0)

predict_backward_log <- predict(backward_log, newdata=test, type = "response")
result_backward_log  <- ifelse(predict_backward_log > best/10,'YES','NO')
misClasificError <- mean(result_backward_log != test$CRED_APPROVED)
acc[3] = 1-misClasificError

# Confusion matrix 
table(test$CRED_APPROVED, predict_backward_log>best/10)
back_names<-names(backward_log$coefficients)

# Comparing Models
name_comp<-matrix(nrow=length(all_names),ncol=3)
for(i in 1:length(all_names)){
  nam = all_names[i]
  name_comp[i,1] = nam
  name_comp[i,2] = nam %in% back_names
  name_comp[i,3] = nam %in% forward_names
}

# They are all the same so we will only consider one of them in the final section

# 3.C: Ridge Selection
########################################

# Split train data into independent and dependent variables 

ridge_x = model.matrix(CRED_APPROVED~., family = "binomial", data = train)
ridge_y = ifelse(train$CRED_APPROVED == 'YES',1,0)

# Use cross validation with to determine the lambda parameter for ridge regression 
lambdas <- 10^seq(4,-5, length = 100)
cv.ridge = cv.glmnet(ridge_x, ridge_y, family = "binomial", lambda = lambdas, alpha = 0, standardize = FALSE)

# Plot lambda vs. cross validation error mean
ridge_lambdas <- cv.ridge$lambda
ridge_cv_means <- cv.ridge$cvm
plot(ridge_lambdas, ridge_cv_means, main = "ridge Lambda vs Cross Validation Error mean")

# should the lambda with the lowest cross validation error rate 
bestlam = cv.ridge$lambda.min

# look at coefficients which are not 0 of ridge regression 
ridge_log_train <- glmnet(ridge_x, ridge_y, family = "binomial", alpha = 0, standardize = FALSE, lambda = lambdas)
ridge_log_coef <- predict(ridge_log_train, type = "coefficients", s= bestlam)[1:(ncol(ridge_x)) ,]

ridge_log_coef[abs(ridge_log_coef) > 0.001]

length(ridge_log_coef[abs(ridge_log_coef) > 0.1])

ridge_log_test <- model.matrix(CRED_APPROVED~., family = "binomial", data=test)

predict_ridge_log <- predict(ridge_log_train, newx= ridge_log_test, type = "response", s= bestlam)
result_ridge_log  <- ifelse(predict_ridge_log > best/10,'YES','NO')

# Compare with the original results 
misClasificError <- mean(result_ridge_log != test$CRED_APPROVED)
acc[4] = 1-misClasificError

# Confusion matrix 
table(test$CRED_APPROVED, predict_ridge_log>best/10)

# 3.D: Lasso Selection
#######################################

# Split train data into independent and dependent variables 
lasso_x = model.matrix(CRED_APPROVED~., data = train)
lasso_y = ifelse(train$CRED_APPROVED== 'YES',1,0)

# Use cross validation with to determine the lambda parameter for lasso regression 
lambdas <- 10^seq(3,-5, length = 100)
cv.lasso = cv.glmnet(lasso_x, lasso_y, family = "binomial", lambda = lambdas, alpha = 1, standardize = FALSE)

# Plot lambda vs. cross validation error mean
lasso_lambdas <- cv.lasso$lambda
lasso_cv_means <- cv.lasso$cvm
plot(lasso_lambdas, lasso_cv_means, main = "Lasso Lambda vs Cross Validation Error mean")

# should the lambda with the lowest cross validation error rate 
(bestlam = cv.lasso$lambda.min)

# look at coefficients which are not 0 of lasso regression 
lasso_log_train <- glmnet(lasso_x, lasso_y, family = "binomial", alpha = 1, standardize = FALSE, lambda = lasso_lambdas)
lasso_log_coef <- predict(lasso_log_train, type = "coefficients", s= bestlam)[1:(ncol(lasso_x)) ,]

lasso_log_coef[lasso_log_coef!=0]
length(lasso_log_coef[lasso_log_coef!=0])


lasso_log_test <- model.matrix(CRED_APPROVED~., family = "binomial", data=test)

predict_lasso_log <- predict(lasso_log_train, newx= lasso_log_test, type = "response", s= bestlam)
result_lasso_log  <- ifelse(predict_lasso_log > best/10,'YES','NO')
# Compare with the original results 
misClasificError <- mean(result_lasso_log != test$CRED_APPROVED)
acc[5]=1-misClasificError

# Confusion matrix 
table(test$CRED_APPROVED, predict_lasso_log>0.5)

# 3.E: Random Forest Selection
#########################################

# Space for RF selection technique

###########################
#                         #
#   Part 4: Final Model   #
#                         #
###########################

