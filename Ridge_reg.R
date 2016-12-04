# Ridge 

# Ridge attempt 

#setwd("C:/Users/board/Desktop/Kaggle/mf850_extra")
data <- read.csv("mf850-loan-data.csv")
set.seed(1)
# Data cleaning - change variables into categorical variables 
data$DEPENDENTS <- as.factor(data$DEPENDENTS)
data$CRED_HERE <- as.factor(data$CRED_HERE)
data$INSTALLMENTRATE <- as.factor(data$INSTALLMENTRATE)
data$ATADDRESSSINCE <- as.factor(data$ATADDRESSSINCE)

# Scale continous variables 
data$AGE <- scale(data$AGE)
data$DURATION <- scale(data$DURATION)


# Splitting the data into test/ train sets  
# https://ragrawal.wordpress.com/2012/01/14/dividing-data-into-training-and-testing-dataset-in-r/

# Sample split into test train set 
indexes <- sample(1:nrow(data), size = 0.2*nrow(data))
test <- data[indexes, ]
train <- data[-indexes, ]

library(glmnet) # glmnet for ridge/ ridge regression 

# Split train data into independent and dependent variables 
# model matrix splits data into dummy variables 
x = model.matrix(CRED_APPROVED~., family = "binomial", data = train)
y = ifelse(train$CRED_APPROVED == 'YES',1,0)

# Use cross validation with to determine the lambda parameter for ridge regression 
lambdas <- 10^seq(4,-5, length = 100)
cv.ridge = cv.glmnet(x, y, family = "binomial", lambda = lambdas, alpha = 0, standardize = FALSE)

# Plot lambda vs. cross validation error mean
ridge_lambdas <- cv.ridge$lambda
ridge_cv_means <- cv.ridge$cvm
plot(ridge_lambdas, ridge_cv_means, main = "ridge Lambda vs Cross Validation Error mean")

# should the lambda with the lowest cross validation error rate 
bestlam = cv.ridge$lambda.min

# look at coefficients which are not very small for the ridge regression 
ridge1 <- glmnet(x, y, family = "binomial", alpha = 0, standardize = FALSE, lambda = lambdas)
ridge.coef <- predict(ridge1, type = "coefficients", s= bestlam)[1:(ncol(x)) ,]
ridge.coef

ridge.coef[abs(ridge.coef) > 0.001]

length(ridge.coef[abs(ridge.coef) > 0.1])


# Test on test set 

# Prepare the test set data 
test1 <- model.matrix(CRED_APPROVED~., family = "binomial", data=test)
# Make predictions using ridge model on test data 
predict_ridge <- predict(ridge1, newx= test1, type = "response", s= bestlam)
# Look at predictions 
hist(predict_ridge, breaks= 20)
# Re format results so they will be comparable 
predict_ridge_yn <- ifelse(predict_ridge > 0.5,'YES','NO')
# Compare with the original results 
misClasificError <- mean(predict_ridge_yn != test$CRED_APPROVED)
print(paste('Accuracy',1-misClasificError))
# Compare with Baseline 
table(test$CRED_APPROVED)/nrow(test)

# Quick error analysis 

# Probabilities where the model predicted correctly 
predict_ridge_correct <- predict_ridge[predict_ridge_yn == test$CRED_APPROVED]
hist(predict_ridge_correct)
# Probabilities where the model predicted incorrectly 
predict_ridge_incorrect <- predict_ridge[predict_ridge_yn != test$CRED_APPROVED]
hist(predict_ridge_incorrect)

