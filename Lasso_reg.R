# Lasso attempt 

setwd("C:/Users/board/Desktop/Kaggle/mf850_extra")
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

library(glmnet) # glmnet for ridge/ lasso regression 

# Split train data into independent and dependent variables 
# model matrix splits data into dummy variables 
x = model.matrix(CRED_APPROVED~., data = train)
y = ifelse(train$CRED_APPROVED== 'YES',1,0)

# Use cross validation with to determine the lambda parameter for lasso regression 
lambdas <- 10^seq(3,-5, length = 100)
cv.lasso = cv.glmnet(x, y, family = "binomial", lambda = lambdas, alpha = 1, standardize = FALSE)

# Plot lambda vs. cross validation error mean
lasso_lambdas <- cv.lasso$lambda
lasso_cv_means <- cv.lasso$cvm
plot(lasso_lambdas, lasso_cv_means, main = "Lasso Lambda vs Cross Validation Error mean")

# should the lambda with the lowest cross validation error rate 
(bestlam = cv.lasso$lambda.min)

# look at coefficients which are not 0 of lasso regression 
lasso1 <- glmnet(x, y, family = "binomial", alpha = 1, standardize = FALSE, lambda = lasso_lambdas)
lasso.coef <- predict(lasso1, type = "coefficients", s= bestlam)[1:(ncol(x)) ,]
lasso.coef

lasso.coef[lasso.coef!=0]
length(lasso.coef[lasso.coef!=0])


predict_lasso <- predict(lasso1, newx= test1, type = "response", s= bestlam)
hist(predict_lasso, breaks= 20)
predict_lasso  <- ifelse(predict_lasso > 0.5,'YES','NO')
# Compare with the original results 
misClasificError <- mean(predict_lasso != test$CRED_APPROVED)
print(paste('Accuracy',1-misClasificError))


# Split categorical variables into seperate columns 
x = model.matrix(CRED_APPROVED~., data = train)
# Re structure data so the data so it is a data frame and not a matrix 
data3 <- data.frame(x)
# Save names of the coefficients in the backward step wise regression 
coef_names <- names(lasso.coef)
# Delete the first name which is X intercept 
coef_names <- coef_names[c(-1,-2)]
# Make sure all the variables are in the data set 
data_col_names <- colnames(data3)
sum(coef_names%in% data_col_names) == (length(coef_names))
# Get the columns from the new data set 
test_rf <- data3[,coef_names]

# new_log <- glm(CRED_APPROVED~., family = "binomial", data = data_rf)

