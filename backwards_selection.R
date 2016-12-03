# Stepwise Variable selection 


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

# Try backwards stepwise regression with logistic regression 
# Inspiration 
# http://www.utstat.toronto.edu/~brunner/oldclass/appliedf11/handouts/2101f11StepwiseLogisticR.pdf 



# Logistic regression with all variables 
fullmod <- glm(CRED_APPROVED~. , family = "binomial", data =train)
summary(fullmod)

# Logisitic regression with no variables 
nothing <- glm(CRED_APPROVED~1, family = "binomial", data = train)
summary(nothing) # Essentially intercept says yes (it is greater than 0.5) 

# Backward stepwise regression 
backwards <- step(fullmod, direction = "backward", trace = 0)
summary(backwards)
length(backwards$coefficients)

# Great only 35 variables compared to 56 
# Let's see how it does on the prediction set 

predict_fit2 <- predict(backwards, newdata=test, type = "response")
hist(predict_fit2, breaks = 20)
# Use 50% threshold for predictions ( we can try different thresholds later )
results2  <- ifelse(predict_fit2 > 0.5,'YES','NO')
# Compare with the original results 
misClasificError <- mean(results2 != test$CRED_APPROVED)
print(paste('Accuracy',1-misClasificError))

# Get the variables from backward selection 

# Split categorical variables into seperate columns 
x = model.matrix(CRED_APPROVED~., data = train)
# Re structure data so the data so it is a data frame and not a matrix 
data3 <- data.frame(x)
# Save names of the coefficients in the backward step wise regression 
coef_names <- names(backwards$coefficients)
# Delete the first name which is X intercept 
coef_names <- coef_names[-1]
# Make sure all the variables are in the data set 
data_col_names <- colnames(data3)
sum(coef_names%in% data_col_names) == (length(coef_names))
# Get the columns from the new data set 
data_rf <- data3[,coef_names]
data1 <- cbind(data_rf, y)

# Random forest with that data 
library(randomForest)
fit_rf2 <- randomForest(as.factor(y)~. , data = data1)

fit_rf2
importance(fit_rf2)

# Create data frame to store ntree solving results 
ntree_df <- data.frame(matrix(0, nrow= 50, ncol = 4))

# 
# for (ntree_times in 1:15){
#   ntree <- 500+ntree_times*10
#   n_tree_times <- 
#   fit_temp <- randomForest(as.factor(y)~., data= data1, ntree = ntree)
#   ntree_df[c(ntree_times,(n_tree_times-1)),] <- fit_temp[5]
#   ntree_df[ntree_times, 4] <- ntree
#   
# }
