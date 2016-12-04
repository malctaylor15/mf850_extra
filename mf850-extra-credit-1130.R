# Remember to change the working directory to the appropriate place on your computer- where ever the data is located 
# on the top line (where file, edit ...) - go to session -> set working directory -> choose directory 
# Then change this line with the appropriate one 

#setwd("C:/Users/board/Desktop/Kaggle/mf850_extra")
data <- read.csv("mf850-loan-data.csv")

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

# Baseline of only guessing yes 
(sum(data$CRED_APPROVED == 'YES')/ length(data$CRED_APPROVED))
table(data$CRED_APPROVED)/nrow(data)

# Fit logisitic regression on test set 
fit2 <- glm(CRED_APPROVED~. , family = "binomial", data=train)

# Predict ratios on test set 
predict_fit2 <- predict(fit2, newdata=test, type = "response")
hist(predict_fit2, breaks= 20)

# Use 50% threshold for predictions ( we can try different thresholds later )
results2  <- ifelse(predict_fit2 > 0.5,'YES','NO')
# Accuracy on test data 
misClasificError <- mean(results2 != test$CRED_APPROVED)
print(paste('Accuracy',1-misClasificError))
# Compare with Baseline
table(test$CRED_APPROVED)/nrow(test)


