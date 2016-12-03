setwd("C:/Users/board/Desktop/Kaggle/mf850_extra")

data <- read.csv("mf850-loan-data.csv")

head(data)
summary(data)

# Take a look at the

# Splitting the data using technique saw here 
# https://ragrawal.wordpress.com/2012/01/14/dividing-data-into-training-and-testing-dataset-in-r/

# Sample split into test train set 
indexes <- sample(1:nrow(data), size = 0.2*nrow(data))
test <- data[indexes, ]
train <- data[-indexes, ]

# Baseline of only guessing yes 
(sum(data$CRED_APPROVED == 'YES')/ length(data$CRED_APPROVED))

fit1 <- glm(CRED_APPROVED~. , family = "binomial", data = data)
summary(fit1)

library(boot) # cv.glm
# cv.glm computes cross validation error rate 
cv_est <- cv.glm(data, fit1, K=10)$delta[1]
(fit1_cv <- 1-cv_est)


fit2 <- glm(CRED_APPROVED~. , family = "binomial", data=test)
predict_fit2 <- predict(fit2, newdata=test, type = "response")
hist(predict_fit2, breaks= 20)
results2  <- ifelse(predict_fit2 > 0.5,'YES','NO')
misClasificError <- mean(results2 != test$CRED_APPROVED)
print(paste('Accuracy',1-misClasificError))

