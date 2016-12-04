# Stepwise Variable selection 


setwd("C:/Users/Evan/Downloads")
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

#summary(backwards)
#length(backwards$coefficients)

# Great only 35 variables compared to 56 
# Let's see how it does on the prediction set 
backwards <- step(fullmod, direction = "backward", trace = 0)
predict_fit2 <- predict(backwards, newdata=test, type = "response")
hist(predict_fit2, breaks = 20)
results2  <- ifelse(predict_fit2 > 0.4,'YES','NO')
misClasificError <- mean(results2 != test$CRED_APPROVED)
print(paste('Accuracy',1-misClasificError))

# Confusion matrix 
table(test$CRED_APPROVED, predict_fit2>0.4)
back_names<-names(backwards$coefficients)

# Try forwards

forwards <- step(nothing, scope = list(lower = nothing, upper = fullmod), direction = "forward", trace = 0)
predict_fit2 <- predict(forwards, newdata=test, type = "response")
hist(predict_fit2, breaks = 20)
results2  <- ifelse(predict_fit2 > 0.4,'YES','NO')
misClasificError <- mean(results2 != test$CRED_APPROVED)
print(paste('Accuracy',1-misClasificError))

# Confusion matrix 
table(test$CRED_APPROVED, predict_fit2>0.4)
for_names<-names(forwards$coefficients)

# try both
bothways <- step(nothing, scope = list(upper=fullmod), direction = "both", trace = 0)
predict_fit2 <- predict(bothways, newdata=test, type = "response")
hist(predict_fit2, breaks = 20)
results2  <- ifelse(predict_fit2 > 0.4,'YES','NO')
misClasificError <- mean(results2 != test$CRED_APPROVED)
print(paste('Accuracy',1-misClasificError))

# Confusion matrix 
table(test$CRED_APPROVED, predict_fit2>0.4)
both_names<-names(bothways$coefficients)

all_names<-names(fullmod$coefficients)

# Comparing Models
name_comp<-matrix(nrow=length(all_names),ncol=4)
for(i in 1:length(all_names)){
  nam = all_names[i]
  name_comp[i,1] = nam
  name_comp[i,2] = nam %in% back_names
  name_comp[i,3] = nam %in% for_names
  name_comp[i,4] = nam %in% both_names
}

# They are all the same, which is good i guess