# Install "rpart" package for recursive partitioning in order to CART modeling 

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

# load the library
library(rpart)

# Sample split into test train set 
indexes <- sample(1:nrow(data), size = 0.2*nrow(data))
test <- data[indexes, ]
train <- data[-indexes, ]

data.tree = rpart(CRED_APPROVED~., data = train, cp = 10^(-6))

#To see entries in a list
names(data.tree)

#Information on the size of the tree
#data.tree$cptable[1:10, ] WHEN I TRIED THIS, IT GIVES ME AN ERROR: SUBSCRIPT OUT OF BOUNDS

data.tree$cptable[dim(data.tree$cptable)[1] - 9:0, ] #I'm not sure how to assess the size of the tree, let me know.

#In the event that the size is huge (probably is) pruning will be required (example: prune tree to 10 splits)
cp10 = which(data.tree$cptable[, 2] == 10)
data.tree10 = prune(data.tree, data.tree$cptable[cp10, 1])

print(data.tree10) #I do not understand how to visualize this but I will check it in tree form

summary(data.tree10)

#Visualizaton time!
png("data.tree10.png", width = 1200, height = 800)
post(data.tree10, file = "", title. = "Classifying Credit Line Size, 10 splits", bp = 18)
dev.off() 

# Predict using the test data 

# Generate predictions on the test data using the pruned tree
predict_cart <- predict(data.tree10, newdata = test)
hist(predict_cart)
abline(v = 0.5, col = "red")
# Rename predictions to match test file 
results_cart  <- ifelse(predict_cart > 0.5,'YES','NO')
# Compare with the original results 
misClasificError <- mean(results_cart != test$CRED_APPROVED)
print(paste('Accuracy',1-misClasificError))
# Compare with Baseline 
table(test$CRED_APPROVED)/nrow(test)

