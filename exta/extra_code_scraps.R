# Extra 850 code 


###################### FROM LASSO REGRESSION ##########

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


###################################################
############# END LASSO EXTRA CODE #################
########################################################

############ BACKWARDS EXTRA CODE ####################################


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
# Add dependent (y) variable to data frame (easier to load into random forest function )
y = ifelse(train$CRED_APPROVED == 'YES',1,0)
data1 <- cbind(data_rf, y)

# Random forest with that data 
library(randomForest)
fit_rf2 <- randomForest(as.factor(y)~. , data = data1)

fit_rf2
importance(fit_rf2)

# Create data frame to store ntree solving results 
ntree_df <- data.frame(matrix(0, nrow= 50, ncol = 2))
names(ntree_df)[1:2] <-c("Accuracy", "N Tree")

for (ntree_times in 1:10){
  ntree <- 500+ntree_times*10
  index_h <- 2 * ntree_times 
  fit_temp <- rfcv(data_rf, y , ntree = ntree)
  error_sum <- mean(fit_temp$error.cv)
  ntree_df[ntree_times, 1] <- 1-error_sum # accuracy 
  ntree_df[ntree_times,2] <- ntree
  
}

plot(ntree_df[ntree_df!=0,])

############################################################
################# END BACKWARDS EXTRA CODE################
############################################################

